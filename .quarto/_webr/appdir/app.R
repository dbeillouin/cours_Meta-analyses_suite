# Load necessary libraries
library(shiny)
library(metafor)
library(ggplot2)
library(truncnorm)
library(gridExtra)
library(dplyr)
library(magrittr)

# Define UI
ui <- fluidPage(
  titlePanel("Forest Plot with Three-Level Meta-Analytical Model"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("sd_true", "Standard Deviation of True Effect Size:",
                  min = 0, max = 1, value = 0, step = 0.01,),
      sliderInput("sd_sampling", "Standard Deviation of Sampling Error:",
                  min = 0, max = 1, value = 0, step = 0.01),
      sliderInput("sd_group", "Standard Deviation of Group Effect:",
                  min = 0, max = 1, value = 0, step = 0.01),  # New slider for group effect variance
      sliderInput("n_studies", "Number of Studies:",
                  min = 3, max = 15, value = 3, step = 1),
      sliderInput("n_groups", "Number of Groups:",
                  min = 2, max = 5, value = 2, step = 1),
      checkboxInput("show_mean", "Show mean without sample error", value = TRUE)
    ),
    
    mainPanel(
      plotOutput("forestPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$forestPlot <- renderPlot({
    # Set parameters
    set.seed(113)  # For reproducibility
    true_effect_size <- 1.5  # Specify a true effect size (e.g., ratio)
    sd_true <- input$sd_true
    sd_sampling <- input$sd_sampling
    sd_group <- input$sd_group  # Standard deviation for group effects
    n_studies <- input$n_studies
    n_groups <- input$n_groups
    
    # Assign each study to a group
    group_assignments <- rep(1:n_groups, length.out = n_studies)
    
    # Generate studies data
    studies <- data.frame(
      study = paste("Study", 1:n_studies),
      n = sample(20:100, n_studies),  # Random sample sizes for each study
      group = factor(group_assignments)
    ) %>% arrange(group) %>% mutate(study = paste("Study", 1:n_studies))

    # Generate random group effects and add to the true effect sizes
       tmp1<- rnorm(n_groups - 1, mean = 0, sd = sd_group)
    group_effects <- c(tmp1,-sum(tmp1))
    studies$group_effect <- group_effects[studies$group]
    
    # Generate true effect sizes with associated group and sampling error
    studies$group_effect<- studies$group_effect+  true_effect_size
    tmp<- rnorm(n_studies - 1, mean = 0, sd = sd_true)
    studies$mean_1 <- studies$group_effect+ c(tmp,-sum(tmp))
    
    studies$mean_ratio <- studies$mean_1 + rnorm(n_studies, mean = 0, sd = sd_sampling)
    
    # Generate standard errors from a truncated normal distribution
    studies$se <-0.01+sd_sampling* rtruncnorm(n_studies, a = 0, mean = 0.1, sd = 0.1)
    
    # Calculate confidence intervals
    studies$ci_lower <- studies$mean_ratio - 1.96 * studies$se
    studies$ci_upper <- studies$mean_ratio + 1.96 * studies$se
    studies$color <- 2
    
    # Calculate overall effect size using a three-level model
    random_effect <- rma.mv(yi = mean_ratio, V = se^2, random = ~ 1 | group/study, data = studies)
    
    # Create summary table for the overall effect size
    summary_table <- data.frame(
      study = "Overall_random_effect",
      n = NA,
      group = NA,
      mean_1 = NA,
      group_effect = NA,
      mean_ratio = random_effect$b,
      se = NA,
      ci_lower = random_effect$ci.lb,
      ci_upper = random_effect$ci.ub,
      color = 1
    )
    
    # Combine study data with overall effect size
    plot_data <- rbind(studies, summary_table)
    

  #  Create forest plot
    p1 <- ggplot(plot_data, aes(x = reorder(study, rev(group)))) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 1) +
      geom_point(aes(y = mean_ratio, color = factor(group))) +
   {if (input$show_mean) geom_point(aes(y = mean_1), color = "red", size = 4, pch = 8)} +
     {if (input$show_mean) geom_point(aes(y = group_effect), color = "orange", size = 4, pch = 9)} +
      geom_hline(yintercept = true_effect_size, linetype = "dashed", color = "blue") +
      labs(title = "Forest Plot: Three-Level Meta-Analysis Model",
           x = "Studies",
           y = "Effect Size (Ratio)") +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip() +
      geom_text(aes(x = reorder(study, rev(study)), y = 0.5, label = paste0("Group = ", group, "; n = ", n))) +
      scale_size_continuous(range = c(2, 5)) +
      geom_hline(yintercept = true_effect_size, linetype = "dotted", color = "blue")+
             geom_rect(aes(xmin = -Inf, xmax = Inf,
                     ymin = true_effect_size - 1.96 * sd_true,
                     ymax = true_effect_size + 1.96 * sd_true), 
                 fill = "blue", alpha = 0.05)

    # Display the forest plot
    p1
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
