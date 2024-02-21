#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(shiny)

ui <- fluidPage(
  titlePanel('Compute Probabilties From Different Distributions'),
  sidebarLayout(
    sidebarPanel(width = 4, 
                 #input select
                 selectInput(inputId = "probtype",
                             label = "Select probability",
                             choices = c("P(X = k)", "P(X <= k)"),
                             selected = "P(X = k)"),
                 
                 numericInput(inputId = k,
                              label = "k",
                              value = 1),
                 
                 selectInput(inputId = "rv",
                             label = "Select Random Variable",
                             choices = c("Binomial", "Poisson", "Uniform", "Exponential", "Normal"),
                             selected = "Binomial"),
                 #Binomial dist
                 conditionalPanel(condition = "input.rv == 'Binomial'",
                                  h5("Binomial Parameters"),
                                  numericInput(inputId = "ntrials",
                                              label = "Number of Trials:",
                                              min = 0,
                                              max = 1000,
                                              value = 1),
                                  numericInput(inputId = "probsuccess",
                                              label = "Probability of Success:",
                                              min = 0,
                                              max = 1,
                                              value = 0.5)),
                 #Poisson dist
                 conditionalPanel(condition = "input.rv == 'Poisson'",
                                  h5("Poisson Parameters"),
                                  numericInput(inputId = "rate1",
                                              label = "Rate:",
                                              min = 1,
                                              max = 10,
                                              value = 1)),
                 #Uniform dist
                 conditionalPanel(condition = "input.rv == 'Uniform'",
                                  h5("Uniform Parameters"),
                                  numericInput(inputId = "min",
                                              label = "Minimum Value:",
                                              min = 0,
                                              max = 100,
                                              value = 0),
                                  numericInput(inputId = "max",
                                              label = "Maximum Value:",
                                              min = 0,
                                              max = 100,
                                              value = 1)),
                 #Exponential dist
                 conditionalPanel(condition = "input.rv == 'Exponential'",
                                  h5("Exponential Parameters"),
                                  numericInput(inputId = "rate2",
                                              label = "Rate Parameter:",
                                              min = 0,
                                              max = 100,
                                              value = 1)),
                 #Normal dist
                 conditionalPanel(condition = "input.rv == 'Normal'",
                                  h5("Normal Parameters"),
                                  numericInput(inputId = "mean",
                                              label = "Mean:",
                                              min = 0,
                                              max = 100,
                                              value = 0),
                                  numericInput(inputId = "sd",
                                              label = "Standard Deviation:",
                                              min = 0.1,
                                              max = 100,
                                              value = 1)),
                 
                 actionButton("Compute","Compute Result")),
    mainPanel(plotOutput("distPlot"))
              #textOutput(outputId = "keystoke_class"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$Compute,  output$distPlot <- renderPlot({
      set.seed(111)
      browser()
      if(input$rv == 'Binomial'){
        df = data.frame(success = 0:input$ntrials, 
                        prob = dbinom(x = 0:input$ntrials, size = input$ntrials, prob = input$probsuccess))
        if(input$probtype == 'P(X = k)'){
          
          df %>% mutate(condition = ifelse(success = input$k, as.character(input$k), "failure")) 
          
        }else{
           df %>% mutate(Heads = ifelse(success <= input$k, as.character(input$k), "failure")) 
        }
        
        ggplot(data = df, aes(x = factor(condition), y = prob, fill = condition)) +
          geom_col() +
          geom_text(aes(label = round(prob,2), y = prob + 0.01),
                    position = position_dodge(0.9),
                    size = 3,
                    vjust = 0)+
          labs(title = paste0("Probability of X =", input$k, " successes."),
               subtitle = paste0("binom(n = ", input$ntials, "p = ", input$probsuccess),
               x = "Successes (x)",
               y = "probability")+
          theme_classic2()
      }
      # }else if(input$model == 'Poisson'){
      #   d = rpois(n=input$ss, lambda = input$rate1)
      # }else if(input$model == 'Uniform'){
      #   d = runif(n=input$ss, min = input$min, max = input$max)
      # }else if(input$model == 'Exponential'){
      #   d = rexp(n=input$ss, rate = input$rate2)
      # }else if(input$model == 'Normal'){
      #   d = rnorm(n=input$ss, mean = input$mean, sd = input$sd)
      # }
    }))
    
    # output$keystoke_class = renderText({
    #   
    #   
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
