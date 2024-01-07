#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel('Histogram Sandbox'),
  sidebarLayout(
    sidebarPanel(width = 4, 
      #input select
      selectInput(inputId = "model",
      label = "Select Model",
      choices = c("Bernoulli", "Binomial", "Poisson", "Uniform", "Exponential", "Normal"),
      selected = "Normal"),
      #Bernoulli dist
      conditionalPanel(condition = "input.model == 'Bernoulli'",
                       h5("Bernoulli Parameters"),
                       sliderInput(inputId = "prob1",
                                   label = "Probability of Success:",
                                   min = 0,
                                   max = 1,
                                   value = 0.5)),
      #Binomial dist
      conditionalPanel(condition = "input.model == 'Binomial'",
                       h5("Binomial Parameters"),
                       sliderInput(inputId = "ntrials",
                                   label = "Number of Trials:",
                                   min = 0,
                                   max = 100,
                                   value = 20),
                       sliderInput(inputId = "prob2",
                                   label = "Probability of Success:",
                                   min = 0,
                                   max = 1,
                                   value = 0.5)),
      #Poisson dist
      conditionalPanel(condition = "input.model == 'Poisson'",
                       h5("Poisson Parameters"),
                       sliderInput(inputId = "rate1",
                                   label = "Rate:",
                                   min = 1,
                                   max = 10,
                                   value = 1)),
      #Uniform dist
      conditionalPanel(condition = "input.model == 'Uniform'",
                       h5("Uniform Parameters"),
                       sliderInput(inputId = "min",
                                   label = "Minimum Value:",
                                   min = 0,
                                   max = 100,
                                   value = 0),
                       sliderInput(inputId = "max",
                                   label = "Maximum Value:",
                                   min = 0,
                                   max = 100,
                                   value = 1)),
      #Exponential dist
      conditionalPanel(condition = "input.model == 'Exponential'",
                       h5("Exponential Parameters"),
                       sliderInput(inputId = "rate2",
                                   label = "Rate Parameter:",
                                   min = 0,
                                   max = 100,
                                   value = 1)),
      #Normal dist  
      conditionalPanel(condition = "input.model == 'Normal'",
                       h5("Normal Parameters"),
                       sliderInput(inputId = "mean",
                                   label = "Mean:",
                                   min = 0,
                                   max = 100,
                                   value = 0),
                       sliderInput(inputId = "sd",
                                   label = "Standard Deviation:",
                                   min = 0.1,
                                   max = 100,
                                   value = 1)),
      sliderInput(inputId = "ss",
                  label = "Sample Size:", 
                  min = 10, 
                  max = 10000, 
                  value = 50),
    
    
    ),
      mainPanel(plotOutput("distPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  output$distPlot <- renderPlot({
    if(input$model == 'Bernoulli'){
      d = rbinom(n=input$ss, size = 1, p = input$prob1)
    }else if(input$model == 'Binomial'){
      d = rbinom(n=input$ss, size = input$ntrials, p = input$prob2)
    }else if(input$model == 'Poisson'){
      d = rpois(n=input$ss, lambda = input$rate1)
    }else if(input$model == 'Uniform'){
      d = runif(n=input$ss, min = input$min, max = input$max)
    }else if(input$model == 'Exponential'){
      d = rexp(n=input$ss, rate = input$rate2)
    }else if(input$model == 'Normal'){
      d = rnorm(n=input$ss, mean = input$mean, sd = input$sd)
    }
  
  
    hist(d,
         col='darkorchid',
         xlab="Sample",
         main="Histogram of Sample")},
    height=300)
}
# server <- function(input, output) {
#   output$distPlot <- renderPlot({
#     hist(rnorm(input$ss),col='darkorchid',xlab="Sample",main="Standard Normally Distributed Sample")},
#     height=300
#   )
# }

# Run the application 
shinyApp(ui = ui, server = server)
