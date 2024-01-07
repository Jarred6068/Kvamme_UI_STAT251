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
    sidebarPanel(width = 3, 
      #input select
      selectInput(inputId = "model",
      label = "Select Model",
      choices = c("Bernoulli", "Binomial", "Poisson", "Uniform", "Exponential", "Normal"),
      selected = "Normal"),
      #Bernoulli dist
      conditionalPanel(condition = "input.model == 'Bernoulli'",
                       h5("Bernoulli Parameters"),
                       sliderInput(inputId = "p",
                                   label = "p:",
                                   min = 0,
                                   max = 1,
                                   value = 0.5),
                       sliderInput(inputId = "samplesize",
                                   label = "Sample Size:", 
                                   min = 10, 
                                   max = 1000, 
                                   value = 50)),
      #Binomial dist
      conditionalPanel(condition = "input.model == 'Binomial'",
                       h5("Binomial Parameters"),
                       sliderInput(inputId = "n",
                                   label = "n:",
                                   min = 0,
                                   max = 100,
                                   value = 20),
                       sliderInput(inputId = "p",
                                   label = "p:",
                                   min = 0,
                                   max = 1,
                                   value = 0.5),
                       sliderInput(inputId = "samplesize",
                                   label = "Sample Size:", 
                                   min = 10, 
                                   max = 1000, 
                                   value = 50)),
      #Poisson dist
      conditionalPanel(condition = "input.model == 'Poisson'",
                       h5("Poisson Parameters"),
                       sliderInput(inputId = "rate",
                                   label = "rate:",
                                   min = 0,
                                   max = 10,
                                   value = 1),
                       sliderInput(inputId = "samplesize",
                                   label = "Sample Size:", 
                                   min = 10, 
                                   max = 1000, 
                                   value = 50)),
      #Uniform dist
      conditionalPanel(condition = "input.model == 'Uniform'",
                       h5("Uniform Parameters"),
                       sliderInput(inputId = "min",
                                   label = "min:",
                                   min = 0,
                                   max = 100,
                                   value = 0),
                       sliderInput(inputId = "max",
                                   label = "max:",
                                   min = 0,
                                   max = 100,
                                   value = 1),
                       sliderInput(inputId = "samplesize",
                                   label = "Sample Size:", 
                                   min = 10, 
                                   max = 1000, 
                                   value = 50)),
      #Exponential dist
      conditionalPanel(condition = "input.model == 'Exponential'",
                       h5("Exponential Parameters"),
                       sliderInput(inputId = "rate",
                                   label = "rate:",
                                   min = 0,
                                   max = 100,
                                   value = 1),
                       sliderInput(inputId = "samplesize",
                                   label = "Sample Size:", 
                                   min = 10, 
                                   max = 1000, 
                                   value = 50)),
      #Normal dist  
      conditionalPanel(condition = "input.model == 'Normal'",
                       h5("Normal Parameters"),
                       sliderInput(inputId = "mean",
                                   label = "mean:",
                                   min = 0,
                                   max = 100,
                                   value = 0),
                       sliderInput(inputId = "sd",
                                   label = "sd:",
                                   min = 0.1,
                                   max = 100,
                                   value = 1),
                      sliderInput(inputId = "samplesize",
                                  label = "Sample Size:", 
                                  min = 10, 
                                  max = 1000, 
                                  value = 50)),
    
    
    ),
      mainPanel(plotOutput("distPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  output$distPlot <- renderPlot({
    if(input$model == 'Bernoulli'){
      d = rbinom(n=input$samplesize, size = 1, p = input$p)
    }else if(input$model == 'Binomial'){
      d = rbinom(n=input$samplesize, size = input$n, p = input$p)
    }else if(input$model == 'Poisson'){
      d = rpois(n=input$samplesize, lambda = input$rate)
    }else if(input$model == 'Uniform'){
      d = runif(n=input$samplesize, min = input$min, max = input$max)
    }else if(input$model == 'Exponential'){
      d = rexp(n=input$samplesize, rate = input$rate)
    }else if(input$model == 'Normal'){
      d = rnorm(n=input$samplesize, mean = input$mean, sd = input$sd)
    }
  
  
    hist(d,
         col='darkorchid',
         xlab="Sample",
         main="Histogram of Sample")},
    height=300)
}
# server <- function(input, output) {
#   output$distPlot <- renderPlot({
#     hist(rnorm(input$samplesize),col='darkorchid',xlab="Sample",main="Standard Normally Distributed Sample")},
#     height=300
#   )
# }

# Run the application 
shinyApp(ui = ui, server = server)
