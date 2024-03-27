#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source('C:/Users/Bruin/Documents/GitHub/UI_STAT251-03_Spring_2024/docs/misc/stat251_tools.R')
library(shiny)
library(ggplot2)
library(ggthemes)
library(latex2exp)
# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel('One Sample Hypothesis Tests'),
  sidebarLayout(
    sidebarPanel(width = 4, 
                 #input select
                 selectInput(inputId = "param",
                             label = "Population Parameter",
                             choices = c("mean", "proportion"),
                             selected = 'mean'),
                 
                 selectInput(inputId = "test",
                             label = "Choose inference type:",
                             choices = c("confidence.interval","upper.tail", 
                                         "lower.tail", 'two.tailed'),
                             selected = 'upper.tail'),
                 
                 sliderInput(inputId = "alpha",
                             label = "Significance Level:",
                             min = 0,
                             max = 1,
                             step = 0.01,
                             value = 0.05),
                 
                 numericInput(inputId = "n",
                              label = "Sample Size: ",
                              value = 100),
                 
                 conditionalPanel(condition = "input.param == 'proportion'",
                                  numericInput(inputId = "p0",
                                              label = "Set null value H_0: p0 = ",
                                              value = 0.5,
                                              min = 0,
                                              max = 1,
                                              step = 0.01),
                                  numericInput(inputId = "obs.prop",
                                               label = "Observed proportion: ",
                                               min = 0,
                                               max = 1,
                                               step = 0.01,
                                               value = 0.1)),
                                  
                 conditionalPanel(condition = "input.param == 'mean'",
                                  numericInput(inputId = "m0",
                                               label = "Set null value H_0: m0 = ",
                                               value = 0),
                                  numericInput(inputId = "obs.mean",
                                               label = "Observed Sample Mean: ",
                                               value = 5),
                                  numericInput(inputId = "obs.s",
                                               label = "Observed Sample Standard Dev.: ",
                                               value = 1)),
                 
                 actionButton("run","Run Inference"),
    ),
    mainPanel(plotOutput("plotresult"), 
              verbatimTextOutput("summary"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$run,  output$plotresult <- renderPlot({
      if(input$param == 'mean'){
        distrib = 't'
        estimate = input$obs.mean
        standard.score = qt(1-input$alpha/2, input$n - 1)
        SE = (input$obs.s/sqrt(input$n))
        test.stat = (input$obs.mean - input$m0)/SE
        LV = estimate - 4*SE
        UV = estimate + 4*SE
        LB = estimate - standard.score*SE
        UB = estimate + standard.score*SE
      }else{
        distrib = 'z'
        estimate = input$obs.prop
        standard.score = qnorm(1-input$alpha/2)
        SE = sqrt(input$obs.prop*(1-input$obs.prop)/sqrt(input$n))
        test.stat = (input$obs.prop - input$p0)/SE
        LB = max(estimate - standard.score*SE, 0)
        LV = max(estimate - 4*SE, 0)
        UB = min(estimate + standard.score*SE, 1)
        UV = min(estimate + 4*SE, 1)
      }
      
      if(input$test == 'confidence.interval'){
        out = ggplot()+xlim(LV, UV)+
          theme_bw()+
          stat_function(fun = dnorm,
                        args = list(mean = estimate, sd = SE),
                        geom = 'area',
                        fill = 'lightgrey')+
          stat_function(fun = dnorm,
                        args = list(mean = estimate, sd = SE),
                        size = 2)+
          geom_vline(xintercept = LB, linetype = 'dotted', size = 2)+
          geom_vline(xintercept = UB, linetype = 'dotted', size = 2)+
          geom_text(aes(x = estimate, y = dnorm(estimate, mean = estimate, sd = SE)/2),
                     label = paste0(100*(1-input$alpha), '%', ' CI'), size = 8)+
          xlab('Estimate')+
          ylab('Probability Density')+
          theme(axis.text = element_text(size = 12),
                axis.title = element_text(size = 14))
          
      }else{
        out = gen.density.plot(n = input$n, 
                               dist = distrib,
                               obs = test.stat,
                               alpha = input$alpha,
                               test = input$test)
      }
      
      plot(out)
      
    }))
    
    observeEvent(input$run,  output$summary <- renderPrint({
      if(input$test == 'confidence.interval'){
        if(input$param == 'mean'){
          one.sample.t.CI(xbar = input$obs.mean,
                          s = input$obs.s,
                          n = input$n,
                          alpha = input$alpha,
                          verbose = T)
          
        }else{
          one.sample.prop.CI(phat = input$obs.prop,
                             n = input$n,
                             alpha = input$alpha,
                             verbose = T)
        }
      }else{
        if(input$param == 'mean'){
          one.sample.t.test(m0 = input$m0, 
                            xbar = input$obs.mean,
                            s = input$obs.s,
                            n = input$n,
                            alpha = input$alpha,
                            test = input$test)
          
        }else{
          one.sample.prop.test(p0 = input$p0,
                               phat = input$obs.prop,
                               n = input$n,
                               alpha = input$alpha,
                               test = input$test)
        }
      }
      
    }))
}

# Run the application 
shinyApp(ui = ui, server = server)
