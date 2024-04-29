#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(ggplot2)
library(ggthemes)
library(latex2exp)
library(gridExtra)
library(ggpubr)



get.selected <- function(input, reactives){
  
  idx1 = which(colnames(reactives$mydata) == input$ResponseVar)
  idx2 = which(colnames(reactives$mydata) == input$explanatoryVars)
  Y = reactives$mydata[,idx1]
  X = reactives$mydata[,idx2]
  df = cbind.data.frame(X, Y)
  colnames(df) = c(input$ResponseVar, input$explanatoryVars)
  
  return(df)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel('Linear Regression and ANOVA'),
  sidebarLayout(
    sidebarPanel(width = 4, 
                 fileInput("myfileinput", 
                           "Please choose a csv File", 
                           multiple = FALSE, 
                           accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv")),
                 
                 selectInput('ResponseVar','Select Response Variable', ""),
                 selectInput('explanatoryVars','Select Predictor(s)', "",
                             multiple = TRUE),
                 
                 actionButton("fit","Fit Linear Model"),
                 actionButton("plotdata","Plot Linear Model"),
                 actionButton("plotdata","Plot Diagnositics"),
                 actionButton("anova","Run ANOVA"),
    ),
    mainPanel(plotOutput("plot"), 
              plotOutput("plotd"),
              verbatimTextOutput("model"),
              verbatimTextOutput("anova"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Reactive to store loaded data
  reactives <- reactiveValues(
    
    mydata = NULL
    
  )
  
  #Observe file being selected
  observeEvent(input$myfileinput, {
    #browser()
    #Store loaded data in reactive
    reactives$mydata <- read.csv(file = input$myfileinput$datapath)
    #browser()
    #Update select input
    updateSelectInput(session, 
                      inputId = 'ResponseVar', 
                      label = 'Select the response variable', 
                      choices  = colnames(reactives$mydata))
    updateSelectInput(session, 
                      inputId = 'explanatoryVars', 
                      label = 'Select explanatory variables',
                      choices  = colnames(reactives$mydata))
    
  })
  
  

  #Data table
  output$mytable <- renderTable({ 
    
    head(reactives$mydata)
    
  })
  
  observeEvent(input$plotdata,  output$plot <- renderPlot({
    
    browser()
    df = get.selected(input, reactives)
    
    lm()
    if(dim(df)[2] > 2){
      showNotification(stop(paste0('Cannot Display Regression Fit for model with ', dim(df)[2]-1,
                            ' predictors')),
                       type = 'error')
    }else{
      
      ggplot()+
        geom_point(aes(x = df[,2], y = df[,1]), 
                   shape =21, size = 2, 
                   color = 'black',fill = 'grey')+
        geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2],
                    lintype = 'dotted', size = 2, color = 'red')+
        theme_bw()+
        xlab(input$explanatoryVars)+
        ylab(input$responseVar)+
        theme(axis.text = element_text(size =12),
              axis.title = element_text(size = 14),
              legend.position = 'top')
      
    }
    
    plot(out)
    
  }))
  
  #Compute Regression and report results
  observeEvent(input$run,  output$summary <- renderPrint({
    #browser()
    }))
  
  #Compute ANOVA and report results
  observeEvent(input$run,  output$summary <- renderPrint({
    #browser()
  }))
}

# Run the application 
shinyApp(ui = ui, server = server)