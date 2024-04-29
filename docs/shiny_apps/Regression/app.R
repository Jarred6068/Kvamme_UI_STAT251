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
library(GGally)


convert.keystroke = function(input){
  return(strsplit(input$keystroke_input, ',')[[1]])
}


get.selected <- function(input, reactives){
  
  idx1 = which(colnames(reactives$mydata) == input$ResponseVar)
  idx2 = match(input$explanatoryVars, colnames(reactives$mydata))
  Y = reactives$mydata[,idx1]
  X = reactives$mydata[,idx2]
  df = cbind.data.frame(Y, X)
  
  if(input$keystroke_input != ""){
    quals = convert.keystroke(input)
    convert.idx = na.omit(match(quals, c(input$ResponseVar, input$explanatoryVars)))
    for(i in 1:length(convert.idx)){
      df[,convert.idx[i]] = as.factor(df[,convert.idx[i]])
    }
  }
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
                 
                 textInput('keystroke_input', 
                              'Enter names of selected predictors that should be treated as factors (comma separated)',
                           value = "",
                           placeholder = 'variable1, variable2,...'),
                 
                 selectInput(inputId = 'label_points',
                             label = 'Label data points in plot?',
                             choices = c('TRUE','FALSE'),
                             selected = 'TRUE'),
                 conditionalPanel("input.label_points == 'TRUE'",
                                  textInput('labels_cn',
                                               'Give column name for data labels',
                                                value = "",
                                            placeholder = 'column_name'),
                                  numericInput('yjust',
                                               'adjust labels (y-axis)',
                                               value = 0),
                                  numericInput('fs',
                                               'select font size',
                                               value = 5,
                                               step = 1)),
                 
                 actionButton("fit","Fit Linear Model"),
                 actionButton("plotdata","Plot Linear Model"),
                 actionButton("plotdiagnostics","Plot Diagnositics"),
                 actionButton("anova","Run ANOVA")
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
    
    #browser()
    df = get.selected(input, reactives)
    
    model = lm(formula(df), data = df)
    if(dim(df)[2] > 2){
      
      showNotification(message('generating plot...this may take a few seconds..'),
                       type = 'message')
      ggpairs(data = df, progress = FALSE)
    }else{
      
      
      if(input$keystroke_input != ""){
        
        means = unlist(lapply(unique(df[,2]), 
                              function(x,y,z) mean(y[z == x]), 
                              y = df[,1], z=df[,2]))
        out = ggplot()+
          
          geom_boxplot(aes(x = df[,2], y = df[,1]),
                     color = 'black',fill = 'grey')+
          geom_point(aes(x = levels(df[,2]), y = means),
                     size = 5, color = 'red', alpha = 0.8)+
          theme_bw()+
          xlab(input$explanatoryVars)+
          ylab(input$responseVar)+
          ggtitle('Red points indicate conditional means')
          theme(axis.text = element_text(size =12),
                axis.title = element_text(size = 14),
                legend.position = 'top')
        
      }else{
        out = ggplot()+
          geom_point(aes(x = df[,2], y = df[,1]), 
                     shape =21, size = 2.3, 
                     color = 'black',fill = 'grey')+
          geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2],
                      linetype = 'dotted', size = 2.5, color = 'red')+
          theme_bw()+
          xlab(input$explanatoryVars)+
          ylab(input$responseVar)+
          theme(axis.text = element_text(size =12),
                axis.title = element_text(size = 14),
                legend.position = 'top')
        
        if(input$label_points == 'TRUE'){
          n = dim(df)[1]
          if(input$labels_cn != ""){
            data.labels = reactives$mydata[,which(colnames(reactives$mydata) == input$labels_cn)]
          }else{
            data.labels = c(1:dim(df)[1])
          }
          
          out = out+geom_text(aes(x = df[,2], y = df[,1]-input$yjust), label = data.labels,
                              size = input$fs, color = 'blue')
        }
      }
      
      plot(out)
    }
    
  }))
  
  
  #make diagnostic plots
  observeEvent(input$plotdiagnostics,  output$plotd <- renderPlot({
    
    #browser()
    df = get.selected(input, reactives)
    
    model = lm(formula(df), data = df)
    
    par(mfrow = c(2,2))
    plot(model)
    par(mfrow = c(1,1))
    
  }))
  
  #Compute Regression and report results
  observeEvent(input$fit,  output$model <- renderPrint({
    #browser()
    #browser()
    df = get.selected(input, reactives)
    
    model = lm(formula(df), data = df)
    summary(model)
    }))
  
  #Compute ANOVA and report results
  observeEvent(input$anova,  output$anova <- renderPrint({
    #browser()
    #browser()
    df = get.selected(input, reactives)
    
    model = lm(formula(df), data = df)
    anova(model)
  }))
}

# Run the application 
shinyApp(ui = ui, server = server)