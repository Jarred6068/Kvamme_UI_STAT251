#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggpubr)
library(knitr)
library(kableExtra)

convert.keystroke = function(keystroke_input){
  indata = strsplit(keystroke_input, ',')[[1]]
  if(sum(suppressWarnings(is.na(as.numeric(indata))))>0){
    converted = as.character(indata)
    values = unique(converted)
    counts = summary(as.factor(converted))
  }else{
    converted = as.numeric(indata)
    if(sum(converted%%1)>0){
      h = hist(converted, plot = F)
      values = h$breaks[-1]
      counts = h$counts
    }else{
      values = unique(converted)
      counts = summary(as.factor(converted))
    }
  }
  df = cbind.data.frame(X = values, frequency = counts)
  return(df)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Make Frequency Tables"),
  sidebarLayout(
    sidebarPanel(width = 3, 
                 #input select
                 # selectInput(inputId = "output.type",
                 #             label = "Select Output",
                 #             choices = c("Table", "Bar Plot"),
                 #             selected = "Table"),
                 textInput(inputId = "keystroke_input", 
                           label = "Enter Comma Separated Values (No spaces!)", 
                           value = "1,2,3, ... or cat, dog, dog, ..."),
                 actionButton("Run","Make table"),
                 actionButton("Plot","Make Plot")
                 ),
    mainPanel("main panel",
              tableOutput("table"),
              plotOutput("plot")
              )
  )
)


server <- function(input, output) {
  
  observeEvent(input$Run,
               output$table <- renderTable({
                 convert.keystroke(input$keystroke_input)
                 }))
  observeEvent(input$Plot,
               output$plot <- renderPlot({
                 ggplot(data = convert.keystroke(input$keystroke_input),
                        aes(x = X, y = frequency))+
                   geom_bar(stat = 'identity', color = 'black', fill = 'grey')+
                   theme_classic2()+
                   xlab('Value')+
                   ylab('Frequnecy')},
                 height=300))
}

shinyApp(ui = ui, server = server)
