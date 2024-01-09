#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


getmode <- function(v) {
  cts = summary(as.factor(v))
  if(max(summary(as.factor(v))) == 1){
    NA 
  }else{
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))] 
  }
}


quantile_fun <- function(x){
  l = length(x)
  if(l%%2 == 0){
    midpoint = l/2
    Q1 = median(x[1:midpoint])
    Q3 = median(x[(midpoint+1):length(x)])
  }else{
    midpoint = l/2
    break1 = floor(midpoint)
    break2 = ceiling(midpoint)+1
    Q1 = median(x[1:break1])
    Q3 = median(x[break2:length(x)])
  }
  return(c(Q1, Q3))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Statistic Calculator"),
    sidebarLayout(
      sidebarPanel(width = 4, 
                   selectInput(inputId = "stat",
                               label = "Select Statistic",
                               choices = c("mean", 
                                           "variance", 
                                           "standard deviation",
                                           "median",
                                           "mode",
                                           "range",
                                           "quartiles",
                                           "IQR"),
                               selected = "Normal"),
                   textInput(inputId = "keystroke_input", 
                             label = "Enter Comma Separated Values", 
                             value = "1,2,3,4,5"),
                   actionButton("go","Run Function")),
      mainPanel(textOutput(outputId = "keystoke_class"))
    )
)

server <- function(input, output) {
  
  
  observeEvent(input$go,
               output$keystoke_class <- renderText({
                 if(input$stat == "mean"){
                   paste('mean = ', mean(as.numeric(strsplit(input$keystroke_input, ',')[[1]])))
                 }else if(input$stat == "variance"){
                   paste('Var = ', var(as.numeric(strsplit(input$keystroke_input, ',')[[1]])))
                 }else if(input$stat == "standard deviation"){
                   paste('StDev =',  sd(as.numeric(strsplit(input$keystroke_input, ',')[[1]])))
                 }else if(input$stat == "median"){
                   paste('median = ', median(as.numeric(strsplit(input$keystroke_input, ',')[[1]])))
                 }else if(input$stat == "mode"){
                   paste('mode = ', getmode(as.numeric(strsplit(input$keystroke_input, ',')[[1]])))
                 }else if(input$stat == "range"){
                   paste(c('min = ', 'max = '), 
                         range(as.numeric(strsplit(input$keystroke_input, ',')[[1]])))
                 }else if(input$stat == "quartiles"){
                   paste(c('Q1 = ', 'Q3 = '), 
                         quantile_fun(as.numeric(strsplit(input$keystroke_input, ',')[[1]])))
                 }else if(input$stat == "IQR"){
                   paste('Interquartile Range = ', 
                         IQR(as.numeric(strsplit(input$keystroke_input, ',')[[1]])))
                 }})
  )
}

shinyApp(ui = ui, server = server)
