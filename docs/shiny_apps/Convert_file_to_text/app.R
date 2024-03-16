
library(shiny)

ui <- fluidPage(  
  titlePanel('Convert values to csv text'),
  sidebarLayout(
    sidebarPanel(width = 4, 
  
      fileInput("myfileinput", 
                "Please choose a csv File", 
                multiple = FALSE, 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
      selectInput('myselectinput','Select variable', ""),
      actionButton("print","Print")),
      
      mainPanel("", tableOutput('mytable'),
                verbatimTextOutput("text"))
    
  )
)
      

server <- function(input, output, session) {
  
  #Reactive to store loaded data
  reactives <- reactiveValues(
    
    mydata = NULL
    
  )
  
  #Observe file being selected
  observeEvent(input$myfileinput, {
    
    #Store loaded data in reactive
    reactives$mydata <- read.csv(file = input$myfileinput$datapath)
    #browser()
    #Update select input
    updateSelectInput(session, inputId = 'myselectinput', label = 'Select the first var', choices  = colnames(reactives$mydata))
    
  })
  
  #Data table
  output$mytable <- renderTable({ 
    
    head(reactives$mydata)
    
  })
  
  
  observeEvent(input$print,  output$text <- renderPrint({
    idx = which(colnames(reactives$mydata) == input$myselectinput)
    paste0(reactives$mydata[,idx], collapse = ',')
  }))
  
}

shinyApp(ui = ui, server = server)