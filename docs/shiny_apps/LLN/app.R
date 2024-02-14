#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plotly)
library(dplyr)
library(ggplot2)
library(ggthemes)


LLNbern<-function(S,p,n){
  #S should be the max sample
  #p is the probability of the outcome
  #n is the number of trials in the experiment (for bernoulli = 1)
  #NOTE: when n > 1 this function transfers to a binomial exper.
  
  avg=rep(0, S)
  expected = n*p
  positive.outcomes=cumsum(rbinom(S, n, p))
  trials=c(1:S)
  avg=positive.outcomes/trials
  error = avg - expected
  variance = c(0,unlist(lapply(c(2:S), function(x,y)  var(y[1:x]), y = avg)))
  
  
  df = cbind.data.frame(Y = c(avg,variance), LN = c(rep("Running Prop.", S), rep("Var. In Prop.", S)),
                        SS = rep(trials, 2))
  
  
  return(df)
  
}


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}



make.animated.plot = function(experiment, xlab = "The Number of Trials", ylab ="Proportion of Flips That Are Heads",
                              expected.value = NULL, expected.variance = NULL){
  
  
  
  fig <- experiment %>% accumulate_by(~SS)
  #first animation
  fig <- fig %>%
    plot_ly(
      x = ~ SS, 
      y = ~ Y,
      frame = ~frame, 
      type = 'scatter',
      mode = 'lines', 
      split = ~LN,
      line = list(simplyfy = F)
    ) %>% layout(
      xaxis = list(
        title = xlab,
        zeroline = F
      ),
      yaxis = list(
        title = ylab,
        zeroline = F
      ),
      shapes = list(hline(expected.value), hline(expected.variance))
    ) %>% animation_opts(
      frame = 100, 
      transition = 0, 
      redraw = FALSE
    ) %>% animation_slider(
      hide = T
    )%>% animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )
  
  fig
}


# n = 1
# p = 0.5
# S = 500
# 
# exp.bernoulli = LLNbern(S, p, n)
# make.animated.plot(exp.bernoulli, expected.value = p, expected.variance = 0)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Law of Large Numbers"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("resamples",
                        "number of resamples:",
                        min = 1,
                        max = 1000,
                        value = 100),
            sliderInput("probability",
                        "probability of event:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            sliderInput("trials",
                        "number of trials:",
                        min = 1,
                        max = 100,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    exp.bernoulli = LLNbern(input$resamples, input$probability, input$trials)
    make.animated.plot(exp.bernoulli, expected.value = input$trials*input$probability, 
                       expected.variance = (input$probability*(1-input$probability))/input$resamples)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
