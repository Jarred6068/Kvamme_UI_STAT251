#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#source('C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/stat251_tools.R')
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(latex2exp)
library(shiny)

#this function is a helper function for gen.density.plot
d_limit <- function(x, input){
  if(input$rv == 'Uniform'){
    y = dunif(x, min = input$min, max = input$max)
  }else if(input$rv == 'Normal'){
    y = dnorm(x, input$mean, input$sd)
  }else if(input$rv == 'Students t'){
    y = dt(x, input$df)
  }
  
  
  if(input$probtype == 'P(X = k)'){
    return(y)
  }else if (input$probtype == 'P(X >= k)'){
    y[which(x <= input$k)] <- NA
    return(y)
  }else if(input$probtype == 'P(X <= k)'){
    y[which(x >= input$k)] <- NA
    return(y)
  }
  
}




ui <- fluidPage(
  titlePanel('Compute Probabilties From Different Distributions'),
  sidebarLayout(
    sidebarPanel(width = 4, 
                 #input select
                 selectInput(inputId = "probtype",
                             label = "Select probability",
                             choices = c("P(X = k)", "P(X <= k)", "P(X >= k)"),
                             selected = "P(X = k)"),
                 
                 numericInput(inputId = 'k',
                              label = "k",
                              value = 1),
                 
                 selectInput(inputId = "rv",
                             label = "Select Random Variable",
                             choices = c("Binomial", "Poisson", "Uniform", "Exponential", "Normal", "Students t"),
                             selected = "Binomial"),
                 #Binomial dist
                 conditionalPanel(condition = "input.rv == 'Binomial'",
                                  h5("Binomial Parameters"),
                                  numericInput(inputId = "ntrials",
                                              label = "Number of Trials:",
                                              min = 1,
                                              value = 1),
                                  numericInput(inputId = "probsuccess",
                                              label = "Probability of Success:",
                                              min = 0,
                                              max = 1,
                                              value = 0.5,
                                              step = 0.05)),
                 #Students t distribution
                 conditionalPanel(condition = "input.rv == 'Students t'",
                                  h5("Binomial Parameters"),
                                  numericInput(inputId = "df",
                                               label = "Degrees of Freedom:",
                                               min = 1,
                                               value = 1)),
                 
                 #Poisson dist
                 conditionalPanel(condition = "input.rv == 'Poisson'",
                                  h5("Poisson Parameters"),
                                  numericInput(inputId = "rate1",
                                              label = "Rate:",
                                              min = 0,
                                              value = 1,
                                              step = 0.5)),
                 #Uniform dist
                 conditionalPanel(condition = "input.rv == 'Uniform'",
                                  h5("Uniform Parameters"),
                                  numericInput(inputId = "min",
                                              label = "Minimum Value:",
                                              value = 0),
                                  numericInput(inputId = "max",
                                              label = "Maximum Value:",
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
    mainPanel("", plotOutput("distPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$Compute,  output$distPlot <- renderPlot({
      set.seed(111)
      #browser()
    
    
      #Binomial 
      if(input$rv == 'Binomial'){
        df = data.frame(success = 0:input$ntrials, 
                        prob = dbinom(x = 0:input$ntrials, size = input$ntrials, prob = input$probsuccess))
        if(input$probtype == 'P(X = k)'){
          df=df %>% mutate(condition = ifelse(success == input$k, as.character(input$k), "failure")) 
          lbl = paste0('$P(X = ', input$k, ') = ')
        }else if (input$probtype == 'P(X <= k)'){
           df=df %>% mutate(condition = ifelse(success <= input$k, as.character(input$k), "failure")) 
           lbl = paste0('$P(X \\leq ', input$k, ') = ')
        }else{
          df=df %>% mutate(condition = ifelse(success >= input$k, as.character(input$k), "failure"))
          lbl = paste0('$P(X \\geq ', input$k, ') = ')
        }
        
        px = round(sum(df$prob[which(df$condition == input$k)]),2)
        ggplot(data = df, aes(x = success, y = prob, fill = condition)) +
          geom_bar(stat = 'identity', color = 'black') +
          geom_col() +
          geom_text(aes(label = round(prob,2), y = prob + 0.01),
                    position = position_dodge(0.9),
                    size = 3,
                    vjust = 0)+
          geom_text(aes(x = mean(success),
                        y = max(prob) + 0.08),
                    label = TeX(paste0(lbl, px, '$')),
                    size = 5,
                    vjust = 0)+
          scale_y_continuous(limits = c(0, min(max(df$prob+0.15), 1)))+
          labs(title = paste0("Probability of X =", input$k, " successes."),
               subtitle = TeX(paste0("$X \\sim binom(n = ", input$ntrials, ", p = ", input$probsuccess, ')$')),
               x = "Number of Successes",
               y = "Probability")+
          theme_classic2()+
          theme(legend.position = 'none')
      
        
        
        
        
      #poisson
      }else if(input$rv == 'Poisson'){
        df = data.frame(events = 0:ceiling(input$rate1+5*input$rate1), 
                        prob = dpois(x = 0:ceiling(input$rate1+5*input$rate1), 
                                     lambda = input$rate1))
        if(input$probtype == 'P(X = k)'){
          df=df %>% mutate(condition = ifelse(events == input$k, as.character(input$k), "failure")) 
          lbl = paste0('$P(X = ', input$k, ') = ')
        }else if (input$probtype == 'P(X <= k)'){
          df=df %>% mutate(condition = ifelse(events <= input$k, as.character(input$k), "failure")) 
          lbl = paste0('$P(X \\leq ', input$k, ') = ')
        }else{
          df=df %>% mutate(condition = ifelse(events >= input$k, as.character(input$k), "failure"))
          lbl = paste0('$P(X \\geq ', input$k, ') = ')
        }
        
        px = round(sum(df$prob[which(df$condition == input$k)]),2)
        ggplot(data = df, aes(x = events, y = prob, fill = condition)) +
          geom_bar(stat = 'identity', color = 'black') +
          geom_col() +
          geom_text(aes(label = round(prob,2), y = prob + 0.01),
                    position = position_dodge(0.9),
                    size = 3,
                    vjust = 0)+
          geom_text(aes(x = mean(events),
                        y = max(prob) + 0.08),
                    label = TeX(paste0(lbl, px, '$')),
                    size = 5,
                    vjust = 0)+
          scale_y_continuous(limits = c(0, min(max(df$prob+0.15), 1)))+
          labs(title = paste0("Probability of X =", input$k, " Events."),
               subtitle = TeX(paste0("$X\\sim Poisson(\\lambda = ", input$rate1, ')$')),
               x = "Number of Events",
               y = "Probability")+
          theme_classic2()+
          theme(legend.position = 'none')
        
        
        
      # Uniform
      }else if(input$rv == 'Uniform'){
        if(input$probtype == 'P(X = k)'){
          px = round(dunif(input$k, input$min, input$max),2)
          lbl = paste0('$P(X = ', input$k, ') = ')
        }else if (input$probtype == 'P(X <= k)'){
          px = round(punif(input$k, input$min, input$max),2)
          lbl = paste0('$P(X \\leq ', input$k, ') = ')
        }else{
          px = 1-round(punif(input$k, input$min, input$max),2)
          lbl = paste0('$P(X \\geq ', input$k, ') = ')
        }
        
        ggplot(data.frame(x = c(input$min, input$max)), aes(x = x))+
          stat_function(fun = d_limit,
                        args = list(input = input), 
                        geom = "area", 
                        fill = "cyan3", 
                        alpha = 0.4)+
          geom_text(aes(x = median(input$min:input$max),
                        y = (1/(input$max - input$min))/2),
                    label = TeX(paste0(lbl, px, '$')),
                    size = 5,
                    vjust = 0)+
          stat_function(fun = dunif,
                        args = list(min = input$min, max = input$max),
                        size = 1)+
          labs(title = paste0("Probability of X"),
               subtitle = TeX(paste0("$X\\sim Uniform(\\min = ", input$min, ', \\max = ',input$max, ')$')),
               x = "X",
               y = "Probability(X)")+
          theme_classic2()+
          theme(legend.position = 'none')
      #   d = runif(n=input$ss, min = input$min, max = input$max)
      # }else if(input$model == 'Exponential'){
      #   d = rexp(n=input$ss, rate = input$rate2)
    }else if(input$rv == 'Normal'){
      if(input$probtype == 'P(X = k)'){
        px = round(dnorm(input$k, input$mean, input$sd),2)
        lbl = paste0('$P(X = ', input$k, ') = ')
      }else if (input$probtype == 'P(X <= k)'){
        px = round(pnorm(input$k, input$mean, input$sd),2)
        lbl = paste0('$P(X \\leq ', input$k, ') = ')
      }else{
        px = 1-round(pnorm(input$k, input$mean, input$sd),2)
        lbl = paste0('$P(X \\geq ', input$k, ') = ')
      }
    
      ggplot(data.frame(x = c(input$mean - 4*input$sd, input$mean + 4*input$sd)), aes(x = x))+
        stat_function(fun = d_limit,
                      args = list(input = input), 
                      geom = "area", 
                      fill = "cyan3", 
                      alpha = 0.4)+
        geom_text(aes(x = input$mean,
                      y = dnorm(input$mean, input$mean, input$sd)/2),
                  label = TeX(paste0(lbl, px, '$')),
                  size = 5,
                  vjust = 0)+
        stat_function(fun = dnorm,
                      args = list(mean = input$mean, sd = input$sd),
                      size = 1)+
        labs(title = paste0("Probability of X"),
             subtitle = TeX(paste0("$X\\sim N(\\mu = ", input$mean, ', \\sigma = ',input$sd, ')$')),
             x = "X",
            y = "Probability(X)")+
        theme_classic2()+
        theme(legend.position = 'none')
      
    
      
    #t distribution
    }else if(input$rv == 'Students t'){
      if(input$probtype == 'P(X = k)'){
        px = round(dt(input$k, input$df),2)
        lbl = paste0('$P(t = ', input$k, ') = ')
      }else if (input$probtype == 'P(X <= k)'){
        px = round(pt(input$k, input$df),2)
        lbl = paste0('$P(t \\leq ', input$k, ') = ')
      }else{
        px = 1-round(pt(input$k, input$df),2)
        lbl = paste0('$P(t \\geq ', input$k, ') = ')
      }
      
      ggplot(data.frame(x = c(-4, 4)), aes(x = x))+
        stat_function(fun = d_limit,
                      args = list(input = input), 
                      geom = "area", 
                      fill = "cyan3", 
                      alpha = 0.4)+
        geom_text(aes(x = 0,
                      y = dt(0, input$df)/2),
                  label = TeX(paste0(lbl, px, '$')),
                  size = 5,
                  vjust = 0)+
        stat_function(fun = dt,
                      args = list(df = input$df),
                      size = 1)+
        labs(title = paste0("Probability of t"),
             subtitle = TeX(paste0("$t\\sim t(df = ", input$df, ')$')),
             x = "t",
             y = "Probability(t)")+
        theme_classic2()+
        theme(legend.position = 'none')
    }
  }))
}

# Run the application 
shinyApp(ui = ui, server = server)
