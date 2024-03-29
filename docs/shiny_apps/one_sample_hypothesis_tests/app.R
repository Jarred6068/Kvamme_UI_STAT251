#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggthemes)
library(latex2exp)
library(gridExtra)
library(ggpubr)
library(plyr)
library(VennDiagram)

#this function is a helper function for gen.density.plot
d_limit <- function(x, alpha, test, dist, n) {
  if(dist == 'z'){
    y = dnorm(x)
    q_a = qnorm(1-alpha)
  }else{
    y = dt(x, df = n-1)
    q_a = qt(1-alpha, df = n-1)
  }
  
  
  if(test == 'two.tailed'){
    y[which(x>-q_a & x<q_a)] <- NA  
    return(y)
  }else if (test == 'upper.tail'){
    y[which(x < q_a)] <- NA
    return(y)
  }else{
    y[which(x > -q_a)] <- NA
    return(y)
  }
  
}


# this function plots a density curve for the z and t distributions and allows for shading the 
# the rejection regions on the plot
gen.density.plot=function(bbox = c(-4,4), n = 10, dist = c('z','t'), alpha = 0.05, obs = 0.5,
                          test = c('two.tailed','lower.tail', 'upper.tail'), shade = TRUE,
                          txt.sz = 5){
  
  p = ggplot(data.frame(x = bbox), aes(x = x))+
    theme_bw()
  if(shade == TRUE){
    p = p + stat_function(fun = d_limit,
                          args = list(alpha = alpha,
                                      test = test,
                                      dist = dist,
                                      n = n), 
                          geom = "area", 
                          fill = "cyan3", 
                          alpha = 0.4)
  }
  if(dist == 'z'){
    q_a = qnorm(1-alpha)
    d_obs = dnorm(obs)
    d_a = dnorm(q_a)
    func = dnorm
    args = list()
    xlabel = "z"
  }else{
    q_a = qt(1-alpha, df = n-1)
    d_obs = dt(obs, df = n-1)
    d_a = dt(q_a, df = n-1)
    func = dt
    args = list(df = n-1)
    xlabel = 't'
  }
  
  if(test == 'two.tailed'){
    xcoords = c(-q_a, q_a, obs)
    ycoords = c(0,0,0)
    xendcoords = c(-q_a, q_a, obs)
    yendcoords = c(d_a, d_a, d_obs)
    if(dist == 'z'){
      lbl1 = c(TeX("$z_{\\alpha/2}$"),TeX("$z_{1-\\alpha/2}$"),TeX("$z_{obs}$"))
    }else{
      lbl1 = c(TeX("$t_{\\alpha/2}$"),TeX("$t_{1-\\alpha/2}$"),TeX("$t_{obs}$"))
    }
  }else if (test == 'upper.tail'){
    xcoords = c(q_a, obs)
    ycoords = c(0,0)
    xendcoords = c(q_a, obs)
    yendcoords = c(d_a, d_obs)
    if(dist == 'z'){
      lbl1 = c(TeX("$z_{1-\\alpha}$"),TeX("$z_{obs}$"))
    }else{
      lbl1 = c(TeX("$t_{1-\\alpha}$"),TeX("$t_{obs}$"))
    }
  }else{
    xcoords = c(-q_a, obs)
    ycoords = c(0,0)
    xendcoords = c(-q_a, obs)
    yendcoords = c(d_a, d_obs)
    if(dist == 'z'){
      lbl1 = c(TeX("$z_{\\alpha}$"), TeX("$z_{obs}$"))
    }else{
      lbl1 = c(TeX("$t_{\\alpha}$") ,TeX("$t_{obs}$"))
    }
  }
  
  if(test == 'two.tailed'){
    final = p+stat_function(fun = func,
                            args = args,
                            size = 1)+
      geom_segment(aes(x=xcoords[1:2],
                       y=ycoords[1:2],
                       xend=xendcoords[1:2],
                       yend=yendcoords[1:2]),
                   size = 0.9,
                   linetype = 'dashed')+
      geom_segment(aes(x=xcoords[3],
                       y=ycoords[3],
                       xend=xendcoords[3],
                       yend=yendcoords[3]),
                   size = 0.9,
                   linetype = 'dashed')+
      geom_point(aes(x = xcoords[1:2], y = ycoords[1:2]), size = 3)+
      geom_label(aes(x = xcoords[1:2], y = ycoords[1:2]-0.025), label = lbl1[1:2], size = txt.sz)+
      geom_point(aes(x = xcoords[3], y = ycoords[3]), size = 3)+
      geom_label(aes(x = xcoords[3], y = ycoords[3]-0.025), label = lbl1[3], size = txt.sz)+
      geom_hline(yintercept = 0, size = 1)+
      xlab(xlabel)+
      ylab('Probability Density')+
      ggtitle(TeX('Distribution under $H_0$'))+
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  }else{
    final = p+stat_function(fun = func,
                            args = args,
                            size = 1)+
      geom_segment(aes(x=xcoords,
                       y=ycoords,
                       xend=xendcoords,
                       yend=yendcoords), 
                   size = rep(0.9, length(xcoords)), 
                   linetype = rep('dashed',length(xcoords)))+
      geom_point(aes(x = xcoords, y = ycoords), size = 3)+
      geom_label(aes(x = xcoords, y = ycoords-0.025), label = lbl1, size = txt.sz)+
      geom_hline(yintercept = 0, size = 1)+
      xlab(xlabel)+
      ylab('Probability Density')+
      ggtitle(TeX('Distribution under $H_0$'))+
      theme(axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12))
  }
  
  
  return(final)
}








# standard error for a sample proportion 
one.sample.prop.SE = function(phat, n){
  SE = sqrt((phat*(1-phat))/n)
  return(SE)
}


# confidence interval for a population proportion 
one.sample.prop.CI = function(phat, n, alpha = 0.05, verbose = FALSE){
  SE = one.sample.prop.SE(phat, n)
  standard.score = qnorm(1-(alpha/2))
  MOE = standard.score*SE
  lower.bound = phat-MOE
  upper.bound = phat+MOE
  if(isTRUE(verbose)){
    print(noquote(paste0(rep('-', 50), collapse = '')))
    print(noquote(paste0('Estimate = ', round(phat, 4))))
    print(noquote(paste0('Critical Value  = ', round(standard.score, 4))))
    print(noquote(paste0('Estimated Standard Error = ', round(SE, 4))))
    print(noquote(paste0('Margin of error = ', round(MOE, 4))))
    print(noquote(paste0((1-alpha)*100, ' % CI = ', paste0('[', round(max(lower.bound,0),4), ',',
                                                           round(min(upper.bound, 1), 4), ']', 
                                                           collapse = ''))))
    print(noquote(paste0(rep('-', 50), collapse = '')))
  }
  return(c(lower.bound, upper.bound))
}



# test for a population proportion 
one.sample.prop.test = function(p0, phat, n, alpha = 0.05, test = c('lower.tail','upper.tail','two.tail'),
                                verbose = TRUE){
  
  estimate.SE = one.sample.prop.SE(p0, n)
  Zobs = (phat - p0)/estimate.SE
  if(test == 'two.tail'){
    critical.value = qnorm((1-(alpha/2)))
    alt.hyp = 'p != '
    pvalue = 2*(1-pnorm(abs(Zobs)))
  }else if(test == 'lower.tail'){
    critical.value = qnorm(alpha)
    alt.hyp = 'p < '
    pvalue = pnorm(Zobs)
  }else{
    critical.value = qnorm(1-alpha)
    alt.hyp = 'p > '
    pvalue = 1-pnorm(Zobs)
  }
  
  CI = one.sample.prop.CI(phat, n, alpha)
  decision = ifelse(pvalue<alpha, 'reject H0', 'fail to reject H0')
  if(isTRUE(verbose)){
    print(noquote(paste0(paste0(rep('=', 20), collapse = ''), ' test results ', paste0(rep('=', 20), collapse = ''))))
    print(noquote(paste0('test type = ', test)))
    print(noquote(paste0('H0: p0 = ', p0)))
    print(noquote(paste0('HA: ', alt.hyp, p0)))
    print(noquote(paste0('estimate = ', round(phat,4))))
    print(noquote(paste0('Estimated Standard Error = ', round(estimate.SE,4))))
    print(noquote(paste0('Critical Value = ', round(critical.value, 4))))
    print(noquote(paste0((1-alpha)*100, '% CI = ', paste0('[', max(round(CI[1],4), 0),',',
                                                          round(CI[2],4),']', 
                                                          collapse = ''))))
    print(noquote(paste0('Test statistic = ', round(Zobs, 4))))
    print(noquote(paste0('Pvalue = ', round(pvalue, 4))))
    print(noquote(paste0('Decision: ', decision)))
    print(noquote(paste0(rep("=", 54), collapse = '')))
  }
} 












#standard error for a sample mean 
one.sample.t.SE = function(s, n){
  SE = s/sqrt(n)
  return(SE)
}


#confidence interval for a population mean 
one.sample.t.CI = function(xbar, s, n, alpha = 0.05, verbose = FALSE){
  SE = one.sample.t.SE(s, n)
  t.score = qt(1-(alpha/2), df = n-1)
  MOE = t.score*SE
  lower.bound = xbar-MOE
  upper.bound = xbar+MOE
  if(isTRUE(verbose)){
    print(noquote(paste0(rep('-', 50), collapse = '')))
    print(noquote(paste0('Estimate = ', round(xbar, 4))))
    print(noquote(paste0('Critical Value  = ', round(t.score, 4))))
    print(noquote(paste0('Estimated Standard Error = ', round(SE, 4))))
    print(noquote(paste0('Margin of error = ', round(MOE, 4))))
    print(noquote(paste0((1-alpha)*100, ' % CI = ', paste0('[', round(lower.bound,4),',', 
                                                           round(upper.bound,4), ']',
                                                           collapse = ''))))
    print(noquote(paste0(rep('-', 50), collapse = '')))
  }
  return(c(lower.bound, upper.bound))
}


# one sample t test 
one.sample.t.test = function(m0, xbar, s, n, alpha = 0.05, test = c('lower.tail','upper.tail','two.tail'),
                             verbose = TRUE){
  
  df = n - 1
  estimate.SE = one.sample.t.SE(s, n)
  tobs = (xbar - m0)/estimate.SE
  if(test == 'two.tail'){
    critical.value = qt((1-(alpha/2)), df = df)
    alt.hyp = 'm != '
    pvalue = 2*(1-pt(abs(tobs), df = df))
  }else if(test == 'lower.tail'){
    critical.value = qt(alpha, df = df)
    alt.hyp = 'm < '
    pvalue = pt(tobs, df = df)
  }else{
    critical.value = qt(1-alpha, df = df)
    alt.hyp = 'm > '
    pvalue = 1-pt(tobs, df = df)
  }
  
  CI = one.sample.t.CI(xbar, s, n, alpha)
  decision = ifelse(pvalue<alpha, 'reject H0', 'fail to reject H0')
  if(isTRUE(verbose)){
    print(noquote(paste0(paste0(rep('=', 20), collapse = ''), ' test results ', paste0(rep('=', 20), collapse = ''))))
    print(noquote(paste0('test type = ', test)))
    print(noquote(paste0('H0: m0 = ', m0)))
    print(noquote(paste0('HA: ', alt.hyp, m0)))
    print(noquote(paste0('Estimate = ', round(xbar,4))))
    print(noquote(paste0('Estimated Standard Error = ', round(estimate.SE,4))))
    print(noquote(paste0('Critical Value = ', round(critical.value, 4))))
    print(noquote(paste0((1-alpha)*100, '% CI = ', paste0('[', max(round(CI[1],4), 0),',',
                                                          round(CI[2],4),']', 
                                                          collapse = ''))))
    print(noquote(paste0('Test statistic = ', round(tobs, 4))))
    print(noquote(paste0('Degrees of Freedom = ', round(df, 4))))
    print(noquote(paste0('Pvalue = ', round(pvalue, 4))))
    print(noquote(paste0('Decision: ', decision)))
    print(noquote(paste0(rep("=", 54), collapse = '')))
  }
} 





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
        SE = sqrt(input$p0*(1-input$p0)/input$n)
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
