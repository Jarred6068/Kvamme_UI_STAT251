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



#unpooled standard error for difference in two proportions
two.sample.prop.SE = function(p1,p2,n1,n2){
  SE = sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
  return(SE)
}

#computing confidence intervals for the difference between two population proportions
two.sample.prop.CI = function(p1,p2,n1,n2,tail = 'two.tail',alpha = 0.05, verbose = FALSE){
  diff = p1-p2
  bounds=c(0,0)
  if(tail != 'two.tail'){
    Z = qnorm(1-alpha)
  }else{
    Z = qnorm(1-(alpha/2))
  }
  SE = two.sample.prop.SE(p1,p2,n1,n2)
  MOE = Z*SE
  bounds[1] = diff - MOE
  bounds[2] = diff + MOE
  bounds = sort(bounds)
  if(isTRUE(verbose)){
    print(noquote(paste0(rep('-', 50), collapse = '')))
    print(noquote(paste0('Estimate = ', round(diff, 4))))
    print(noquote(paste0('Critical Value  = ', round(Z, 4))))
    print(noquote(paste0('Estimated Standard Error = ', round(SE, 4))))
    print(noquote(paste0('Margin of error = ', round(MOE, 4))))
    print(noquote(paste0((1-alpha)*100, ' % CI = ', paste0('[',round(bounds[1], 4), ',', 
                                                           round(bounds[2], 4), ']'))))
    print(noquote(paste0(rep('-', 50), collapse = '')))
  }
  
  return(list(estimate = diff, interval = bounds, margin.of.error = MOE, critical.value = Z))
}


#two sample proportion test
two.sample.prop.test = function(p0,x1,x2,n1,n2,alpha = 0.05, test = c('lower.tail','upper.tail','two.tail'),
                                pooled = TRUE, verbose = TRUE){
  
  
  p1 = x1/n1
  p2 = x2/n2
  estimate = p1 - p2
  estimate.SE = two.sample.prop.SE(p1, p2, n1, n2)
  if(isTRUE(pooled)){
    calc = 'pooled'
    p.pooled = (x1+x2)/(n1+n2)
    test.SE = sqrt( (p.pooled*(1-p.pooled))*(1/n1 + 1/n2) )
  }else{
    calc = 'unpooled'
    test.SE = two.sample.prop.SE(p1,p2,n1,n2) 
  }
  Zobs = (estimate - p0)/test.SE
  if(test == 'two.tail'){
    critical.value = qnorm((1-(alpha/2)))
    alt.hyp = 'p1 - p2 != '
    pvalue = 2*(1-pnorm(abs(Zobs)))
  }else if(test == 'lower.tail'){
    critical.value = qnorm(alpha)
    alt.hyp = 'p1 - p2 < '
    pvalue = pnorm(Zobs)
  }else{
    critical.value = qnorm(1-alpha)
    alt.hyp = 'p1 - p2 > '
    pvalue = 1-pnorm(Zobs)
  }
  
  CI = sort(c(estimate - critical.value * estimate.SE, estimate + critical.value * estimate.SE))
  decision = ifelse(pvalue<alpha, 'reject H0', 'fail to reject H0')
  if(isTRUE(verbose)){
    print(noquote(paste0(paste0(rep('=', 20), collapse = ''), ' test results ', paste0(rep('=', 20), collapse = ''))))
    print(noquote(paste0('test type = ', test)))
    print(noquote(paste0('H0: p1 - p2 = ', p0)))
    print(noquote(paste0('HA: ', alt.hyp, p0)))
    print(noquote(paste0('estimate = ', round(estimate,4))))
    print(noquote(paste0(calc,' SE = ', round(test.SE,4))))
    print(noquote(paste0((1-alpha)*100, '% CI = ', paste0('[', max(round(CI[1],4), 0),',',
                                                          round(CI[2],4),']', 
                                                          collapse = ''))))
    print(noquote(paste0('Critical Value = ', round(critical.value,4))))
    print(noquote(paste0('Test statistic = ', round(Zobs, 4))))
    print(noquote(paste0('Pvalue = ', round(pvalue, 4))))
    print(noquote(paste0('Decision: ', decision)))
    print(noquote(paste0(rep("=", 54), collapse = '')))
  }
} 


#two sample unpooled standard error for means 
two.sample.t.SE = function(s1,s2,n1,n2){
  SE = sqrt((s1^2/n1)+(s2^2/n2))
  return(SE)
}

#confidence intervals for a difference between two means
two.sample.t.CI = function(x1,x2,s1,s2,n1,n2, tail = 'two.tail',alpha = 0.05, verbose = FALSE){
  diff = x1-x2
  bounds=c(0,0)
  df = min(n1-1, n2-1)
  if(tail != 'two.tail'){
    t.crit = qt(1-alpha, df = df)
  }else{
    t.crit = qt(1-(alpha/2), df = df)
  }
  SE = two.sample.t.SE(s1,s2,n1,n2)
  MOE = t.crit*SE
  bounds[1] = diff - MOE
  bounds[2] = diff + MOE
  bounds = sort(bounds)
  if(isTRUE(verbose)){
    print(noquote(paste0(rep('-', 50), collapse = '')))
    print(noquote(paste0('Estimate = ', round(diff, 4))))
    print(noquote(paste0('Critical Value  = ', round(t.crit, 4))))
    print(noquote(paste0('Estimated Standard Error = ', round(SE, 4))))
    print(noquote(paste0('Margin of error = ', round(MOE, 4))))
    print(noquote(paste0((1-alpha)*100, ' % CI = ', paste0('[',round(bounds[1], 4), ',', 
                                                           round(bounds[2], 4), ']'))))
    print(noquote(paste0(rep('-', 50), collapse = '')))
  }
  
  return(list(estimate = diff, interval = bounds, margin.of.error = MOE, critical.value = t.crit))
}


#two sample t test 
two.sample.t.test = function(m0,x1,x2,s1,s2,n1,n2,alpha = 0.05, test = c('lower.tail','upper.tail','two.tail'),
                             pooling = c('pooled', 'unpooled','approx.unpooled'), verbose = TRUE){
  
  estimate = x1 - x2
  estimate.SE = two.sample.t.SE(s1, s2, n1, n2)
  if(pooling == 'pooled'){
    calc = 'pooled'
    df = n1 + n2 - 2
    s.pooled = sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
    test.SE = s.pooled * sqrt((1/n1)+(1/n2))
  }else if (pooling == 'unpooled'){
    calc = 'exact.unpooled'
    df = (((s1^2/n1)+(s2^2/n2))^2)/(((s1^2/n1)^2/(n1-1)) + ((s2^2/n2)^2/(n2-1)))
    test.SE = two.sample.t.SE(s1,s2,n1,n2) 
  }else{
    calc = 'approx.unpooled'
    df = min(n1-1, n2-1)
    test.SE = two.sample.t.SE(s1,s2,n1,n2) 
  }
  tobs = (estimate - m0)/test.SE
  if(test == 'two.tail'){
    critical.value = qt((1-(alpha/2)), df = df)
    alt.hyp = 'm1 - m2 != '
    pvalue = 2*(1-pt(abs(tobs), df = df))
  }else if(test == 'lower.tail'){
    critical.value = qt(alpha, df = df)
    alt.hyp = 'm1 - m2 < '
    pvalue = pnorm(tobs)
  }else{
    critical.value = qt(1-alpha, df = df)
    alt.hyp = 'm1 - m2 > '
    pvalue = 1-pnorm(tobs)
  }
  
  CI = sort(c(estimate - critical.value * estimate.SE, estimate + critical.value * estimate.SE))
  decision = ifelse(pvalue<alpha, 'reject H0', 'fail to reject H0')
  if(isTRUE(verbose)){
    print(noquote(paste0(paste0(rep('=', 20), collapse = ''), ' test results ', paste0(rep('=', 20), collapse = ''))))
    print(noquote(paste0('test type = ', test)))
    print(noquote(paste0('H0: m1 - m2 = ', m0)))
    print(noquote(paste0('HA: ', alt.hyp, m0)))
    print(noquote(paste0('estimate = ', round(estimate,4))))
    print(noquote(paste0(calc,' SE = ', round(test.SE,4))))
    print(noquote(paste0((1-alpha)*100, '% CI = ', paste0('[',round(CI[1],4),',',
                                                          round(CI[2],4),']', 
                                                          collapse = ''))))
    print(noquote(paste0('Critical Value = ', round(critical.value,4))))
    print(noquote(paste0('Test statistic = ', round(tobs, 4))))
    print(noquote(paste0(calc,' degrees of freedom = ', round(df, 4))))
    print(noquote(paste0('Pvalue = ', round(pvalue, 4))))
    print(noquote(paste0('Decision: ', decision)))
    print(noquote(paste0(rep("=", 54), collapse = '')))
  }
}








# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel('Two-Sample Hypothesis Tests'),
  sidebarLayout(
    sidebarPanel(width = 4, 
                 #input select
                 selectInput(inputId = "param",
                             label = "Which Comparison",
                             choices = c("two.means", "two.proportions"),
                             selected = "two.means"),
                 
                 selectInput(inputId = "test",
                             label = "Choose inference type:",
                             choices = c("confidence.interval","upper.tail", 
                                         "lower.tail", "two.tail"),
                             selected = "two.tail"),
                 
                 sliderInput(inputId = "alpha",
                             label = "Significance Level:",
                             min = 0,
                             max = 1,
                             step = 0.01,
                             value = 0.05),
                 
                 numericInput(inputId = "n1",
                              label = "Sample Size Pop1: n1 ",
                              value = 12),
                 
                 numericInput(inputId = "n2",
                              label = "Sample Size Pop2: n2 ",
                              value = 10),
                 
                 conditionalPanel(condition = "input.param == 'two.means'",
                                  numericInput(inputId = "m0",
                                               label = "Set null value H_0: mu1 - mu2 = ",
                                               value = 0),
                                  numericInput(inputId = "xbar1",
                                               label = "Sample Mean Pop1: xbar1",
                                               value = 4),
                                  numericInput(inputId = "xbar2",
                                               label = "Sample Mean Pop2: xbar2",
                                               value = 5),
                                  numericInput(inputId = "s1",
                                               label = "Sample Standard Dev. Pop1: s1 ",
                                               value = 1),
                                  numericInput(inputId = "s2",
                                               label = "Sample Standard Dev. Pop2: s2 ",
                                               value = 1),
                                  selectInput(inputId = "pooling",
                                              label = "Choose pooling option:",
                                              choices = c("pooled", "unpooled", "approx.unpooled"),
                                              selected = "approx.unpooled")),
                 
                 conditionalPanel(condition = "input.param == 'two.proportions'",
                                  numericInput(inputId = "p0",
                                               label = "Set null difference H_0: p1 - p2 = ",
                                               value = 0,
                                               min = 0,
                                               max = 1,
                                               step = 0.01),
                                  numericInput(inputId = "x1",
                                               label = "Observed count for population 1: x1",
                                               min = 0,
                                               value = 10),
                                  numericInput(inputId = "x2",
                                               label = "Observed count for population 2: x2",
                                               min = 0,
                                               value = 8)),
                 actionButton("run","Run Inference"),
    ),
    mainPanel(plotOutput("plotresult"), 
              verbatimTextOutput("summary"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$run,  output$plotresult <- renderPlot({
    if(input$param == 'two.means'){
      lbl.height = 0.1
      distrib = 't'
      estimate = input$xbar1 - input$xbar2
      if(input$pooling == 'pooled'){
        df = input$n1 + input$n2 - 2
        s.pooled = sqrt( ((input$n1-1)*input$s1^2 + (input$n2-1)*input$s2^2)/(input$n1+input$n2-2))
        SE = s.pooled * sqrt((1/input$n1)+(1/input$n2))
      }else if (input$pooling == 'unpooled'){
        df = ( ( (input$s1^2/input$n1)+(input$s2^2/input$n2) )^2)/(((input$s1^2/input$n1)^2/(input$n1-1)) + ((input$s2^2/input$n2)^2/(input$n2-1)))
        SE = two.sample.t.SE(input$s1,input$s2,input$n1,input$n2)
      }else if(input$pooling == 'approx.unpooled'){
        df = min(input$n1-1, input$n2-1)
        SE = two.sample.t.SE(input$s1,input$s2,input$n1,input$n2)
      }
      standard.score = qt(1-input$alpha/2, df)
      LV = estimate - 4*SE
      UV = estimate + 4*SE
      LB = estimate - standard.score*SE
      UB = estimate + standard.score*SE
      calc = '\\bar{x}_1 - \\bar{x}_2 = '
      est1 = '$\\bar{x}_1$'
      est2 = '$\\bar{x}_2$'
      est.val1 = input$xbar1
      est.val2 = input$xbar2
      est.sd1 = input$s1/sqrt(input$n1)
      est.sd2 = input$s2/sqrt(input$n2)
    }else{
      lbl.height = 0.2
      distrib = 'z'
      estimate = (input$x1/input$n1) - (input$x2/input$n2)
      p.pooled = (input$x1+input$x2)/(input$n1+input$n2)
      SE = sqrt( (p.pooled*(1-p.pooled))*(1/input$n1 + 1/input$n2))
      standard.score = qnorm(1-input$alpha/2)
      LB = estimate - standard.score*SE
      LV = estimate - 4*SE
      UB = estimate + standard.score*SE
      UV = estimate + 4*SE
      calc = '\\hat{p}_1 - \\hat{p}_2 = '
      est1 = '$\\hat{p}_1$'
      est2 = '$\\hat{p}_2$'
      est.val1 = input$x1/input$n1
      est.val2 = input$x2/input$n2
      est.sd1 = sqrt(est.val1*(1-est.val1)/input$n1)
      est.sd2 = sqrt(est.val2*(1-est.val2)/input$n2)
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
        
        geom_label(aes(x = estimate, y = 0), 
                   label = TeX(paste0('$', calc, round(estimate, 2), '$')), size = 6)+
        xlab('Estimate')+
        ylab('Probability Density')+
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14))

    }else{
      #browser()
      idx = which(c(est.val1, est.val2) == min(est.val1, est.val2))
      if(idx == 1){
        LB = est.val1 - 4*est.sd1
        UP = est.val2 + 4*est.sd2
        added = est.val1+abs(est.val1 - est.val2)/2
      }else{
        LB = est.val2 - 4*est.sd2
        UP = est.val1 + 4*est.sd1
        added = est.val2+(est.val1 - est.val2)/2
      }
      arr.height = min(dnorm(est.val1, est.val1, est.sd1)/2,
                       dnorm(est.val2, est.val2, est.sd2)/2)
      out = ggplot()+xlim(LB, UP)+theme_pander()+
        stat_function(fun = dnorm,
                      args = list(mean = est.val1, sd = est.sd1),
                      size = 1.5)+
        stat_function(fun = dnorm,
                      args = list(mean = est.val1, sd = est.sd1),
                      geom = 'area',
                      fill = 'red',
                      alpha = 0.5)+
        stat_function(fun = dnorm,
                      args = list(mean = est.val2, sd = est.sd2),
                      size = 1.5)+
        stat_function(fun = dnorm,
                      args = list(mean = est.val2, sd = est.sd2),
                      geom = 'area',
                      fill = 'blue',
                      alpha = 0.5)+
        
        geom_segment(aes(x = est.val1, y = arr.height, 
                         xend = est.val2, yend = arr.height), #yend = dnorm(input$xbar2, input$xbar2, input$s2)/2), 
                     size = 1, 
                     arrow = arrow(length = unit(0.4, "cm"), ends = 'both'))+
        
        geom_segment(aes(x = est.val1, y = 0, 
                         xend = est.val1, yend = dnorm(est.val1, est.val1, est.sd1)), 
                     size = 1, linetype = 'dotted')+
        
        geom_segment(aes(x = est.val2, y = 0, 
                       xend =est.val2, yend = dnorm(est.val2, est.val2, est.sd2)), 
                   size = 1, linetype = 'dotted')+
        
        geom_label(aes(x = est.val1, y = dnorm(est.val1, est.val1, est.sd1)+0.01),
                   label = 'Population 1', size = 6)+
        
        geom_label(aes(x = est.val2, y = dnorm(est.val2, est.val2, est.sd2)+0.01),
                   label = 'Population 2', size = 6)+
        
        geom_label(aes(x = est.val1, y = 0),
                   label = TeX(est1), size = 6)+
        
        geom_label(aes(x = est.val2, y = 0),
                   label = TeX(est2), size = 6)+
        
        geom_label(aes(x = added, y = arr.height+lbl.height),
                   label = TeX(paste0('$', calc, round(est.val1 - est.val2, 3), '$')),
                   size = 5)+
        xlab('')+
        ylab('Density')+
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14))
    }
    
    plot(out)

  }))

  observeEvent(input$run,  output$summary <- renderPrint({
    #browser()
    if(input$test == 'confidence.interval'){
      if(input$param == 'two.means'){

        ot = two.sample.t.CI(x1 = input$xbar1,
                        x2 = input$xbar2,
                        s1 = input$s1,
                        s2 = input$s2,
                        n1 = input$n1,
                        n2 = input$n2,
                        tail = 'two.tail',
                        alpha = input$alpha,
                        verbose = T)

      }else{
        ot = two.sample.prop.CI(p1 = input$x1/input$n1,
                           p2 = input$x2/input$n2,
                           n1 = input$n1,
                           n2 = input$n2,
                           tail = 'two.tail',
                           alpha = input$alpha,
                           verbose  = T)
      }
    }else{
      if(input$param == 'two.means'){
        two.sample.t.test(m0 = input$m0,
                          x1 = input$xbar1,
                          x2 = input$xbar2,
                          s1 = input$s1,
                          s2 = input$s2,
                          n1 = input$n1,
                          n2 = input$n2,
                          alpha = input$alpha,
                          test = input$test,
                          pooling = input$pooling,
                          verbose = T)

      }else{
        two.sample.prop.test(p0 = input$p0,
                             x1 = input$x1,
                             x2 = input$x2,
                             n1 = input$n1,
                             n2 = input$n2,
                             alpha = input$alpha,
                             test = input$test,
                             pooled = TRUE,
                             verbose = T)
      }
    }

  }))
}

# Run the application 
shinyApp(ui = ui, server = server)
