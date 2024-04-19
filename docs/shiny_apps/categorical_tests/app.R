#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(grid)
library(latex2exp)


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

# a function to conduct the chi-square goodness of fit test
chi.squared.GOF.test = function(observed.ct=NULL, expected.freq=NULL, expected.ct = NULL,  alpha = 0.05, 
                                verbose = TRUE){
  
  n = sum(observed.ct)
  if(is.null(expected.ct)){
    if(is.null(expected.freq)){
      stop('missing one of expected.freq or expected.ct')
    }else{
      expected.ct = n*expected.freq 
    }
  }else{
    expect.freq = expected.ct/n
  }
  df = length(observed.ct)-1
  chi.dist = (observed.ct - expected.ct)^2 / expected.ct
  chi.obs = sum(chi.dist)
  tabres = cbind.data.frame(Observed = observed.ct, Expected = round(expected.ct, 3), 
                            Distance = round(chi.dist, 3))
  row.names(tabres) = paste0('category ', 1:length(observed.ct))
  
  null.hyp = paste0(paste0('P(category ', 1:length(observed.ct), ') = '), round(expected.freq, 4))
  alt.hyp ='The probabilities are different than those stated in H0'
  
  crit = qchisq(1-alpha, df)
  pvalue = 1-pchisq(chi.obs, df)
  
  decision = ifelse(pvalue<alpha, 'reject H0', 'fail to reject H0')
  if(isTRUE(verbose)){
    print(noquote(paste0(paste0(rep('=', 20), collapse = ''), ' test results ', paste0(rep('=', 20), collapse = ''))))
    print(noquote(paste0('H0: ', null.hyp)))
    print(noquote(paste0('HA: ', alt.hyp)))
    print(noquote(paste0('Degrees of freedom = ', df)))
    print(paste0(rep('-', 54), collapse = ''))
    print(tabres)
    print(paste0(rep('-', 54), collapse = ''))
    print(noquote(paste0('test statistic = ', round(chi.obs, 4))))
    if(df == 1){
      print(noquote(paste0('Z-statistic = ', round(sqrt(chi.obs), 4))))
    }
    print(noquote(paste0('critical value = ', round(crit, 4))))
    print(noquote(paste0('Pvalue = ', round(pvalue, 4))))
    print(noquote(paste0('Decision: ', decision)))
    print(noquote(paste0(rep("=", 54), collapse = '')))
    
  }
}



# chi square test of independence and homogeneity
chi.squared.test = function(cont.table, var1.name = NULL, var2.name = NULL,  alpha = 0.05, 
                            test = c('homogeneity','independence'), verbose = TRUE,
                            return.stats = FALSE, ...){
  
  rowcats = row.names(cont.table)
  colcats = colnames(cont.table)
  r.t = rowSums(cont.table)
  c.t = colSums(cont.table)
  n = sum(cont.table)
  exp.cts = outer(r.t, c.t)/n
  ct.dists = (cont.table - exp.cts)^2/exp.cts
  chi.obs = sum(ct.dists)
  df = prod(dim(cont.table)-1)
  
  final.observed = cbind(rbind(cont.table, c.t), c(r.t, n))
  if(is.null(var1.name)){
    colnames(ct.dists) = paste0('Var A: category ', colcats)
    colnames(exp.cts) = paste0('Var A: category ', colcats)
    colnames(final.observed) = c(paste0('Var A: category ', colcats),'Row Total')
  }else{
    colnames(ct.dists) = paste0(var1.name, ': category ', colcats)
    colnames(exp.cts) = paste0(var1.name, ': category ', colcats)
    colnames(final.observed) = c(paste0(var1.name, ': category ', colcats),'Row Total')
  }
  
  if(is.null(var2.name)){
    row.names(exp.cts) = paste0('Var B: category ', rowcats)
    row.names(ct.dists) = paste0('Var B: category ', rowcats)
    row.names(final.observed) = c(paste0('Var B: category ', rowcats),'Column Total')
  }else{
    row.names(exp.cts) = paste0(var2.name, ': category ', rowcats)
    row.names(ct.dists) = paste0(var2.name, ': category ', rowcats)
    row.names(final.observed) = c(paste0(var2.name, ': category ', rowcats),'Column Total')
  }
  

  
  
  crit = qchisq(1-alpha, df)
  if(test == 'homogeneity'){
    null.hyp = 'The conditional distributions of the rows are homogeneous'
    alt.hyp = 'The conditional distributions of the rows are not homogeneous'
  }else{
    null.hyp = 'The row variable and column variable are independent'
    alt.hyp = 'The row variable and column variable are dependent'
  }
  pvalue = 1-pchisq(chi.obs, df)
  
  
  decision = ifelse(pvalue<alpha, 'reject H0', 'fail to reject H0')
  if(isTRUE(verbose)){
    print(noquote(paste0(paste0(rep('=', 20), collapse = ''), ' test results ', paste0(rep('=', 20), collapse = ''))))
    print(noquote(paste0('test type = ', test)))
    print(noquote(paste0('H0: ', null.hyp)))
    print(noquote(paste0('HA: ', alt.hyp)))
    print(noquote(paste0('Degrees of freedom = ', df)))
    print(noquote(paste0(rep('-', 54), collapse = '')))
    print('Observed Counts')
    print(round(final.observed, 4))
    print('Expected Counts: (R * C)/n')
    print(round(exp.cts, 4))
    print('Distances: (observed - expected)^2 / expected')
    print(round(ct.dists, 4))
    print(noquote(paste0(rep('-', 54), collapse = '')))
    print(noquote(paste0('test statistic = ', round(chi.obs, 4))))
    if(df == 1){
      print(noquote(paste0('Z-statistic = ', round(sqrt(chi.obs), 4))))
    }
    print(noquote(paste0('critical value = ', round(crit, 4))))
    print(noquote(paste0('Pvalue = ', round(pvalue, 4))))
    print(noquote(paste0('Decision: ', decision)))
    print(noquote(paste0(rep("=", 54), collapse = '')))
    
  }
  
  if(return.stats){
    return(list(final.obs = final.observed, expected.cts = exp.cts, residuals = ct.dists))
  }
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel('Non-Parametric Hypothesis Tests'),
  sidebarLayout(
    sidebarPanel(width = 4, 
                 #input select
                 selectInput(inputId = "which",
                             label = "Choose a test",
                             choices = c("goodness of fit", "chi-square"),
                             selected = "goodness of fit"),
                 
                 sliderInput(inputId = "alpha",
                             label = "Significance Level:",
                             min = 0,
                             max = 1,
                             step = 0.01,
                             value = 0.05),
                 
                 fileInput("myfileinput", 
                           "Please choose a csv File", 
                           multiple = FALSE, 
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 
                 selectInput('myselectinput1','Select Response Variable', ""),
                 
                 conditionalPanel(condition = "input.which == 'goodness of fit'",
                                  textInput(inputId = "expected_props", 
                                            label = "Enter k Comma Separated Expected Proportions H_0", 
                                            value = "",
                                            placeholder = "0.3,0.3,0.3,..."),
                                  textInput(inputId = "expected_cts", 
                                            label = "(optional) Enter Comma Separated Expected Counts H_0", 
                                            value = "",
                                            placeholder = '10,10,10,...')),
                 conditionalPanel(condition = "input.which == 'chi-square'",
                                  selectInput('myselectinput2','Select Explanatory Variable', ""),
                                  selectInput(inputId = "test",
                                              label = "Choose a test",
                                              choices = c("independence", "homogeneity"),
                                              selected = "independence")),
                 
                 
                 actionButton("run","Run Inference"),
    ),
    mainPanel(tableOutput('mytable'),
              tableOutput('contable'),
              plotOutput('modelplot'),
              verbatimTextOutput("summary"))
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
    
    #Store loaded data in reactive
    reactives$mydata <- read.csv(file = input$myfileinput$datapath)
    #browser()
    #Update select input
    updateSelectInput(session, inputId = 'myselectinput1', label = 'Select the response variable', choices  = colnames(reactives$mydata))
    updateSelectInput(session, inputId = 'myselectinput2', label = 'Select the explanatory variable', choices  = colnames(reactives$mydata))
    
  })
  
  #Data table
  output$mytable <- renderTable({ 
    
    head(reactives$mydata)
    
  })
  
  observeEvent(input$run, output$contable <- function(){
    
    #browser()
    if(input$which == 'goodness of fit'){
      idx1 = which(colnames(reactives$mydata) == input$myselectinput1)
      Y = as.factor(reactives$mydata[,idx1])
      df = cbind.data.frame(names(summary(Y)), summary(Y))
      kable(df, format = 'html', digits = 2, col.names = c('Category', 'Count'),
            caption = 'Summary of Observed Counts',
            row.names = F, booktabs = T, escape = F)%>%kable_styling(bootstrap_options = 'striped')
    }else if(input$which == 'chi-square'){
      #browser()
      idx1 = which(colnames(reactives$mydata) == input$myselectinput1)
      Y = as.factor(reactives$mydata[,idx1])
      idx2 = which(colnames(reactives$mydata) == input$myselectinput2)
      X = as.factor(reactives$mydata[,idx2])
      cts = as.numeric(table(X,Y))
      
      df = as.data.frame(matrix(cts, nrow = length(summary(X)), ncol = length(summary(Y)),
                                byrow = F))
      row.names(df) = names(summary(X))
      kable(df, format = 'html', digits = 2, col.names = names(summary(Y)),
            caption = 'Summary of Observed Counts',
            row.names = T, booktabs = T, escape = F)%>%kable_styling(bootstrap_options = 'striped')
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$run, output$modelplot <- renderPlot({
    
    
    #browser()
    if(input$which == 'goodness of fit'){
      idx1 = which(colnames(reactives$mydata) == input$myselectinput1)
      Y = as.factor(reactives$mydata[,idx1])
      df = cbind.data.frame(names(summary(Y)), summary(Y))
      k = length(summary(Y))
      
      expected_props = as.numeric(strsplit(input$expected_props, ',')[[1]])
      if(length(expected_props) != 0){
        if(length(expected_props) != k ){
          showNotification(stop(paste0('Expected ', k, 
                                       ' proportions - user supplied ', 
                                       length(expected_props))),
                           type = 'error')
        }
      }else{
        expected_props = NULL
      }
      expected_cts = as.numeric(strsplit(input$expected_cts, ',')[[1]])
      if(length(expected_cts) != 0){
        if(length(expected_cts) != k ){
          showNotification(stop(paste0('Expected ', k, 
                                       ' counts - user supplied ', 
                                       length(expected_cts))),
                           type = 'error')
        }
      }else{
        expected_cts = NULL
      }
      
      if(is.null(expected_cts)){
        if(!is.null(expected_props)){
          expected_cts = length(Y)*expected_props
        }else{
          showNotification(stop('must enter wither expected proportions or expected counts under H_0'),
                           duration = 8,
                           type = 'error')
        }
      }
      
      df.cts = cbind.data.frame(Count = c(summary(Y),expected_cts),
                                Category = as.factor(rep(names(summary(Y)), 2)),
                                Model = as.factor(c(rep('Observed',k), rep('Expected',k))))
      
      fp = ggplot(data = df.cts)+theme_pander()+
        geom_bar(aes(x = Category, y = Count, fill = Model), 
                 stat = 'identity', color = 'black', position = position_dodge(),
                 alpha = 0.5)+
        #scale_fill_manual('darkviolet','lavender')+
        scale_fill_brewer(palette="Paired")+
        xlab(input$myselectinput1)+
        ylab('Count')+
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14))
      
    }else if(input$which == 'chi-square'){
      #browser()
      idx1 = which(colnames(reactives$mydata) == input$myselectinput1)
      Y = as.factor(reactives$mydata[,idx1])
      idx2 = which(colnames(reactives$mydata) == input$myselectinput2)
      X = as.factor(reactives$mydata[,idx2])
      cts = as.numeric(table(X,Y))
      xcats = names(summary(X))
      ycats = names(summary(Y))
      r = length(xcats)
      c = length(ycats)
      
      df = as.data.frame(matrix(cts, nrow = length(summary(X)), ncol = length(summary(Y)),
                                byrow = F))
      row.names(df) = xcats
      colnames(df) = ycats
      
      labs = expand.grid(Category1 = xcats, Category2 = ycats)
      
      #debug(chi.squared.test)
      output = chi.squared.test(cont.table = df,
                       var1.name = input$myselectinput1,
                       var2.name = input$myselectinput2,
                       alpha = input$alpha,
                       test = input$test,
                       verbose = T,
                       return.stats = T)
      
      plotdata.lf = cbind.data.frame(labs,
                                     Model = as.factor(c(rep('Observed', r*c),rep('Expected', r*c))),
                                     Frequency = c(as.numeric(cts), 
                                                   as.numeric(output$expected.cts)))
      
      plotdata.lf$Category1 = as.factor(plotdata.lf$Category1)
      plotdata.lf$Category2 = as.factor(plotdata.lf$Category2)
      plist = list()
      pals = rep(c('Reds','Greens','Blues','Purples','Oranges','Greys'), r)
      for(i in 1:r){
        currentcat = levels(plotdata.lf$Category1)[i]
        subdata = subset(plotdata.lf, Category1 == currentcat)
        plist[[i]] = ggplot(data = subdata)+theme_pander()+
          geom_bar(aes(x = Category2, y = Frequency, fill = Model), 
                   stat = 'identity', color = 'black', position = position_dodge2(),
                   alpha = 0.8)+
          #scale_fill_manual('darkviolet','lavender')+
          scale_fill_brewer(palette=pals[i])+
          xlab('')+
          ylab('')+
          ggtitle(paste0(input$myselectinput2, ":", currentcat))+
          theme(axis.text = element_text(size = 12),
                axis.title = element_text(size = 14))
        
      }
      
      if((r-1) > 1){
        xp = ggarrange(plotlist = plist, nrow = r-1, ncol = 2)
      }else{
        xp = ggarrange(plotlist = plist, nrow = 1, ncol = r)
      }
      
      fp = annotate_figure(xp, 
                           left = textGrob('Count', rot = 90, vjust = 1, gp = gpar(cex = 1)),
                           bottom = textGrob(input$myselectinput1, gp = gpar(cex = 1)))
    }
    
    
    plot(fp)
    
    
  }))
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$run,  output$summary <- renderPrint({
    
    #browser()
    if(input$which == 'goodness of fit'){
      
      idx1 = which(colnames(reactives$mydata) == input$myselectinput1)
      Y = as.factor(reactives$mydata[,idx1])
      df = cbind.data.frame(names(summary(Y)), summary(Y))
      k = length(summary(Y))
      
      expected_props = as.numeric(strsplit(input$expected_props, ',')[[1]])
      if(length(expected_props) != 0){
        if(length(expected_props) != k ){
          showNotification(stop(paste0('Expected ', k, 
                                       ' proportions - user supplied ', 
                                       length(expected_props))),
                           type = 'error')
        }
      }else{
        expected_props = NULL
        showNotification(warning('No expected proportions were entered'),
                         duration = 8,
                         type = 'warning')
      }
      expected_cts = as.numeric(strsplit(input$expected_cts, ',')[[1]])
      if(length(expected_cts) != 0){
        if(length(expected_cts) != k ){
          showNotification(stop(paste0('Expected ', k, 
                                       ' counts - user supplied ', 
                                       length(expected_cts))),
                           type = 'error')
        }
      }else{
        expected_cts = NULL
        showNotification(warning('No expected counts were entered'),
                         duration = 8,
                         type = 'warning')
      }
      
      
      chi.squared.GOF.test(observed.ct = summary(Y),
                           expected.ct = expected_cts,
                           expected.freq = expected_props,
                           alpha = input$alpha,
                           verbose = T)
      
      
    }else if(input$which == 'chi-square'){
      idx1 = which(colnames(reactives$mydata) == input$myselectinput1)
      Y = as.factor(reactives$mydata[,idx1])
      idx2 = which(colnames(reactives$mydata) == input$myselectinput2)
      X = as.factor(reactives$mydata[,idx2])
      cts = as.numeric(table(X,Y))
      
      df = as.data.frame(matrix(cts, nrow = length(summary(X)), ncol = length(summary(Y)),
                                byrow = F))
      row.names(df) = paste0(input$myselectinput2, names(summary(X)))
      colnames(df) = paste0(input$myselectinput1, names(summary(Y)))
      
      chi.squared.test(cont.table = df,
                       var1.name = input$myselectinput1,
                       var2.name = input$myselectinput2,
                       alpha = input$alpha,
                       test = input$test,
                       verbose = T)
    }
    
  }))
}

# Run the application 
shinyApp(ui = ui, server = server)
