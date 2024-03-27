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
library(ggthemes)

convert.keystroke = function(keystroke_input){
  indata = strsplit(keystroke_input, ',')[[1]]
  if(sum(suppressWarnings(is.na(as.numeric(indata))))>0){
    converted = as.character(indata)
    values = unique(converted)
    counts = summary(as.factor(converted))
    input = as.factor(converted)
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
    input = converted
  }
  ft = cbind.data.frame(X = values, frequency = counts)
  return(list(ft = ft, data = input))
}



calc.binwidth = function(method, data){
  n = length(data)
  if(method == 'simple'){
    k = round(sqrt(n))
    w = (max(data) - min(data))/k
  }else if(method == 'sturges'){
    k = round(log2(n))+1
    w = (max(data) - min(data))/k
  }else if(method == 'rice'){
    k = round(2*(n^(1/3)))
    w = (max(data) - min(data))/k
  }else{
    k = NULL
    w = NULL
  }
  
  if(is.null(w)){
    mes = ''
  }else{
    mes = paste0(k, " bins using method ", method)
  }
  
  return(list(mes, k, w))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel('Make Plots'),
  sidebarLayout(
    sidebarPanel(width = 4, 
                 #input select
                 textInput(inputId = "keystroke_input", 
                           label = "Enter Comma Separated Values (No spaces!)", 
                           value = "1,2,3, ... or cat, dog, dog, ..."),

                 selectInput(inputId = "plotting",
                             label = "Select plot options",
                             choices = c("standard", "custom"),
                             selected = 'standard'),
                
                 selectInput(inputId = "graph_option",
                             label = "Select graph",
                             choices = c('Histogram','Dotplot', 
                                         'Stem plot', 'Boxplot',
                                         'Barplot', 'Pie chart'),
                             selected = "Histogram"),
                 
                 conditionalPanel(condition = "input$plotting == 'custom'",
                                  selectInput(inputId = "bin",
                                              label = "bin method (histogram only)",
                                              choices = c('automatic', 'simple', 'sturges', 'rice'),
                                              selected = "automatic"),
                                  selectInput(inputId = "theme",
                                              label = "Select plot theme",
                                              choices = c('classic', 'HC', 'BW'),
                                              selected = "classic"),
                                  textInput(inputId = "xlabel_keystroke", 
                                            label = "x-axis label", 
                                            value = "X"),
                                  textInput(inputId = "ylabel_keystroke", 
                                            label = "y-axis label", 
                                            value = "Y"),
                                  textInput(inputId = "title_keystroke", 
                                            label = "plot title", 
                                            value = NULL),
                                  textInput(inputId = "fill", 
                                            label = "plot fill", 
                                            value = 'lightblue'),
                                  textInput(inputId = "dotsize", 
                                            label = "Dot size (dot plot only)", 
                                            value = '1')),
                 actionButton("Plot","Plot data")
                 
                 
    ),
    mainPanel("",
              plotOutput("plot"),
              verbatimTextOutput("text")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  observeEvent(input$Plot,  output$plot <- renderPlot({
    
    if(is.null(input$title_keystroke)){
      plot_title = paste(input$graph_option, ' of ', input$xlabel_keystroke)
    }else{
      plot_title = input$title_keystroke
    }
    #browser()
    out = convert.keystroke(input$keystroke_input)
    if(input$graph_option == 'Histogram'){
      pt = 'ggplot'
      kw = calc.binwidth(method = input$bin,
                         data = out$data)
      
      A = ggplot()+
        geom_histogram(aes(x = out$data), 
                       color = 'black', 
                       fill = input$fill,
                       binwidth = kw[[3]])+
        theme(axis.text = element_text(size = 12))+
        xlab(input$xlabel_keystroke)+
        ylab(input$ylabel_keystroke)+
        ggtitle(plot_title, subtitle = paste0(kw[[1]]))
    }else if(input$graph_option == 'Stem plot'){
      pt = 'ggplot'
      A = ggplot()+geom_blank()+theme(axis.line.y = element_blank())
    }else if(input$graph_option == 'Dotplot'){
      pt = 'ggplot'
      A = ggplot()+geom_dotplot(aes(x = out$data), 
                                color = 'black', 
                                fill = input$fill,
                                dotsize = as.numeric(input$dotsize))+
        theme(axis.text = element_text(size = 12))+
        xlab(input$xlabel_keystroke)+
        ylab(input$ylabel_keystroke)+
        ggtitle(plot_title)
    }else if(input$graph_option == 'Boxplot'){
      pt = 'ggplot'
      mini = round(min(out$data), 2)
      maxi = round(max(out$data), 2)
      med = round(median(out$data), 2)
      avg = round(mean(out$data), 2)
      q1 = round(as.numeric(quantile(out$data)[2]), 2)
      q3 = round(as.numeric(quantile(out$data)[4]), 2)
      interr = round(IQR(out$data),2)
      stdev = sd(out$data)
      upper.outliers.idx = which(out$data > q3+1.5*interr & out$data < maxi)
      lower.outliers.idx = which(out$data < q1-1.5*interr & out$data > mini)
      # boxplot(out$data, horizontal = TRUE, axes = FALSE, staplewex = 1,
      #         xlab = input$xlabel_keytroke, main = plot_title,
      #         col = input$fill)
      # text(x=estims, 
      #      labels = paste(c("Q1","Q2", "Q3"), round(estims,1), 
      #                            sep = "="), 
      #      y=1.25, cex = 1)
      # text(x=c(mini+(stdev/10), maxi - (stdev/10)), 
      #      labels = paste(c("Min", "Max"), round(c(mini, maxi),1), 
      #                     sep = "="), 
      #      y=1.30, cex = 1)
      # text(x=avg, 
      #      labels = paste(c("Mean"), round(avg,1), 
      #                     sep = "="), 
      #      y=1.35, cex = 1)
      # text(x=interr, labels =paste("IQR", round(interr,1), sep = " = "), y=1.5, cex = 1.25)
      #browser()
      A = ggplot()+geom_boxplot(aes(x = out$data), 
                                fill = 'lightgrey',
                                outlier.shape = 23,
                                outlier.fill = 'green',
                                outlier.size = 5)+
        geom_point(aes(x = c(mini, maxi), y = c(0, 0)), 
                   shape = 21, 
                   fill = 'red', 
                   size = 5)+
        geom_label(aes(x = mini, y = 0.05), label = paste('min =', mini, collapse = ''), 
                   label.size = 0.3,)+
        geom_label(aes(x = maxi, y = 0.05), label = paste('max =', maxi, collapse = ''),
                   label.size = 0.3,)+
        geom_label(aes(x = med, y = 0.45), label = paste('median =', med, collapse = ''),
                   label.size = 0.3,)+
        geom_label(aes(x = q1-(stdev/8), y = 0.405), label = paste('Q1 =', q1, collapse = ''),
                   label.size = 0.3,)+
        geom_label(aes(x = q3+(stdev/8), y = 0.405), label = paste('Q3 =', q3, collapse = ''),
                   label.size = 0.3,)+
        geom_label(aes(x = med, y = 0.495), label = paste('IQR =', interr, collapse = ''),
                   label.size = 0.3,)+
        scale_x_continuous(limits = c(plyr::round_any(mini-(stdev/2), 0.5), 
                                      plyr::round_any(maxi+(stdev/2), 0.5)),
                           n.breaks = 10)+
        theme_classic()+
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.title.y = element_blank(),
              axis.line.y = element_blank())+
        xlab(input$xlabel_keystroke)+
        ylab(input$ylabel_keystroke)+
        ggtitle(plot_title)
      
      if(sum(lower.outliers.idx) != 0){
        lower.outliers = out$data[lower.outliers.idx]
        A = A+geom_text(aes(x = lower.outliers, y = -0.05), 
                        label = paste(round(lower.outliers, 2)))
      }
      if(sum(upper.outliers.idx) != 0){
        upper.outliers = out$data[upper.outliers.idx]
        A = A+geom_text(aes(x = upper.outliers, y = -0.05), 
                        label = paste(round(upper.outliers,2)))
      }
      
    }else if(input$graph_option == 'Barplot'){
      pt = 'ggplot'
      A = ggplot()+
        geom_bar(aes(x = out$ft$X, y = out$ft$frequency),
                 stat = 'identity',
                 color = 'black',
                 fill = input$fill)+
        theme(axis.text = element_text(size = 12))+
        xlab(input$xlabel_keystroke)+
        ylab(input$ylabel_keystroke)+
        ggtitle(plot_title)
      
    }else if(input$graph_option == 'Pie chart'){
      pt = 'ggplot'
      out$ft$prop = round(out$ft$frequency/sum(out$ft$frequency),2)
      A = ggplot(out$ft, aes(x = "", y = prop, fill = X)) +
        geom_col(color = "black") +
        geom_text(aes(label = prop), 
                  position = position_stack(vjust = 0.5), size = 10) +
        coord_polar(theta = "y") +
        theme_void()+
        scale_fill_brewer(palette = 'Reds') +
        theme(legend.text = element_text(size = 12))
    }
    
    if (pt == 'ggplot' && input$graph_option != "Pie chart"){
      if(input$graph_option == 'Boxplot'){
        plot(A)
      }else{
        if(input$theme == 'classic'){
          P = A + theme_classic2()+theme(axis.title = element_text(size = 14))
        }else if (input$theme == 'HC'){
          P = A + theme_hc()+theme(axis.title = element_text(size = 14))
        }else{
          P = A + theme_bw()+theme(axis.title = element_text(size = 14))
        }
        plot(P)
      }
    }else{
      plot(A)  
    }
    
  }))
  
  observeEvent(input$Plot,  output$text <- renderPrint({
    if(input$graph_option == 'Stem plot'){
      out = convert.keystroke(input$keystroke_input)
      print('Stem Plot')
      stem(out$data)
    }
  }))
  
}

shinyApp(ui = ui, server = server)
