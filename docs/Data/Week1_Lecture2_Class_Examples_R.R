
#read in plotting library ggplot2
library(ggplot2)
library(ggthemes)
#Set path to data file
pathname = 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/heights.csv'

heights = read.csv(pathname)
heights$GENDER = as.factor(heights$GENDER)

#lets first look at the frequency table:
#frequency table for height:
freq.table.height = as.data.frame(table(heights$HEIGHT))
RF.table.height = as.data.frame(table(heights$HEIGHT)/sum(table(heights$HEIGHT)))
print(freq.table.height)
print(RF.table.height)
#since the data is continuous its better to select intervals to compute the frequency
x = hist(heights$HEIGHT, breaks = 10)
height.table.binned = cbind.data.frame(binpoint = c('55 - 60', '60 - 65',
                                                    '65 - 70', '70 - 75', '75 - 80',
                                                    '80 - 85', '85 - 90', '90 - 95'),
                                       Frequency = x$counts,
                                       Relative.Frequency = x$counts/sum(x$counts),
                                       Cum.Relative.Frequency = cumsum(x$counts/sum(x$counts)))
print(height.table.binned)

#print a descriptive summary of the variables
summary(heights)
#Heights of students at a public school
#---------------------------------------------------------------
#Stem plot
stem(heights$HEIGHT)

#Dot plot
ggplot(data = heights, aes(x = HEIGHT))+
  geom_dotplot(dotsize = 0.5)+
  theme_classic()+
  ylab("")+
  xlab("Height in Inches")+
  theme_base()+
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 18), axis.title.y = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank())

#plot histogram
ggplot(data = heights, aes(x = HEIGHT))+
  theme_base()+
  geom_histogram(bins = 10, color = 'black', fill = 'lightgrey')+
  xlab('Height In Inches')+
  ylab('Frequency')

#plot a histogram for each gender
ggplot(data = heights, aes(x = HEIGHT, fill = GENDER))+
  theme_base()+
  geom_histogram(bins = 15, color = 'black', position = position_dodge())+
  xlab('Height In Inches')+
  ylab('Frequency')

#print a discriptive summary withing each gender
girls = subset(heights, GENDER == "Female")
boys = subset(heights, GENDER == "Male")
#summary for boys
summary(boys$HEIGHT)
#summary for girls
summary(girls$HEIGHT)

#girls look to be about 5 inches shorter on average

#compute the frequency of boys and girls in the school
summarized = summary(as.factor(heights$GENDER))
#reformat into new data table
gender.prop = cbind.data.frame(gender = names(summarized), proportion = summarized/sum(summarized))

#pie chart of geneder
pie1 = ggplot(gender.prop, aes(x = "", y = proportion, fill = gender)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste(round(proportion, 3)*100, '%')), 
            position = position_stack(vjust = 0.5), size = 6) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = 'Reds') +
  theme_void()+
  theme(legend.text = element_text(size = 12))
plot(pie1)
