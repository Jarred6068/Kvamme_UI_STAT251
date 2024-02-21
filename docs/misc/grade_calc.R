

temp = read.csv('C:/Users/Bruin/Desktop/2024-02-08T1604_Grades-Statistical_Methods.csv')


library(ggpubr)
library(ggthemes)

xx = subset(temp, Exam.1 > 0)
k = ceiling(sqrt(dim(temp)[1]))
score = xx$Exam.1/50
score.grade = function(score){
  input = rep("A", length(score))
  input[score<0.5] = "F"
  input[which(score>=0.5 & score<0.625)] = 'D'
  input[which(score>=0.625 & score<0.75)] = 'C'
  input[which(score>=0.75 & score<0.875)] = 'B'
  input[which(score>=0.875)] = 'A'
  
  return(input)
}
xx$Letter.Grade = score.grade(score)


A = ggplot(data = xx, aes(x = Exam.1))+
  geom_boxplot(color = 'black', fill = 'lightblue')+
  theme_classic2()+
  scale_x_continuous(limits=c(20, 50), breaks = seq(20, 50, 5))+
  xlab('Exam 1 score (out of 50pts)')+
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

B = ggplot(data = xx, aes(x = Letter.Grade))+
  geom_bar(stat = 'count', color = 'black', fill = 'lightblue')+
  theme_classic2()+
  scale_y_continuous(limits = c(0,20))+
  xlab('Exam 1 Letter Grades')+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
  

print(c(min(xx$Exam.1), quantile(xx$Exam.1, 0.25, type =2), 
      median(xx$Exam.1), quantile(xx$Exam.1, 0.25, type =2), 
      max(xx$Exam.1)))
ggarrange(A,B, nrow=2)

