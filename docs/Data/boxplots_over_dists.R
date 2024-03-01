
path1 = 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/'
path2 = 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_2/'

library(ggpubr)
library(ggthemes)
library(latex2exp)

a=rbeta(100000, 2, 10)
b=rnorm(100000)
c=rbeta(100000, 10, 2)
d=c(rnorm(50000), rnorm(50000, 3))


iqra = IQR(a)

iqrb = IQR(b)
iqrc = IQR(c)
iqrd = IQR(d)


sr = ggplot()+
  geom_density(aes(x = a), fill = 'lightblue', size = 1)+
  geom_boxplot(aes(x = a, y = 2), size = 1, fill = 'salmon', outlier.fill = 'red', outlier.shape = 21)+
  geom_vline(xintercept = c(0, summary(a)[5]+(1.5*iqra)),
             linetype = 'dotted', color = 'green2', size = 1)+
  theme_base()+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab('Skew Right')
sym = ggplot()+
  geom_density(aes(x = b), fill = 'lightblue', size = 1)+
  geom_boxplot(aes(x = b, y = 0.1), size = 1, fill = 'salmon', outlier.fill = 'red', outlier.shape = 21,
               width = 0.1)+
  geom_vline(xintercept = c(summary(b)[2]-(1.5*iqrb), summary(b)[5]+(1.5*iqrb)), 
             linetype = 'dotted', color = 'green2', size = 1)+
  theme_base()+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab('Symmetric')

sl = ggplot()+
  geom_density(aes(x = c), fill = 'lightblue', size = 1)+
  geom_boxplot(aes(x = c, y = 2), size = 1, fill = 'salmon', outlier.fill = 'red', outlier.shape = 21)+
  geom_vline(xintercept = c(summary(c)[2]-(1.5*iqrc), 1),
             linetype = 'dotted', color = 'green2', size = 1)+
  theme_base()+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab('Skew Left')

bm = ggplot()+
  geom_density(aes(x = d), fill = 'lightblue', size = 1)+
  geom_boxplot(aes(x = d, y = 0.06), size = 1, fill = 'salmon', 
               outlier.fill = 'red', outlier.shape = 21, width = 0.05)+
  geom_vline(xintercept = c(summary(d)[2]-(1.5*iqrd)+0.95, summary(d)[5]+(1.5*iqrd))-0.25,
             linetype = 'dotted', color = 'green2', size = 1)+
  theme_base()+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab('Bimodal')

ggarrange(sym, sl, sr, bm, nrow = 2, ncol = 2)






fl = read.csv(paste0(path1, 'fl_student_survey.csv'))
ga = read.csv(paste0(path2, 'georgia_student_survey.csv'))
heights = read.csv(paste0(path2, 'heights.csv'))


women = heights$HEIGHT[heights$GENDER == 'Female']
men = heights$HEIGHT[heights$GENDER == 'Male']

men.x = unique(round(men))
men.F = summary(as.factor(round(men)))
men.RF = men.F/sum(men.F)
men.CRF = cumsum(men.RF)


women.x = unique(round(women))
women.F = summary(as.factor(round(women)))
women.RF = women.F/sum(women.F)
women.CRF = cumsum(women.RF)



crf.table = cbind.data.frame(Height = c(men.x, women.x), 
                             Gender = c(rep('Male', length(men.x)), rep('Female', length(women.x))),
                             CRF = c(men.CRF, women.CRF))


female.height.tablular = cbind.data.frame(Height = women.x, 
                                        `F(x)` = women.F,
                                        `RF(x)` = women.RF,
                                        `CRF(X)` = women.CRF)


A = ggplot(data = heights, aes(x = HEIGHT, fill = GENDER))+
  geom_boxplot(outlier.fill = 'red', outlier.shape = 21, size = 1, outlier.size = 3,
               width = 0.08)+
  theme_hc()+
  theme(legend.position = 'top')
plot(A)


ggplot(data = crf.table, aes(x = Height, y = CRF, fill = Gender, color = Gender))+geom_step()+
  geom_point(shape = 21, size = 3)+
  theme_hc()+
  xlab('x')+
  ylab('Cumulative Relative Frequency')+
  #scale_x_continuous(breaks = seq(1, 6, 0.5))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank())



A=ggplot(data = heights, aes(x = HEIGHT, fill = GENDER))+
  geom_histogram(breaks = seq(56, 92, 1.9), color = 'black')+
  xlab('Height')+
  ylab('Frequency')+
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(heights$HEIGHT), max(heights$HEIGHT), 5))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        legend.position = 'none')
plot(A)

heights$zscore = (heights$HEIGHT - mean(heights$HEIGHT))/sd(heights$HEIGHT)

B=ggplot(data = heights, aes(x = zscore, fill = GENDER))+
  geom_histogram(breaks = seq(-3, 6.5, 0.5), color = 'black')+
  xlab('Z-score')+
  ylab('Frequency')+
  theme_minimal()+
  scale_x_continuous(breaks = seq(round(min(heights$zscore)), round(max(heights$zscore)), 1))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        legend.position = 'none')
plot(B)







x = sort(rnorm(1000000), decreasing = F)
probs <- c(0.025, 0.975)
quants <- quantile(x, prob=probs)
dens = density(x)
df = cbind.data.frame(x = dens$x, y = dens$y)
df$quant <- factor(findInterval(df$x,quants))
A = ggplot(aes(x = x, y = y), data = df)+
  geom_line(color = 'black', size = 1)+
  geom_ribbon(aes(ymin = 0, ymax = y, fill = quant), alpha = 0.5)+
  scale_x_continuous(breaks = seq(-4, 4, 1), labels = c("", 
                                                        TeX('$\\bar{x} - 3s$'),
                                                        TeX("$\\bar{x} - 2s$"), 
                                                        TeX("$\\bar{x} - 1s$"),
                                                        TeX('$\\bar{x}'),
                                                        TeX("$\\bar{x} + 1s$"), 
                                                        TeX("$\\bar{x} + 2s$"),
                                                        TeX("$\\bar{x} + 3s$"), 
                                                        ""))+
  scale_fill_manual(values = c('cyan','salmon','cyan'))+
  geom_vline(xintercept = 0, size = 1,
             color = 'black', linetype = 'dotted')+
  geom_vline(xintercept = c(2, -2), size =1,
             color = 'green2', linetype = 'dashed')+

  theme_minimal()+
  theme(axis.text.x = element_text(size = 14, angle = 45),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none')


probs <- c(0.975)
quants <- quantile(x, prob=probs)
dens = density(x)
df = cbind.data.frame(x = dens$x, y = dens$y)
df$quant <- factor(findInterval(df$x,quants))
B1 = ggplot(aes(x = x, y = y), data = df)+
  geom_line(color = 'black', size = 1)+
  geom_ribbon(aes(ymin = 0, ymax = y, fill = quant), alpha = 0.5)+
  scale_x_continuous(breaks = seq(-4, 4, 1), labels = c("", 
                                                        TeX('$\\bar{x} - 3s$'),
                                                        TeX("$\\bar{x} - 2s$"), 
                                                        TeX("$\\bar{x} - 1s$"),
                                                        TeX('$\\bar{x}'),
                                                        TeX("$\\bar{x} + 1s$"), 
                                                        TeX("$\\bar{x} + 2s$"),
                                                        TeX("$\\bar{x} + 3s$"), 
                                                        ""))+
  scale_fill_manual(values = c('salmon','cyan'))+
  geom_vline(xintercept = 0, size = 1,
             color = 'black', linetype = 'dotted')+
  geom_vline(xintercept = 2, size =1,
             color = 'green2', linetype = 'dashed')+
  
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14, angle = 45),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none')




probs <- c(0.025)
quants <- quantile(x, prob=probs)
dens = density(x)
df = cbind.data.frame(x = dens$x, y = dens$y)
df$quant <- factor(findInterval(df$x,quants))
B2 = ggplot(aes(x = x, y = y), data = df)+
  geom_line(color = 'black', size = 1)+
  geom_ribbon(aes(ymin = 0, ymax = y, fill = quant), alpha = 0.5)+
  scale_x_continuous(breaks = seq(-4, 4, 1), labels = c("", 
                                                        TeX('$\\bar{x} - 3s$'),
                                                        TeX("$\\bar{x} - 2s$"), 
                                                        TeX("$\\bar{x} - 1s$"),
                                                        TeX('$\\bar{x}'),
                                                        TeX("$\\bar{x} + 1s$"), 
                                                        TeX("$\\bar{x} + 2s$"),
                                                        TeX("$\\bar{x} + 3s$"), 
                                                        ""))+
  scale_fill_manual(values = c('cyan','salmon'))+
  geom_vline(xintercept = 0, size = 1,
             color = 'black', linetype = 'dotted')+
  geom_vline(xintercept = -2, size =1,
             color = 'green2', linetype = 'dashed')+
  
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14, angle = 45),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none')



ggarrange(A, ggarrange(B1, B2, nrow = 1, ncol = 2), nrow = 2,ncol = 1)
















#teacher salary margin of error:
teach = read.csv(paste0(path2, 'teacher_salary.csv'))

sample.means = list()

teach$indicator = rep(1, dim(teach)[1])
teach$indicator[sample(c(1:dim(teach)[1]), 10)] = 2
teach$indicator = as.factor(teach$indicator)

samp = teach$salary[which(teach$indicator == 2)]

ggplot(data=teach, aes(x = salary))+
  geom_histogram(color = 'black', fill = 'lightgrey')+
  geom_dotplot(fill = teach$indicator)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15))+
  xlab('Teacher Salary In Dollars')+
  ylab('Frequency')



ggplot(data=teach, aes(x = salary))+
  geom_histogram(color = 'black', fill = 'lightgrey')+
  geom_point(aes(x = mean(salary), y = 0), shape = 24, size = 4, fill = 'green2')+
  geom_vline(xintercept = mean(teach$salary), color = 'green2', linetype = 'dotted', size = 1.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15))+
  xlab('Teacher Salary In Dollars')+
  ylab('Frequency')+
  ggtitle("Population Distribution of Teacher Salaries")


ggplot(data=teach, aes(x = salary))+
  geom_histogram(color = 'black', fill = 'lightgrey')+
  geom_point(aes(x = mean(salary) , y = 0), shape = 24, size = 4, fill = 'green2')+
  geom_point(aes(x = mean(samp) , y = 0), shape = 24, size = 4, fill = 'red')+
  geom_vline(xintercept = mean(teach$salary), 
             color = 'green2', linetype = 'dotted', size = 1.5)+
  geom_vline(xintercept = mean(samp), 
             color = 'red', linetype = 'dotted', size = 1.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15))+
  xlab('Teacher Salary In Dollars')+
  ylab('Frequency')


samp2 = teach$salary[sample(1:length(teach$salary), 10)]
ggplot(data=teach, aes(x = salary))+
  geom_histogram(color = 'black', fill = 'lightgrey')+
  geom_point(aes(x = mean(salary) , y = 0), shape = 24, size = 4, fill = 'green2')+
  geom_point(aes(x = mean(samp) , y = 0), shape = 24, size = 4, fill = 'red')+
  geom_point(aes(x = mean(samp2) , y = 0), shape = 24, size = 4, fill = 'red')+
  geom_vline(xintercept = mean(teach$salary), 
             color = 'green2', linetype = 'dotted', size = 1.5)+
  geom_vline(xintercept = mean(samp), 
             color = 'red', linetype = 'dotted', size = 1.5)+
  geom_vline(xintercept = mean(samp2), 
             color = 'red', linetype = 'dotted', size = 1.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15))+
  xlab('Teacher Salary In Dollars')+
  ylab('Frequency')
  

for(i in 1:1000){
  sample.means[[i]] = mean(sample(teach$salary, 10))
}


ggplot()+
  geom_histogram(aes(x = unlist(sample.means)), color = 'black', fill = 'lightgrey')+
  geom_point(aes(x = mean(teach$salary) , y = 0), shape = 24, size = 4, fill = 'green2')+
  geom_vline(xintercept = mean(teach$salary), 
             color = 'green2', linetype = 'dotted', size = 1.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15))+
  xlab('Mean Teacher Salary In Dollars')+
  ylab('Frequency')+
  ggtitle('Distribution of the Sample Mean')


