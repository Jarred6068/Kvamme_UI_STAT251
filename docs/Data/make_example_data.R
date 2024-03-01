
library(ggplot2)
library(ggthemes)
set.seed(123)
N = 100
p = 0.65
n = 20
`Identification Number` = paste("FN", sample(c(2000:2999), n), sep = "-")
`Duty Posting` = sample(c("Death Star", "Fondor Ship Yard", "Corellia", "Berchest Station", "Lothal"), n, 
                        replace = T)
`Height (cm)` = round(rnorm(n, mean = 190.5, sd = 3.3),1)
Age = round(rnorm(n, mean = 21, sd = 2))
`Blaster Accuracy` = rbinom(n, size = N, p = p)/N
Rank = sample(c('PV1', 'PV2', "PFC", "CPL"), prob = c(0.4, 0.3, 0.2, 0.1))
st.data = cbind.data.frame(`Identification Number`, `Duty Posting`, `Height (cm)`, Age, `Blaster Accuracy`, Rank)
#write.csv(st.data, 
#          file = "C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/STnames.csv", quote = F)

library(HSAUR2)
skull.data = rbind.data.frame(skulls[1:15,], skulls[145:150,])
#write.csv(skull.data, 
#          file = "C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/Skulldata.csv", quote = F)


ggplot(skulls, aes(x = bl, fill = factor(epoch))) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all", dotsize = 0.5)+
  theme_classic()+
  ylab("")+
  xlab("basilveolar length")+
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 18), axis.title.y = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'top')+
  labs(fill = 'Number of Cylinders')



X = runif(500)
Y = runif(500)
ex = cbind.data.frame(X,Y)

coords1 = seq(0.1, 1, 0.1)
ymax = sample(coords1, 10)
xmax = sample(coords1, 10)
A = ggplot(data = ex)+
  geom_rect(xmin=xmax[1]-0.1, xmax=xmax[1], ymin = ymax[1]-0.1, ymax = ymax[1], fill = 'lightgrey')+
  geom_rect(xmin=xmax[2]-0.1, xmax=xmax[2], ymin = ymax[2]-0.1, ymax = ymax[2], fill = 'lightgrey')+
  geom_rect(xmin=xmax[3]-0.1, xmax=xmax[3], ymin = ymax[3]-0.1, ymax = ymax[3], fill = 'lightgrey')+
  geom_rect(xmin=xmax[4]-0.1, xmax=xmax[4], ymin = ymax[4]-0.1, ymax = ymax[4], fill = 'lightgrey')+
  geom_rect(xmin=xmax[5]-0.1, xmax=xmax[5], ymin = ymax[5]-0.1, ymax = ymax[5], fill = 'lightgrey')+
  geom_rect(xmin=xmax[6]-0.1, xmax=xmax[6], ymin = ymax[6]-0.1, ymax = ymax[6], fill = 'lightgrey')+
  geom_rect(xmin=xmax[7]-0.1, xmax=xmax[7], ymin = ymax[7]-0.1, ymax = ymax[7], fill = 'lightgrey')+
  geom_rect(xmin=xmax[8]-0.1, xmax=xmax[8], ymin = ymax[8]-0.1, ymax = ymax[8], fill = 'lightgrey')+
  geom_rect(xmin=xmax[9]-0.1, xmax=xmax[9], ymin = ymax[9]-0.1, ymax = ymax[9], fill = 'lightgrey')+
  geom_rect(xmin=xmax[10]-0.1, xmax=xmax[10], ymin = ymax[10]-0.1, ymax = ymax[10], fill = 'lightgrey')+
  geom_point(aes(X, Y), size = 0.8)+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 1, 0.1))+
  theme_void()+
  geom_vline(xintercept = seq(0,1, 0.1), linetype = 'dotted')+
  geom_hline(yintercept = seq(0,1, 0.1), linetype = 'dashed')+

  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlab(element_blank())+
  ylab(element_blank())
plot(A)

pdf(file = 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/gridsample.pdf', height = 10, width = 10)
plot(A)
dev.off()


library(dplyr)

datnew = cbind.data.frame(`Duty Posting` = names(summary(as.factor(st.data$`Duty Posting`))), 
                          value = summary(as.factor(st.data$`Duty Posting`)))

data <- datnew %>% 
  arrange(desc(`Duty Posting`)) %>%
  mutate(prop = datnew$value / sum(datnew$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
pie = ggplot(data, aes(x="", y=prop, fill=`Duty Posting`)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
library(scales)

pdf(file = 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/duty_posting_piechart.pdf', height = 10, width = 10)
pie + scale_fill_brewer("Blues")  +  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = prop/2 + c(0, cumsum(prop)[-length(prop)]), 
                label = percent(prop/100)), size=5)+
  guides(fill=guide_legend(title="Duty Posting"))
dev.off()





#student attention likert responses
set.seed(123)
teen.data = cbind.data.frame(Response = factor(c("Often", "Sometimes", "Rarely", 'Never'),
                                               levels = c('Never', "Rarely", "Sometimes", "Often")),
                             `Proportion of Teens`= c(0.08,0.24,0.29,0.39))

P1 = ggplot(data = teen.data, aes(x = Response, y = `Proportion of Teens`, fill = Response))+
  geom_bar(stat = 'identity', color = 'black', size = 0.8)+
  theme_hc()+
  scale_y_continuous(breaks = seq(0, 0.5, 0.1))+
  coord_flip()

pdf(file = 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/distracted_teens_pie.pdf', height = 10, width = 12)
pie1 = ggplot(teen.data, aes(x = "", y = `Proportion of Teens`, fill = Response)) +
  geom_col(color = "black") +
  geom_text(aes(label = `Proportion of Teens`), 
            position = position_stack(vjust = 0.5), size = 14) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = 'Reds') +
  theme_void()+
  theme(legend.text = element_text(size = 22))
plot(pie1)
dev.off()

pdf(file = 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/distracted_teens.pdf', height = 8, width = 12)
plot(P1+scale_fill_brewer(palette = 'Reds')+theme(legend.position = 'none')+
       theme(axis.text.x = element_text(size = 15))+
       theme(axis.text.y = element_text(size = 15, angle = 45))+
       theme(axis.title.x = element_text(size = 18))+
       theme(axis.title.y = element_text(size = 18))+
       ggtitle("Are You Losing Focus In Class By Checking Your Cell Phone?"))
dev.off()

P2 = ggplot(data = teen.data, aes(x = Response, y = `Proportion of Teens`, fill = Response))+
  geom_bar(stat = 'identity', color = 'black', size = 0.8)+
  theme_hc()+
  scale_y_continuous(breaks = seq(0, 0.5, 0.1))+
pdf(file = 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/distracted_teens2.pdf', height = 8, width = 12)
plot(P2+scale_fill_brewer(palette = 'Reds')+theme(legend.position = 'none')+
       ylab('Relative Frequency')+
       theme(axis.text.x = element_text(size = 15))+
       theme(axis.text.y = element_text(size = 15, angle = 45))+
       theme(axis.title.x = element_text(size = 18))+
       theme(axis.title.y = element_text(size = 18))+
       ggtitle("Are You Losing Focus In Class By Checking Your Cell Phone?"))
dev.off()






#dice distribution
roll.die = cbind.data.frame(trial = c(1:100), 
                         roll =  sample(c(1:6), 100, prob = c(0.19, 0.18, 0.16, 0.13, 0.2, 0.14), replace = T))
fq = summary(as.factor(roll.die$roll))

dd = cbind.data.frame(Roll = c(1:6), Frequency = fq,
                      `Relative Frequency` = fq/sum(fq), 
                      `Cumulative Relative Frequency` = cumsum(fq/sum(fq)))
write.csv(dd, 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/Dice_Dist.csv', row.names = F)

P2 = ggplot(data = roll.die, aes(x = roll))+
  geom_histogram(binwidth = 1, fill = 'lightblue', color = 'black')+
  scale_y_continuous(breaks = seq(1, max(dd$Frequency), 1))+
  scale_x_continuous(breaks = c(1:6))+
  ylab('Frequency')+
  ggtitle('Histogram: 50 rolls of a 6-sided Die')+
  theme_hc()
plot(P2)
pdf(file = 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/Die_Dist_Hist.pdf', height = 8, width = 12)
plot(P2+theme(legend.position = 'none')+
       theme(axis.text.x = element_text(size = 15))+
       theme(axis.text.y = element_text(size = 15))+
       theme(axis.title.x = element_text(size = 18))+
       theme(axis.title.y = element_text(size = 18)))
dev.off()


#mendels peas
library(multibridge)
pea.data = peas
colnames(pea.data) = c('Type', 'Frequency')
pea.data$Relative.Frequency = pea.data$Frequency/sum(pea.data$Frequency)
pea.data.raw = cbind.data.frame(`Pea Color` = c(rep("Yellow", 416), rep("Green", 140)))



#old faithful waiting times
write.csv(faithful, file =  'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/oldfaithful.csv',
          row.names = F)
pdf(file = 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/Old_Faithful_dotplot.pdf', 
    height = 8, width = 12)
ggplot(data = faithful, aes(x = waiting))+
  geom_dotplot(dotsize = 0.7)+
  theme_classic()+
  ylab("")+
  xlab("Waiting Time Until Erapution (Min)")+
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 18), axis.title.y = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank())
dev.off()


#Cars data 
mtcars$Model = row.names(mtcars)
write.csv(mtcars, file =  'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/carsdata.csv',
          row.names = F)
pdf(file = 'C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/cars.pdf', 
    height = 8, width = 12)
ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")+
  theme_classic()+
  ylab("")+
  xlab("Miles Per Gallon (MPG)")+
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 18), axis.title.y = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'top')+
  labs(fill = 'Number of Cylinders')
dev.off()


#distribution shapes
sr = ggplot()+
  geom_density(aes(x = rbeta(100000, 2, 10)), fill = 'lightblue', size = 1)+
  theme_base()+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab('Skew Right')
sym = ggplot()+
  geom_density(aes(x = rnorm(100000)), fill = 'lightblue', size = 1)+
  theme_base()+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab('Symmetric')

sl = ggplot()+
  geom_density(aes(x = rbeta(100000, 10, 2)), fill = 'lightblue', size = 1)+
  theme_base()+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab('Skew Left')

bm = ggplot()+
  geom_density(aes(x = c(rnorm(50000), rnorm(50000, 3))), fill = 'lightblue', size = 1)+
  theme_base()+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab('Bimodal')

ggarrange(sym, sl, sr, bm, nrow = 2, ncol = 2)


#faithful density plot
ggplot(data = faithful, aes(x = waiting))+
  geom_histogram(bins = 15, color = 'black', fill = 'lightblue')+
  theme_hc()+
  xlab('Eruption Waiting Time (Mins)')+
  ylab('Frequency')






#Distribution Variability
nn = 100000
D = cbind.data.frame(Variability = c(rep('Low', nn),
                                      rep('Medium', nn),
                                      rep('High', nn)),
                     Value = c(rnorm(nn, sd = 1),
                               rnorm(nn, sd = 2),
                               rnorm(nn, sd = 5)))

sym.w = ggplot(data = D, aes(x = Value, fill = Variability))+
  geom_density(size = 1, alpha = 0.5)+
  theme_base()+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab('')
plot(sym.w)



a = round(rnorm(50, 5))
b = round(abs(runif(50, 7, 15)))

x = c(a, b, c(16,16,16,16,17,17,17,18,18,20,20,20,20,20))
hist(x)
axis(side=1,at=seq(0,20,1),labels=seq(0,20,1))

ggplot()+
  geom_histogram(aes(x =x), fill = 'grey', color = 'black', size = 1, alpha = 0.5,
                 bins = 10)+
  theme_void()+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  
  xlab('')






#Cereal sodium
cereal = read.csv('C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/cereal.csv')

#frequency table
freq = summary(as.factor(cereal$Sodium))
FT = cbind.data.frame(`Sodium Level (mg)`=names(freq), `Fr(x)` = freq,
                      `RF(x)` = freq/sum(freq), `CRF(x)` = cumsum(freq)/sum(freq))

write.csv(FT, file = "C:/Users/Bruin/Desktop/GS Academia/TA/Teaching STAT 251/FALL/Week_1/cereal_frequency_table.csv")

ggplot(data = cereal, aes(x = Sodium))+
  geom_histogram(binwidth = 50, color = 'black', fill = 'grey', center = 25)+
  scale_x_continuous(breaks = seq(0, 350, 50), limits = c(0, 350))+
  geom_dotplot()+geom_vline(xintercept = mean(cereal$Sodium),
                            linetype = 'solid', color = 'red',
                            size = 2)+theme_minimal()+
  ylab('Frequency')+
  xlab('Sodium (mg)')+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))
  


x = sort(rnorm(1000000), decreasing = F)
#normal quantiles
ggplot()+
  geom_density(aes(x = x), color = 'black', fill = 'lightblue', alpha = 0.5)+
  scale_x_continuous(breaks = seq(-4, 4, 1), labels = c("", 
                                                        TeX('$\\bar{x} - 3s$'),
                                                        TeX("$\\bar{x} - 2s$"), 
                                                        TeX("$\\bar{x} - 1s$"),
                                                        TeX('$\\bar{x}'),
                                                        TeX("$\\bar{x} + 1s$"), 
                                                        TeX("$\\bar{x} + 2s$"),
                                                        TeX("$\\bar{x} + 3s$"), 
                                                        ""))+
  geom_vline(xintercept = 0, size = 1,
             color = 'black', linetype = 'dotted')+
  geom_vline(xintercept = c(1, -1), size = 1,
             color = 'red', linetype = 'dashed')+
  geom_vline(xintercept = c(2, -2), size =1,
             color = 'blue', linetype = 'dashed')+
  geom_vline(xintercept = c(3, -3), size = 1,
             color = 'green', linetype = 'dashed')+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14, angle = 45),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())



#normal quantiles
x = rnorm(1000000)
probs <- c(0.25, 0.5, 0.75)
quants <- quantile(x, prob=probs)
dens = density(x)
df = cbind.data.frame(x = dens$x, y = dens$y)
df$quant <- factor(findInterval(df$x,quants))
ggplot(aes(x = x, y = y), data = df)+
  geom_line(color = 'black', size = 1)+
  geom_ribbon(aes(ymin = 0, ymax = y, fill = quant), alpha = 0.5)+
  scale_x_continuous(breaks = quants, labels = c(TeX('$Q1$'),
                                                 TeX("$Q2$"), 
                                                 TeX("$Q3$")),
                     limits = c(-3,3))+
  geom_vline(xintercept = quants[1], size = 1,
             color = 'red', linetype = 'dashed')+
  geom_vline(xintercept = quants[2], size = 1,
             color = 'darkgreen', linetype = 'dashed')+
  geom_vline(xintercept = quants[3], size =1,
             color = 'blue', linetype = 'dashed')+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14, angle = 45),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())




#normal cdf
#normal quantiles
x = rnorm(1000000)
probs <- c(0.25, 0.5, 0.75)
quants <- quantile(x, prob=probs)
dens = density(x)
df = cbind.data.frame(x = dens$x, y = cumsum(dens$y))
df$quant <- factor(findInterval(df$x,quants))
ggplot(aes(x = x, y = y), data = df)+
  geom_line(color = 'black', size = 1)+
  geom_ribbon(aes(ymin = 0, ymax = y, fill = quant), alpha = 0.5)+
  scale_x_continuous(breaks = quants, labels = c(TeX('$Q1$'),
                                                 TeX("$Q2$"), 
                                                 TeX("$Q3$")),
                     limits = c(-3,3))+
  geom_vline(xintercept = quants[1], size = 1,
             color = 'red', linetype = 'dashed')+
  geom_vline(xintercept = quants[2], size = 1,
             color = 'darkgreen', linetype = 'dashed')+
  geom_vline(xintercept = quants[3], size =1,
             color = 'blue', linetype = 'dashed')+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14, angle = 45),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())



x = c(1,2,3,4,5,6)
cdfx = c(0.1, 0.2, 0.4, 0.7, 0.8, 1)
ggplot()+geom_step(aes(x = x, y = cdfx), size = 0.8)+
  geom_point(aes(x = x, y = cdfx), size = 3)+
  theme_hc()+
  xlab('x')+
  ylab('Cumulative Relative Frequency ')+
  scale_x_continuous(breaks = c(1:6))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank())

#quartiles example as a CDF
#x = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 5, 5, 6, 6, 6, 6)
x = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 5, 5, 6, 6, 6)
xi = unique(x)
rfx = summary(as.factor(x))/sum(summary(as.factor(x)))
crfx = cumsum(rfx)
ggplot()+geom_step(aes(x = xi, y = crfx), size = 0.8)+
  geom_point(aes(x = xi, y = crfx), size = 3)+
  theme_hc()+
  xlab('x')+
  ylab('Cumulative Relative Frequency ')+
  scale_x_continuous(breaks = seq(1, 6, 0.5))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank())




#faithful cdf
#quartiles example as a CDF
xi = unique(sort(faithful$waiting))
rfx = summary(as.factor(sort(faithful$waiting)))/sum(summary(as.factor(sort(faithful$waiting))))
crfx = cumsum(rfx)
ggplot()+geom_step(aes(x = xi, y = crfx), size = 0.8)+
  geom_point(aes(x = xi, y = crfx), size = 3)+
  theme_hc()+
  xlab('x')+
  ylab('Cumulative Relative Frequency ')+
  scale_x_continuous(breaks = seq(1, 6, 0.5))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank())







#Simple Random Sample
df = as.data.frame(expand.grid(1:10,1:5, paste('stratum', 1:4)))
label = rep(0, dim(df)[1])
label[sample(1:dim(df)[1], 50)] = 1

df$label = as.factor(label)
colnames(df) = c('x',"y","stratum", 'label')

ggplot(data=df, aes(x=jitter(x, 1),y=jitter(y, 1), shape = label, color = label, fill = label))+
  geom_point(size = 5, stroke = 2)+
  facet_wrap(~stratum)+
  scale_shape_manual(values = c(21, 4))+
  theme_bw()+
  theme(axis.text.y   = element_blank(),
        axis.text.x   = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        legend.position = 'none')


#stratified random sample
df = as.data.frame(expand.grid(1:5,1:5,paste0))
label = rep(0, 50)
label[sample(1:50, 15)] = 1

df$label = as.factor(label)
colnames(df) = c('x',"y","label",'strata')















x = c(-1.5, -1.2, -1.0, -0.8, -0.7, -0.6, 
      -0.1, -0.1, 0.1, 0.1, 0.1, 0.6, 0.6, 
      0.8, 1.1, 1.2, 1.3, 1.8, 1.9, 2.4)
h = hist(x, 
     breaks = 4, 
     col = c('red','green','blue','yellow','orange'))














# Rocket League Example Warm up 

#Consider the following 
RL = read.table(paste0(datapath, 'RLData.txt'), sep = ',', header = T)

