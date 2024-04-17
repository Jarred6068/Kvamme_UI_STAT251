
library(ggplot2)
library(ggpubr)
library(latex2exp)
set.seed(222)
phat = NULL
SE = NULL
n = 100
alpha = 0.05
standard.score = qnorm(1-alpha/2)
covers.param = rep('True', 100)
for(i in 1:100){
  data = as.factor(sample(c('H','T'), n, replace = T))
  phat[i] = summary(data)[1]/n
}

SE = sqrt(phat*(1-phat)/n)

LB = phat - standard.score*SE
UP = phat + standard.score*SE
p = 0.5
covers.param[which(LB > 0.5 | UP < 0.5)] = 'False'

df = cbind.data.frame(x = c(1:100),
                      estimate = phat, 
                      UB = UP,
                      LB = LB,
                      `Confidence Interval Covers Parameter` = covers.param)

ggplot(df, aes(x = x, y = estimate, color = `Confidence Interval Covers Parameter`,
               fill = `Confidence Interval Covers Parameter`))+
  geom_hline(yintercept = p, linetype = 'dashed', linewidth = 2, color = 'black')+
  geom_point(shape = 19)+
  geom_errorbar(aes(ymin = LB, ymax = UP), linewidth = 1)+
  geom_text(aes(x = 0, y = p+0.01), label = TeX('$p$'), size = 6, color = 'black')+
  theme_classic2()+
  scale_x_continuous(breaks = seq(0, 100, 5))+
  xlab('Sample number')+
  ylab(TeX('$\\hat{p}$'))+
  theme(legend.position = 'top',
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))
