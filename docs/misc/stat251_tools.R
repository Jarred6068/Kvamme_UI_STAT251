## STAT 251 - helper functions for confidence intervals and significance tests.
##
## This file is the single source of truth for these functions: the lecture
## notes, homework solutions and exams source() it, and misc/hp_r_code.Rmd
## (Resources > R code > Functions) renders straight out of it via
## knitr::read_chunk(). Comment the code HERE and the website updates itself.
##
## The `## ---- name ----` lines are those read_chunk() markers. They are
## ordinary R comments and have no effect on source(); each one starts a new
## named block that the website can display. Keep one above every function.
##
## Usage on your own machine:  source('stat251_tools.R')

## ---- setup-libraries ----
# ggpubr/ggthemes are for the plot styling, latex2exp renders the Greek
# letters in the axis labels, plyr and VennDiagram are used by the lecture
# notes that source this file.



library(latex2exp)
library(gridExtra)
library(ggpubr)
library(ggthemes)
library(plyr)
library(VennDiagram)

## ---- rejection-region-helper ----
# Helper for gen.density.plot() - not meant to be called on its own.
# Returns the density at x, but NA everywhere OUTSIDE the rejection region.
# ggplot skips NA values, so the shaded area lands only on the tail(s).
d_limit <- function(x, alpha, test, dist, n) {
  test <- match.arg(test, c('two.tailed', 'upper.tail', 'lower.tail'))
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


## ---- density-plot ----
# Draws the distribution of the test statistic under H0, with the rejection
# region shaded and the critical value(s) and observed statistic labelled.
#   bbox     x-axis range to draw over
#   n        sample size - only used to get df = n - 1 when dist = 't'
#   dist     'z' for the standard normal, 't' for the t distribution
#   alpha    significance level, i.e. the area shaded
#   obs      the observed test statistic, marked on the axis
#   test     'two.tailed', 'lower.tail' or 'upper.tail'
#   shade    FALSE draws the curve with no shading
#   txt.sz   size of the axis labels
# Returns a ggplot object, so you can add to it or print it.
gen.density.plot=function(bbox = c(-4,4), n = 10, dist = c('z','t'), alpha = 0.05, obs = 0.5,
                          test = c('two.tailed','lower.tail', 'upper.tail'), shade = TRUE,
                          txt.sz = 5){
  test <- match.arg(test)
  dist <- match.arg(dist)

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








## ---- one-sample-prop-se ----
# Standard error of a sample proportion: SE = sqrt(phat(1-phat)/n).
one.sample.prop.SE = function(phat, n){
  SE = sqrt((phat*(1-phat))/n)
  return(SE)
}


## ---- one-sample-prop-ci ----
# Confidence interval for a population proportion.
#   phat     the sample proportion
#   n        the sample size
#   alpha    1 - alpha is the confidence level
#   verbose  TRUE prints the pieces of the calculation
# Returns c(lower.bound, upper.bound). Unlike the test above, the SE here is
# computed from phat - a confidence interval has no null value to lean on.
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



## ---- one-sample-prop-test ----
# One-sample z test for a population proportion.
#   p0       hypothesized proportion under H0
#   phat     the sample proportion
#   n        the sample size
#   alpha    significance level
#   test     'lower.tail', 'upper.tail' or 'two.tail'
#   verbose  TRUE prints the results
# The test statistic uses the SE computed from p0, not from phat, because
# the null hypothesis is assumed true while testing.
one.sample.prop.test = function(p0, phat, n, alpha = 0.05, test = c('lower.tail','upper.tail','two.tail'),
                                verbose = TRUE){
  test <- match.arg(test)
  
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












## ---- one-sample-t-se ----
# Standard error of a sample mean: SE = s / sqrt(n).
one.sample.t.SE = function(s, n){
  SE = s/sqrt(n)
  return(SE)
}


## ---- one-sample-t-ci ----
# Confidence interval for a population mean.
#   xbar     the sample mean
#   s        the sample standard deviation
#   n        the sample size
#   alpha    1 - alpha is the confidence level
#   verbose  TRUE prints the pieces of the calculation
# Returns c(lower.bound, upper.bound).
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


## ---- one-sample-t-test ----
# One-sample t test for a population mean.
#   m0       hypothesized mean under H0
#   xbar     the sample mean
#   s        the sample standard deviation
#   n        the sample size
#   alpha    significance level
#   test     'lower.tail', 'upper.tail' or 'two.tail'
#   verbose  TRUE prints the results
# Prints the results and returns nothing.
one.sample.t.test = function(m0, xbar, s, n, alpha = 0.05, test = c('lower.tail','upper.tail','two.tail'),
                             verbose = TRUE){
  test <- match.arg(test)
  
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




## ---- two-sample-prop-se ----
# Unpooled standard error of a difference between two sample proportions:
# SE = sqrt(p1(1-p1)/n1 + p2(1-p2)/n2).
two.sample.prop.SE = function(p1,p2,n1,n2){
  SE = sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
  return(SE)
}

## ---- two-sample-prop-ci ----
# Confidence interval for the difference between two population proportions.
#   p1, p2   the two sample proportions
#   n1, n2   the two sample sizes
#   tail     'two.tail' for a two-sided interval, anything else one-sided
#   alpha    1 - alpha is the confidence level
#   verbose  TRUE prints the pieces of the calculation
# Returns a list: estimate, interval, margin.of.error, critical.value.
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


## ---- two-sample-prop-test ----
# Two-sample z test for a difference between two population proportions.
#   p0       difference under H0 (usually 0)
#   x1, x2   the two SUCCESS COUNTS (not proportions)
#   n1, n2   the two sample sizes
#   alpha    significance level
#   test     'lower.tail', 'upper.tail' or 'two.tail'
#   pooled   TRUE uses the pooled SE, which is the right choice when H0 says
#            the two proportions are equal; FALSE uses the unpooled SE
#   verbose  TRUE prints the results
# Note the interval is always built from the UNPOOLED SE, since a confidence
# interval does not assume H0 is true.
two.sample.prop.test = function(p0,x1,x2,n1,n2,alpha = 0.05, test = c('lower.tail','upper.tail','two.tail'),
                                pooled = TRUE, verbose = TRUE){
  test <- match.arg(test)
  
  
  p1 = x1/n1
  p2 = x2/n2
  estimate = p1 - p2
  estimate.SE = two.sample.prop.SE(p1, p2, n1, n2)
  if(isTRUE(pooled)){
    # H0 says the two proportions are equal, so under H0 there is really only
    # ONE proportion to estimate - pool both samples into a single estimate.
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


## ---- two-sample-t-se ----
# Unpooled standard error of a difference between two sample means:
# SE = sqrt(s1^2/n1 + s2^2/n2). Variances add even though we subtract means.
two.sample.t.SE = function(s1,s2,n1,n2){
  SE = sqrt((s1^2/n1)+(s2^2/n2))
  return(SE)
}

## ---- two-sample-t-ci ----
# Confidence interval for the difference between two population means.
#   x1, x2   the two sample means
#   s1, s2   the two sample standard deviations
#   n1, n2   the two sample sizes
#   tail     'two.tail' for a two-sided interval, anything else one-sided
#   alpha    1 - alpha is the confidence level
#   verbose  TRUE prints the pieces of the calculation
# Returns a list: estimate, interval, margin.of.error, critical.value.
# Uses the conservative df = min(n1, n2) - 1 rather than Welch's formula.
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


## ---- two-sample-t-test ----
# Two-sample t test for a difference between two population means.
#   m0       difference under H0 (usually 0)
#   x1, x2   the two sample means
#   s1, s2   the two sample standard deviations
#   n1, n2   the two sample sizes
#   alpha    significance level
#   test     'lower.tail', 'upper.tail' or 'two.tail'
#   pooling  'pooled' (assume equal variances), 'unpooled' (Welch), or
#            'approx.unpooled' (Welch SE with the easy df = min(n1,n2) - 1)
#   verbose  TRUE prints the results
# Prints the results and returns nothing.
two.sample.t.test = function(m0,x1,x2,s1,s2,n1,n2,alpha = 0.05, test = c('lower.tail','upper.tail','two.tail'),
                             pooling = c('pooled', 'unpooled','approx.unpooled'), verbose = TRUE){
  test <- match.arg(test)
  pooling <- match.arg(pooling)
  
  estimate = x1 - x2
  estimate.SE = two.sample.t.SE(s1, s2, n1, n2)
  if(pooling == 'pooled'){
    # Equal variances assumed: average the two sample variances, weighted by
    # their degrees of freedom, into one pooled estimate. Buys the full
    # n1 + n2 - 2 degrees of freedom.
    calc = 'pooled'
    df = n1 + n2 - 2
    s.pooled = sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
    test.SE = s.pooled * sqrt((1/n1)+(1/n2))
  }else if (pooling == 'unpooled'){
    # Variances NOT assumed equal. The Welch-Satterthwaite formula below is the
    # ugly one from the book - it usually lands on a fractional df, which is
    # normal and not a mistake.
    calc = 'unpooled'
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
    pvalue = pt(tobs, df = df)
  }else{
    critical.value = qt(1-alpha, df = df)
    alt.hyp = 'm1 - m2 > '
    pvalue = 1-pt(tobs, df = df)
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
    print(noquote(paste0('degrees of freedom = ', round(df, 4))))
    print(noquote(paste0('Pvalue = ', round(pvalue, 4))))
    print(noquote(paste0('Decision: ', decision)))
    print(noquote(paste0(rep("=", 54), collapse = '')))
  }
}




## ---- sign-test ----
# The sign test for paired data: throw away how BIG each difference is and
# keep only whether it was + or -, then test those signs as a proportion.
#   p0       proportion of + signs under H0 (usually 0.5)
#   x1, x2   the two paired samples
#   alpha    significance level
#   test     'lower.tail', 'upper.tail' or 'two.tail'
#   verbose  TRUE prints the results
# P-values come from the exact binomial, not a normal approximation.
sign.test = function(p0, x1, x2, alpha = 0.05, test = c('lower.tail','upper.tail','two.tail'),
                     verbose = TRUE){
  test <- match.arg(test)
  
  diffs = x1 - x2
  # Pairs that tied (difference exactly 0) get no sign, so they are counted in
  # neither total - which is why total.signs can be smaller than length(x1).
  total.signs = sum(diffs>0)+sum(diffs<0)
  positive.signs = sum(diffs>0)
  
  phat = positive.signs/total.signs
  SE = sqrt(phat*(1-phat)/total.signs)
  if(test == 'two.tail'){
    upper.bound = qbinom(1-(alpha/2), total.signs, phat)
    lower.bound = qbinom(alpha/2, total.signs, phat)
    alt.hyp = 'p != '
    pvalue = 2*(1-pbinom(positive.signs-1, total.signs, p0))
    if(pvalue > 1){
      pvalue = 1
    }
  }else if(test == 'lower.tail'){
    upper.bound = qbinom(1-alpha/2, total.signs, phat)
    lower.bound = qbinom(alpha/2, total.signs, phat)
    alt.hyp = 'p < '
    pvalue = pbinom(positive.signs, total.signs, p0)
  }else{
    upper.bound = qbinom(1-(alpha/2), total.signs, phat)
    lower.bound = qbinom(alpha/2, total.signs, phat)
    alt.hyp = 'p > '
    pvalue = 1-pbinom(positive.signs-1, total.signs, p0)
  }
  
  CI = c(lower.bound/total.signs, upper.bound/total.signs)
  decision = ifelse(pvalue<alpha, 'reject H0', 'fail to reject H0')
  if(isTRUE(verbose)){
    print(noquote(paste0(paste0(rep('=', 20), collapse = ''), ' test results ', paste0(rep('=', 20), collapse = ''))))
    print(noquote(paste0('test type = ', test)))
    print(noquote(paste0('H0: p0 = ', p0)))
    print(noquote(paste0('HA: ', alt.hyp, p0)))
    print(noquote(paste0('# of positive signs = ', positive.signs)))
    print(noquote(paste0('# of total signs = ', total.signs)))
    print(noquote(paste0('Estimated P(+ sign) = ', round(phat, 4))))
    print(noquote(paste0('Estimated Standard Error P(+ sign) = ', round(SE,4))))
    print(noquote(paste0((1-alpha)*100, '% CI P(+ sign) = ', paste0('[', round(CI[1],4),',',
                                                            round(CI[2],4),']', 
                                                            collapse = ''))))
    print(noquote(paste0('Pvalue = ', round(pvalue, 4))))
    print(noquote(paste0('Decision: ', decision)))
    print(noquote(paste0(rep("=", 54), collapse = '')))
    
  }
}







## ---- rank-sum-stat ----
# Rank-sum statistic S: pool both samples, rank them from smallest to
# largest, then add up the ranks that belong to X.
#   X, Y  the two independent samples
# Returns the single number S.
rank.sum.stat=function(X, Y){
  D = sort(c(X, Y), decreasing = FALSE)
  ranks = match(X,D)
  S = sum(ranks)
  return(S)
}


## ---- wilcoxon-rank-sum-test ----
# Wilcoxon rank-sum (Mann-Whitney) test for two INDEPENDENT samples - the
# non-parametric stand-in for the two-sample t test.
#   m0        location shift under H0 (usually 0)
#   X, Y      the two samples; or supply S, n1 and n2 instead
#   S         a precomputed rank sum for sample X
#   n1, n2    the two sample sizes, required only when S is given directly
#   alpha     significance level
#   test      'lower.tail', 'upper.tail' or 'two.tail'
#   verbose   TRUE prints the results
# Prints the results and returns nothing.
Wilcoxon.rank.sum.test = function(m0, X=NULL, Y=NULL, S=NULL, n1=NULL, n2=NULL, alpha = 0.05, 
                                  test = c('lower.tail','upper.tail','two.tail'), verbose = TRUE){
  test <- match.arg(test)
  
  if(is.null(S)){
    S = rank.sum.stat(X,Y)
    n1 = length(X)
    n2 = length(Y)
  }
  
  # Convert the rank sum S into the Mann-Whitney U by subtracting the smallest
  # rank sum sample X could possibly have had (1 + 2 + ... + n1). R's wilcox
  # distribution functions are written in terms of U, not S.
  U = S - (n1*(n1+1))/2
  if(test == 'two.tail'){
    upper.bound = qwilcox(1-(alpha/2), n1, n2)
    lower.bound = qwilcox(alpha/2, n1, n2)
    alt.hyp = 'true location shift != '
    pvalue = 2*pwilcox(U, n1, n2)
    if(pvalue > 1){
      pvalue = 1
    }
  }else if(test == 'lower.tail'){
    upper.bound = qwilcox(1-(alpha/2), n1, n2)
    lower.bound = qwilcox(alpha/2, n1, n2)
    alt.hyp = 'true location shift is < '
    pvalue = pwilcox(U, n1, n2)
  }else{
    upper.bound = qwilcox(1-(alpha/2), n1, n2)
    lower.bound = qwilcox(alpha/2, n1, n2)
    alt.hyp = 'true location shift is > '
    pvalue = 1-pwilcox(U, n1, n2)
  }
  
  CI = c(lower.bound, upper.bound)
  decision = ifelse(pvalue<alpha, 'reject H0', 'fail to reject H0')
  if(isTRUE(verbose)){
    print(noquote(paste0(paste0(rep('=', 20), collapse = ''), ' test results ', paste0(rep('=', 20), collapse = ''))))
    print(noquote(paste0('test type = ', test)))
    print(noquote(paste0('H0: the true location shift = ', m0)))
    print(noquote(paste0('HA: ', alt.hyp, m0)))
    print(noquote(paste0('Sign rank statistic = ', S)))
    print(noquote(paste0('Mann-Whitney U = ', round(U, 4))))
    print(noquote(paste0('E[S] = ', round((n1*(n1+n2+1))/2, 4 ))))
    print(noquote(paste0('SE(S) =', round(sqrt( (n1*n2*(n1+n2+1))/12 ), 4 ))))
    print(noquote(paste0((1-alpha)*100, '% CI for S = ', paste0('[',max(round(CI[1],4),0),',',
                                                            round(CI[2],4),']', 
                                                            collapse = ''))))
    print(noquote(paste0('Pvalue = ', round(pvalue, 4))))
    print(noquote(paste0('Decision: ', decision)))
    print(noquote(paste0(rep("=", 54), collapse = '')))
    
  }
}





## ---- sign-rank-stat ----
# Signed-rank statistic W for paired data: rank the ABSOLUTE differences,
# put the sign of each difference back on its rank, then add them up.
#   X, Y  the two paired samples
#   ...   passed to rank() - e.g. ties.method
# Returns a list: W, the differences, the absolute differences, the ranks
# and the signed ranks, so the whole calculation can be shown in class.
sign.rank.stat = function(X,Y, ...){
  diffs = X-Y

  # Zero differences are discarded, which is the standard Wilcoxon treatment.
  #
  # This used to be written as an if/else that defined `omit` ONLY when at
  # least one difference was zero, and then used `diffs[-omit]` unconditionally
  # below. With no ties - the normal case for continuous data - `omit` did not
  # exist and the call failed with "object 'omit' not found". Worse, if an
  # `omit` happened to exist in the calling environment, R's scoping found it
  # and silently applied the signs to the WRONG observations, returning a wrong
  # W with no error at all.
  #
  # A logical mask fixes both, and avoids the negative-indexing trap that makes
  # the obvious repair (omit = integer(0)) just as broken: `x[-integer(0)]`
  # returns an EMPTY vector, not the whole of x.
  kept      = diffs[diffs != 0]
  abs.diffs = abs(kept)

  ranks = rank(abs.diffs, ...)
  signed.ranks = ranks
  signed.ranks[kept < 0] = -1*ranks[kept < 0]
  W = sum(signed.ranks)
  return(list(W = W, differences = diffs, absolute.diff = abs.diffs, ranks = ranks, signed.ranks = signed.ranks))
}

## ---- wilcoxon-sign-rank-test ----
# Wilcoxon signed-rank test for two DEPENDENT (paired) samples - the
# non-parametric stand-in for the paired t test.
#   m0       location shift under H0 (usually 0)
#   X, Y     the two paired samples, same length
#   W        a precomputed signed-rank statistic; supply n with it
#   n        number of pairs, required only when W is given directly
#   alpha    significance level
#   test     'lower.tail', 'upper.tail' or 'two.tail'
#   verbose  TRUE prints the results
# Uses the normal approximation to W, so it wants a reasonable number of pairs.
Wilcoxon.sign.rank.test = function(m0, X=NULL, Y=NULL, W=NULL, n=NULL,  alpha = 0.05, 
                                  test = c('lower.tail','upper.tail','two.tail'), verbose = TRUE,
                                  ...){
  test <- match.arg(test)
  
  # EW, VW and Z used to be computed INSIDE this if-block but are used
  # unconditionally below (and printed at the end). Passing a precomputed W -
  # which this signature explicitly invites - therefore failed with
  # "object 'EW' not found". Same shape of bug as the one in sign.rank.stat
  # above: assigned in one branch, used in all of them.
  if(is.null(W)){
    if(is.null(X) || is.null(Y)){
      stop('supply either W (with n), or both X and Y!...stopping')
    }
    if(length(X)!=length(Y)){
      stop('length(X) != length(Y)!...stopping')
    }
    W = sign.rank.stat(X,Y,...)$W
    n = length(X)
  }else if(is.null(n)){
    stop('when W is supplied directly, n must be supplied too!...stopping')
  }

  EW = m0
  VW = (n*(n+1)*(2*n +1))/6
  Z = (W-m0)/sqrt(VW)


  if(test == 'two.tail'){
    crit = qnorm(1-alpha/2, EW, sqrt(VW))
    alt.hyp = 'true location shift != '
    pvalue = 2*(1-pnorm(W, EW, sqrt(VW)))
    if(pvalue > 1){
      pvalue = 1
    }
  }else if(test == 'lower.tail'){
    crit = qnorm(alpha, EW, sqrt(VW))
    alt.hyp = 'true location shift < '
    pvalue = pnorm(W, EW, sqrt(VW))
  }else{
    crit = qnorm(1-alpha, EW, sqrt(VW))
    alt.hyp = 'true location shift > '
    pvalue = 1-pnorm(W, EW, sqrt(VW))
  }
  
  # qnorm() is already given the mean and sd of W, so it returns a bound on the
  # W scale directly. This used to multiply that bound by sqrt(VW) and add EW a
  # SECOND time, which inflated the interval by a factor of sqrt(VW) - a 10-pair
  # test printed [-754.59, 754.59] for a statistic whose SE is 19.62.
  upper.bound = qnorm(1-alpha/2, EW, sqrt(VW))
  lower.bound = qnorm(alpha/2, EW, sqrt(VW))
  CI = c(lower.bound, upper.bound)
  decision = ifelse(pvalue<alpha, 'reject H0', 'fail to reject H0')
  if(isTRUE(verbose)){
    print(noquote(paste0(paste0(rep('=', 20), collapse = ''), ' test results ', paste0(rep('=', 20), collapse = ''))))
    print(noquote(paste0('test type = ', test)))
    print(noquote(paste0('H0: true location shift = ', m0)))
    print(noquote(paste0('HA: ', alt.hyp, m0)))
    print(noquote(paste0('Rank-Sum statistic = ', W)))
    print(noquote(paste0('E[W] = ', round( EW, 4 ))))
    print(noquote(paste0('SE(W) =', round(sqrt( VW ), 4 ))))
    print(noquote(paste0('Approximate Z-statistic = ', round(Z, 4))))
    print(noquote(paste0((1-alpha)*100, '% CI for W = ', paste0('[',round(CI[1],2),',',
                                                        round(CI[2],2),']', 
                                                        collapse = ''))))
    print(noquote(paste0('critical value = ', round(crit, 4))))
    print(noquote(paste0('Pvalue = ', round(pvalue, 4))))
    print(noquote(paste0('Decision: ', decision)))
    print(noquote(paste0(rep("=", 54), collapse = '')))
    
  }
}




## ---- chi-squared-gof-test ----
# Chi-square goodness-of-fit test: do observed counts match a claimed set of
# probabilities?
#   observed.ct    vector of observed counts, one per category
#   expected.freq  claimed PROBABILITIES under H0 (must sum to 1)
#   expected.ct    claimed COUNTS under H0 - supply this OR expected.freq
#   alpha          significance level
#   verbose        TRUE prints the observed/expected/distance table
# Prints the results and returns nothing.
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



## ---- chi-squared-test ----
# Chi-square test for a two-way table: independence or homogeneity.
#   cont.table  matrix/table of observed counts, rows x columns
#   var1.name   name printed for the COLUMN variable (default 'Var A')
#   var2.name   name printed for the ROW variable (default 'Var B')
#   alpha       significance level
#   test        'independence' or 'homogeneity' - same arithmetic, different wording
#   verbose     TRUE prints the observed, expected and distance tables
# Prints the full worked table and returns nothing.
# Independence vs homogeneity is a question of how the data were COLLECTED
# (one sample cross-classified, vs one sample per row), not of the test itself.
chi.squared.test = function(cont.table, var1.name = NULL, var2.name = NULL, alpha = 0.05, 
                            test = c('homogeneity','independence'), verbose = TRUE,...){
  test <- match.arg(test)
  
  r.t = rowSums(cont.table)
  c.t = colSums(cont.table)
  n = sum(cont.table)
  # Expected count for every cell at once: outer() builds the table of
  # (row total) * (column total), then dividing by n gives the familiar
  # (R * C)/n from the notes.
  exp.cts = outer(r.t, c.t)/n
  # Cell-by-cell (observed - expected)^2 / expected; the test statistic is
  # just their sum.
  ct.dists = (cont.table - exp.cts)^2/exp.cts
  chi.obs = sum(ct.dists)
  # (rows - 1) * (columns - 1)
  df = prod(dim(cont.table)-1)
  colcats = colnames(cont.table)
  rowcats = row.names(cont.table)
  
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
}
