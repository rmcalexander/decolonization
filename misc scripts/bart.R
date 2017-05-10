library(tidyverse)
library(BayesTree)

bartdf<-bayes_iv_randomeffects_df[,c(10,3:9,11,14,15,16)]

xp<-dplyr::filter(bartdf,violent_resist==1)
xp$violent_resist<-0
bart.tot <- bart(x.train=bartdf[,-5], y.train=bartdf$autonNS, x.test=xp[,-5])


mndiffs = apply(bart.tot$yhat.train[,bartdf$violent_resist==1]
                - bart.tot$yhat.test, 1, mean)
mean(mndiffs)
sd(mndiffs)
