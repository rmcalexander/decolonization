library(MatchIt)
library(tidyverse)
matchdf <- dplyr::select(decolonization,autonNS,violent_resist,density_imputed,AREA,settler_pop,nsf5neigh,rr_std,time1,British,French,africa,middleeast,lamerica,Netherlands,nonviolent_resist,literacy,asia,time2,age_imputed,centercap,time3) %>% na.omit() %>% as.data.frame()
matchdf$density_imputed_log<-log(matchdf$density_imputed)
matchdf$age_log<-log(matchdf$age_imputed+1)
m.out <- matchit(violent_resist ~density_imputed_log+ rr_std + AREA + time1 + British + literacy+French+middleeast+asia+centercap, method="genetic",
                 data=matchdf,distance="logit")
summary(m.out)

m.data <- MatchIt::match.data(m.out)
exactRankTests::wilcox.exact(m.data$autonNS[m.data$violent_resist==1],
             m.data$autonNS[m.data$violent_resist==0])

mean(m.data$autonNS[m.data$violent_resist==1])
mean(m.data$autonNS[m.data$violent_resist==0])
mean(m.data$autonNS[m.data$violent_resist==1])-mean(m.data$autonNS[m.data$violent_resist==0])


#genmatch
library("Matching")
library("rbounds")
Y <- matchdf$autonNS
Tr <- matchdf$violent_resist

glm1 <- glm(Tr ~ density_imputed_log+ rr_std + AREA + time1 +asia+settler_pop, family = binomial("logit"), data = matchdf)
summary(glm1)
#mb<-MatchBalance(Tr ~ density_imputed_log + settler_pop + rr_std + AREA + time1 +British + literacy + middleeast ,nboots = 500, data = ivprobitdf)
rr1 <- Match(Y = Y, Tr = Tr, X = glm1$fitted)
summary(rr1)
rr2 <- GenMatch(Tr = Tr, X = glm1$fitted,pop.size = 2000)

mgen1 <- Match(Y = Y, Tr = Tr, X = glm1$fitted, Weight.matrix = rr2)

MatchBalance(Tr ~ density_imputed_log+ rr_std + AREA + time1 +French+middleeast+asia+centercap+settler_pop+nsf5neigh, data = matchdf,match.out=mgen1)
summary(mgen1)
mean(matchdf$autonNS[mgen1$index.treated])
mean(matchdf$autonNS[mgen1$index.control])


exactRankTests::wilcox.exact(matchdf$autonNS[mgen1$index.treated]
  ,matchdf$autonNS[mgen1$index.control]
)
t.test(matchdf$autonNS[mgen1$index.treated]
       ,matchdf$autonNS[mgen1$index.control]
)
wilcox.test(matchdf$autonNS[mgen1$index.treated]
                             ,matchdf$autonNS[mgen1$index.control],paired=TRUE
)
psens(mgen1, Gamma=5, GammaInc=.5)
binarysens(mgen1, Gamma=10, GammaInc=.25)

#####
#now do it where treatment is nonviolent

m.out_non <- matchit(nonviolent_resist ~(density_imputed_log)+ directrule2+  + rr_std + AREA + time1 + British + literacy+French+middleeast+asia+time2, method="genetic",data=matchdf,distance="logit")

m.data_non <- MatchIt::match.data(m.out_non)
exactRankTests::wilcox.exact(m.data_non$autonNS[m.data_non$nonviolent_resist==1],
                             m.data_non$autonNS[m.data_non$nonviolent_resist==0])

mean(m.data_non$autonNS[m.data_non$nonviolent_resist==1])
mean(m.data_non$autonNS[m.data_non$nonviolent_resist==0])
mean(m.data_non$autonNS[m.data_non$nonviolent_resist==1])-mean(m.data_non$autonNS[m.data_non$nonviolent_resist==0])


#genmatch
library("Matching")
library("rbounds")
Y <- matchdf$autonNS
Tr_non <- matchdf$nonviolent_resist

glm1_non <- glm(Tr_non ~ density_imputed_log+ rr_std + AREA + time1 +French+British+middleeast+literacy+settler_pop, family = binomial("logit"), data = matchdf)
summary(glm1_non)
#mb<-MatchBalance(Tr ~ density_imputed_log + settler_pop + rr_std + AREA + time1 +British + literacy + middleeast ,nboots = 500, data = ivprobitdf)
rr1_non <- Match(Y = Y, Tr = Tr_non, X = glm1_non$fitted)
summary(rr1_non)
rr2_non <- GenMatch(Tr = Tr_non, X = glm1_non$fitted,pop.size = 2000)

mgen1_non <- Match(Y = Y, Tr = Tr_non, X = glm1_non$fitted, Weight.matrix = rr2_non)

exactRankTests::wilcox.exact(m.data_non$autonNS[m.data_non$nonviolent_resist==1],
                             m.data_non$autonNS[m.data_non$nonviolent_resist==0])
summary(mgen1_non)

mean(matchdf$autonNS[mgen1_non$index.treated])
mean(matchdf$autonNS[mgen1_non$index.control])
exactRankTests::wilcox.exact(matchdf$autonNS[mgen1_non$index.treated]
                             ,matchdf$autonNS[mgen1_non$index.control]
)
t.test(matchdf$autonNS[mgen1_non$index.treated]
       ,matchdf$autonNS[mgen1_non$index.control]
)
wilcox.test(matchdf$autonNS[mgen1_non$index.treated]
            ,matchdf$autonNS[mgen1_non$index.control],paired=FALSE
)
psens(mgen1_non, Gamma=5, GammaInc=.1)
binarysens(mgen1_non, Gamma=2, GammaInc=.1)

#try caussens
library(causalsens)
ymodel<-lm(autonNS ~ violent_resist+ rr_std + AREA + time1 +French+British+time2+time3+middleeast+settler_pop+nsf5neigh, data = matchdf)
summary(ymodel)

pmodel<-glm(violent_resist ~ density_imputed_log+ rr_std + AREA + time1 +French+middleeast+asia+centercap+settler_pop+nsf5neigh, family = binomial("logit"), data = matchdf)

ll.sens <-causalsens(ymodel, pmodel, ~ violent_resist, data = matchdf, confound = one.sided.att)
ll.sens
plot(ll.sens, type = "r.squared", bty = "n")

ll.sens2 <-causalsens(ymodel, pmodel, ~ violent_resist, data = matchdf, confound = one.sided)
ll.sens2
plot(ll.sens2, type = "r.squared", bty = "n")

ll.sens3 <-causalsens(ymodel, pmodel, ~ violent_resist, data = matchdf, confound = alignment)
ll.sens3
plot(ll.sens3, type = "r.squared", bty = "n")

ll.sens4 <-causalsens(ymodel, pmodel, ~ violent_resist, data = matchdf, confound = alignment.att)
ll.sens4
plot(ll.sens4, type = "r.squared", bty = "n")

pdf("figures/causalsensonesidedatt.pdf",height = 5, width = 5)
plot(ll.sens, type = "r.squared", bty = "n")
dev.off()

pdf("figures/causalsensonesided.pdf",height = 5, width = 5)
plot(ll.sens2, type = "r.squared", bty = "n")
dev.off()

pdf("figures/causalsensalignment.pdf",height = 5, width = 5)
plot(ll.sens3, type = "r.squared", bty = "n")
dev.off()

pdf("figures/causalsensalignmentatt.pdf",height = 5, width = 5)
plot(ll.sens4, type = "r.squared", bty = "n")
dev.off()
