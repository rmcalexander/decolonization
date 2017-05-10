#this file uses a control function approach to account for endogeneity in violent and nonviolent resistance

#create new dataset
#only include countries that have ever experienced resistance to colonial rule.

cf_probitdf<-decolonization_only_resist
cf_probitdf<-dplyr::select(cf_probitdf,density_imputed,violent_resist,nonviolent_resist,time1,time2,French,British,africa,settler_pop,rr_std,AREA,literacy,autonNS,country,nsf5neigh,age_log) %>% na.omit()
cf_probitdf$density_log<-log(cf_probitdf$density_imputed+1)

#collect first stage probit residuals when violent resistance is the endogenous binary DV.
#probit link
residual_v<-
  residuals(glm(violent_resist ~ age_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh,data=cf_probitdf,family = binomial(link = "probit")))/
  sd(residuals(glm(violent_resist ~ age_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh,data=cf_probitdf,family = binomial(link = "probit"))))

#second stage
#probit
summary(glm(autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+residual_v+nsf5neigh,family = binomial(link = "probit"),data=cf_probitdf))

#logit
summary(glm(autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+residual_v+nsf5neigh,family = binomial(link = "logit"),data=cf_probitdf))

#now do same for nonviolent resistance
#probit link
residual_nv<-
  residuals(glm(nonviolent_resist ~ age_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh,data=cf_probitdf,family = binomial(link = "probit")))/
  sd(residuals(glm(nonviolent_resist ~ age_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh,data=cf_probitdf,family = binomial(link = "probit"))))

#second stage
#probit
summary(glm(autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+residual_nv+nsf5neigh,family = binomial(link = "probit"),data=cf_probitdf))

#logit
summary(glm(autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+residual_nv+nsf5neigh,family = binomial(link = "logit"),data=cf_probitdf))

