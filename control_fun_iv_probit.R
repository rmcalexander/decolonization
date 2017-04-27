library(SemiParBIVProbit)
cf_probitdf<-ivprobitdf
cf_probitdf<-dplyr::select(cf_probitdf,density_imputed,violent_resist,nonviolent_resist,time1,time2,French,British,africa,settler_pop,rr_std,AREA,literacy,autonNS,country) %>% na.omit()
cf_probitdf$density_log<-log(cf_probitdf$density_imputed+1)


summary(SemiParBIVProbit(list(
  nonviolent_resist ~ density_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,
  autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy),
  data = ivprobitdf,fp=TRUE,margins=c("probit","probit")))

summary(SemiParBIVProbit(list(
  violent_resist ~ density_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,
  autonNS ~ nonviolent_resist +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy),
  data = ivprobitdf,fp=TRUE,margins=c("probit","probit")))

residual<-
  residuals(glm(violent_resist ~ density_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=cf_probitdf,family = binomial(link = "probit")))/
  sd(residuals(glm(violent_resist ~ density_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=cf_probitdf,family = binomial(link = "probit"))))

summary(glm(autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+residual,family = binomial(link = "probit"),data=cf_probitdf))


cf_probitdf$residual<-
  residuals(glm(violent_resist ~ density_log +nonviolent_resist+ time1+time2+as.factor(country),data=cf_probitdf,family = binomial(link = "probit")))/
  sd(residuals(glm(violent_resist ~ density_log +nonviolent_resist+ time1+time2++as.factor(country),data=cf_probitdf,family = binomial(link = "probit"))))

summary(glm(autonNS ~ violent_resist +nonviolent_resist+ time1+time2+as.factor(country)+residual,family = binomial(link = "probit"),data=cf_probitdf))

