library(SemiParBIVProbit)

ivprobitdf<-dplyr::select(decolonization_only_resist,year,
                            country,
                            time1,
                            time2,
                            time3,
                            autonNS,
                            rr_std,
                            literacy,
                            nsf5neigh,
                            violent_resist,
                            nonviolent_resist,
                            nonviolent_resist.l1,
                            violent_resist_weighted,
                            nsf5neigh,density_scaled,violent_resist_weighted,settler_pop,French,British,Spain,africa,middleeast,asia,density_imputed,AREA,age_imputed,centercap) %>% na.omit() 

ivprobitdf$density_log<-log(ivprobitdf$density_imputed+1)
ivprobitdf$age_log<-log(ivprobitdf$age_imputed+1)

#what is the predictive power of each instrument?
#density
summary(glm(violent_resist ~ density_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=ivprobitdf,family=binomial("probit")))
summary(glm(nonviolent_resist ~ density_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=ivprobitdf,family=binomial("probit")))

#age
summary(glm(violent_resist ~ age_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=ivprobitdf,family=binomial("probit")))
summary(glm(nonviolent_resist ~ age_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=ivprobitdf,family=binomial("probit"))) #age is only good predictor of nonviolent_resistance

#both
summary(glm(violent_resist ~ age_log + density_log + nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=ivprobitdf,family=binomial("probit")))
summary(glm(nonviolent_resist ~ age_log + density_log + violent_resist + time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=ivprobitdf,family=binomial("probit")))


#try with density
summary(SemiParBIVProbit(list(
  violent_resist ~ density_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh,
  autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh),
                         data = ivprobitdf))


summary(SemiParBIVProbit(list(
  nonviolent_resist ~ density_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh,
  autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh),
  data = ivprobitdf))

#try with age
summary(SemiParBIVProbit(list(
  violent_resist ~ age_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh,
  autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh),
  data = ivprobitdf))

summary(SemiParBIVProbit(list(
  nonviolent_resist ~ age_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh,
  autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh),
  data = ivprobitdf))

#try with both
summary(SemiParBIVProbit(list(
  violent_resist ~ age_log +density_log+nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh,
  autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh),
  data = ivprobitdf))

summary(SemiParBIVProbit(list(
  nonviolent_resist ~ age_log +density_log+violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh,
  autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh),
  data = ivprobitdf))


summary(SemiParBIVProbit(list(
  violent_resist ~ age_log +density_log +nonviolent_resist+ time1+time2+africa+centercap+nsf5neigh+settler_pop+rr_std+AREA+literacy,
  autonNS ~ violent_resist +nonviolent_resist+ time1+time2+centercap+africa+nsf5neigh+settler_pop+rr_std+AREA+literacy),
  data = ivprobitdf))

#caclulate treatment effect
biv_ate<-SemiParBIVProbit(list(
  violent_resist ~ age_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh,
  autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy+nsf5neigh),
  data = ivprobitdf)

#ATE
AT(biv_ate,nm.end = "violent_resist",type="simultaneous")
#ATT
AT(biv_ate,nm.end = "violent_resist",E=FALSE,treat=TRUE,type="simultaneous")
#plot
AT(biv_ate,nm.end = "violent_resist",E=FALSE,treat=TRUE,type="simultaneous",hd.plot=TRUE)
