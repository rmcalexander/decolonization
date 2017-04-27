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
                            nsf5neigh,density_scaled,violent_resist_weighted,settler_pop,French,British,Spain,africa,middleeast,asia,density_imputed,AREA,age_imputed) %>% na.omit() 

ivprobitdf$density_log<-log(ivprobitdf$density_imputed+1)
ivprobitdf$age_log<-log(ivprobitdf$age_imputed+1)
summary(SemiParBIVProbit(list(
  violent_resist ~ density_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,
  autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy),
                         data = ivprobitdf))

summary(SemiParBIVProbit(list(
  nonviolent_resist ~ density_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,
  autonNS ~ violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy),
  data = ivprobitdf))
