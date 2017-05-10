#main models
summary(glm(autonNS ~
              violent_resist*camp_conf_intensity+ switched_from_violent_to_non+ switched_from_non_to_violent+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+nsf5neigh+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))
#violence is ineffective

summary(glm(autonNS ~
              nonviolent_resist+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+nsf5neigh+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))
#nonviolence is effective


#interaction models
#media
summary(glm(autonNS ~
             violent_resist*in_media+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+nsf5neigh+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))
#violence is less effective if people are paying attention
summary(glm(autonNS ~
             nonviolent_resist*in_media+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+nsf5neigh+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))
#nonviolence is only effect if people are paying attention

#does international condemnation matter?
summary(glm(autonNS ~ violent_resist*ab_inter_con+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+nsf5neigh+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))

summary(glm(autonNS ~ nonviolent_resist*ab_inter_con+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+nsf5neigh+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica+AREA,
            family=binomial(link="probit"),data=decolonization))

summary(glm(autonNS ~ violent_resist+ab_inter_con+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+nsf5neigh+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica+AREA,
            family=binomial(link="probit"),data=decolonization))

summary(glm(autonNS ~ nonviolent_resist+ab_inter_con+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+nsf5neigh+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))
#not really



#try survival models
library(survival)
  S<-Surv(time=decolonization$year,event=decolonization$autonNS)
summary(coxph(S~nonviolent_resist+violent_resist+rr_std+literacy+settler_pop+
          +French+British+nsf5neigh+
          middleeast+africa+AREA,control = coxph.control(iter.max = 20),data=decolonization
         ))

summary(coxph(S~violent_resist_weighted+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
                Netherlands+French+British+nsf5neigh+
                middleeast+africa+lamerica,control = coxph.control(iter.max = 20),data=decolonization
))

summary(coxph(S~nonviolent_resist*in_media+ab_inter_con+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
                Netherlands+Portugal+French+British+nsf5neigh+
                middleeast+africa+lamerica,control = coxph.control(iter.max = 20),data=decolonization
))

summary(coxph(S~violent_resist*in_media+ab_inter_con+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
                Netherlands+Portugal+French+British+nsf5neigh+
                middleeast+africa+lamerica,control = coxph.control(iter.max = 20),data=decolonization
))

summary(coxph(S~nonviolent_resist*camp_conf_intensity+ab_inter_con+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
                Netherlands+Portugal+French+British+nsf5neigh+
                middleeast+africa+lamerica,control = coxph.control(iter.max = 20),data=decolonization
))

summary(coxph(S~violent_resist*camp_conf_intensity+ab_inter_con+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
                Netherlands+Portugal+French+British+nsf5neigh+
                middleeast+africa+lamerica,control = coxph.control(iter.max = 20),data=decolonization
))

#try using population for instrument

summary(AER::ivreg(
  autonNS~violent_resist+csyear1+csyear2+rr_std+literacy+
    Portugal+French+British+africa+asia+lamerica+settler_pop
  |csyear1+csyear2+rr_std+literacy+
    Portugal+French+British+africa+asia+lamerica+settler_pop
  +age_imputed,data=decolonization
),diagnostics = TRUE)

summary(AER::ivreg(
  autonNS~violent_resist+csyear1+csyear2+rr_std+literacy+
    Portugal+French+British+africa+asia+lamerica+settler_pop
  |csyear1+csyear2+rr_std+literacy+
    Portugal+French+British+africa+asia+lamerica+settler_pop
  +density_imputed,data=decolonization
),diagnostics = TRUE)

summary(AER::ivreg(
  autonNS~violent_resist+csyear1+csyear2+rr_std+literacy+
    Portugal+French+British+africa+asia+lamerica+settler_pop
  |csyear1+csyear2+rr_std+literacy+
    Portugal+French+British+africa+asia+lamerica+settler_pop
  +age_imputed+density_imputed,data=decolonization
),diagnostics = TRUE)

summary(AER::ivreg(
  autonNS~violent_resist+csyear1+csyear2+rr_std+literacy+
    Portugal+French+British+africa+asia+lamerica+settler_pop+constborder
  |csyear1+csyear2+rr_std+literacy+
    Portugal+French+British+africa+asia+lamerica+settler_pop+constborder
  +density_scaled,data=decolonization
),diagnostics = TRUE)

summary(AER::ivreg(
  autonNS~violent_resist+csyear1+csyear2+rr_std+literacy+constborder+
    Portugal+French+British+Netherlands+africa+asia+lamerica+settler_pop
  |csyear1+csyear2+rr_std+literacy+Netherlands+
    Portugal+French+British+Netherlands+africa+asia+lamerica+settler_pop+constborder
  +age_imputed+density_imputed,data=decolonization
),diagnostics = TRUE)

summary(AER::ivreg(
  autonNS~violent_resist+csyear1+csyear2+rr_std+literacy+Netherlands+nsf5neigh+
    Portugal+French+British+Netherlands+africa+asia+lamerica+settler_pop+constborder+AREA
  |csyear1+csyear2+rr_std+literacy+Netherlands+nsf5neigh+
    Portugal+French+British+Netherlands+africa+asia+lamerica+settler_pop+constborder+density_imputed+I(density_imputed^2)+AREA,data=decolonization
),diagnostics = TRUE)


summary(AER::ivreg(
  autonNS~violent_resist+as.factor(country)+nonviolent_resist+density_imputed+rr_std+nsf5neigh+I(density_imputed^2)
  |as.factor(country)+density_imputed+I(density_imputed^2)+I(density_imputed^3)+nonviolent_resist+rr_std+nsf5neigh,data=decolonization
),diagnostics = TRUE)

summary(AER::ivreg(
  autonNS~violent_resist+as.factor(country)+nonviolent_resist+rr_std+nsf5neigh+time1+time2+time3
  |as.factor(country)+log(density_imputed)+nonviolent_resist+rr_std+nsf5neigh+time1+time2+time3,data=decolonization
),diagnostics = TRUE)


summary(AER::ivreg(
  autonNS~
    violent_resist_weighted+as.factor(country)+nonviolent_resist.l1+nsf5neigh+rr_std+time1+time2+time3
  |as.factor(country)+density_scaled+nonviolent_resist.l1+nsf5neigh+rr_std+time1+time2+time3,data=decolonization
),diagnostics = TRUE)

#only countries that have had any resistance
summary(AER::ivreg(
  autonNS~
    violent_resist+as.factor(country)+nsf5neigh+rr_std+as.factor(year)
  |as.factor(country)+log(density_imputed+1)+nsf5neigh+rr_std+as.factor(year),data=dplyr::filter(decolonization,cown==352|cown==404|cown==452|cown==471|cown==475|cown==501|cown==540|cown==541|cown==551|cown==553|cown==580|cown==600|cown==615|cown==616|cown==666|cown==679|cown==816|cown==820)
),diagnostics = TRUE)
#try with predict

predictdf<-dplyr::select(decolonization,country,autonNS,violent_resist,nonviolent_resist.l1,time1,time2,time3,nsf5neigh,density_scaled,violent_resist_weighted,cown,year) %>% na.omit()


predictdf$pred_density<-predict(lm(violent_resist_weighted~as.factor(country)+nonviolent_resist.l1+time1+time2+time3+nsf5neigh+density_scaled,data=predictdf))

predictdf$pred_density_probit<-predict(glm(violent_resist~as.factor(country)+nonviolent_resist.l1+time1+time2+time3+nsf5neigh+density_scaled,data=predictdf,family=binomial("probit")))

predictdf$pred_density_probit2<-predict(glm(violent_resist~as.factor(country)+nonviolent_resist.l1+time1+time2+time3+nsf5neigh+density_scaled+pred_density_probit,data=predictdf,family=binomial("probit")))

summary(lm(autonNS ~ as.factor(country)+nonviolent_resist.l1+time1+time2+time3+nsf5neigh+pred_density_probit2,data=predictdf))

summary((lm(violent_resist_weighted~as.factor(country)+nonviolent_resist.l1+as.factor(year)+nsf5neigh+density_scaled,data=dplyr::filter(predictdf,
                                                                                                                                          cown!=615|
                                                                                                                                            cown!=540|
                                                                                                                                            cown!=471|
                                                                                                                                            cown!=352|
                                                                                                                                            cown!=404|
                                                                                                                                            cown!=850|
                                                                                                                                            cown!=666|
                                                                                                                                            cown!=501|
                                                                                                                                            cown!=580|
                                                                                                                                            cown!=820|
                                                                                                                                            cown!=600|
                                                                                                                                            cown!=541|
                                                                                                                                            cown!=616|
                                                                                                                                            cown!=816|
                                                                                                                                            cown!=679                                                                                                                                  ))))


 
print(stan_glm(autonNS~as.factor(country)+nonviolent_resist.l1+time1+time2+time3+nsf5neigh+pred_density,data=predictdf,family=binomial(link="logit"),chains=1),digits=4)




#get right standard errros from bootstrap
library(boot)
bootfun<-function(data=predictdf,index){
  stage1<-predict(lm(violent_resist_weighted~as.factor(country)+nonviolent_resist.l1+time1+time2+time3+nsf5neigh+density_scaled,subset=index,data=predictdf))
  coefficients(glm(autonNS~as.factor(country)+nonviolent_resist.l1+time1+time2+time3+nsf5neigh+stage1,subset=index,data=predictdf,family=binomial("logit")))
}
  

boot_output<-boot(predictdf,bootfun,R=50)
boot_output
summary(boot_output)
coefficients(glm(autonNS~as.factor(country)+nonviolent_resist.l1+time1+time2+time3+nsf5neigh+pred_density,family=binomial("logit"),data=predictdf))



#ivprobit
library(ivprobit)

ivprob(I(predictdf$autonNS), predictdf$settler_pop, predictdf$violent_resist, predictdf$density_imputed)


#try fixed effects
summary(lm(autonNS~as.factor(cown)+csyear1+csyear2+violent_resist,data=decolonization))
