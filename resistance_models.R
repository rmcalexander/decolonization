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
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))

summary(glm(autonNS ~ violent_resist+ab_inter_con+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+nsf5neigh+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
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
summary(coxph(S~nonviolent_resist+ab_inter_con+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
          Netherlands+Portugal+French+British+nsf5neigh+violent_resist.l2+
          middleeast+africa+lamerica,control = coxph.control(iter.max = 20),data=decolonization
         ))

summary(coxph(S~violent_resist+ab_inter_con+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
                Netherlands+Portugal+French+British+nsf5neigh+
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
  autonNS~violent_resist_weighted+csyear1+csyear2+rr_std+literacy+constborder+
    Portugal+French+British+Netherlands+africa+asia+lamerica+settler_pop+nonviolent_resist.l1
  |csyear1+csyear2+rr_std+literacy+Netherlands+
    Portugal+French+British+Netherlands+africa+asia+lamerica+settler_pop+constborder+nonviolent_resist.l1
  +age_imputed+density_imputed,data=decolonization
),diagnostics = TRUE)

summary(AER::ivreg(
  autonNS~violent_resist_weighted+csyear1+csyear2+rr_std+literacy+Netherlands+
    Portugal+French+British+Netherlands+africa+asia+lamerica+settler_pop+constborder+nonviolent_resist.l1
  |csyear1+csyear2+rr_std+literacy+Netherlands+
    Portugal+French+British+Netherlands+africa+asia+lamerica+settler_pop+constborder+nonviolent_resist.l1+density_imputed,data=decolonization
),diagnostics = TRUE)

#try with predict

predictdf<-dplyr::select(decolonization,autonNS,violent_resist_weighted,violent_resist,nonviolent_resist.l1,
                  csyear1,csyear2,rr_std,literacy,constborder,Portugal,French,British,Netherlands,africa,asia,lamerica,settler_pop,age_imputed,density_imputed) %>% na.omit()

predictdf$pred_age<-predict(lm(violent_resist_weighted~csyear1+csyear2+rr_std+literacy+Netherlands+
                                 Portugal+French+British+Netherlands+africa+asia+lamerica+settler_pop+constborder+nonviolent_resist.l1+age_imputed,data=predictdf))

predictdf$pred_density<-predict(lm(violent_resist_weighted~csyear1+csyear2+rr_std+literacy+Portugal+French+British+africa+asia+lamerica+settler_pop+constborder+nonviolent_resist.l1+density_imputed,data=predictdf))

summary(lm(violent_resist_weighted~csyear1+csyear2+rr_std+literacy+
             Portugal+French+British+Netherlands+africa+asia+lamerica+settler_pop+constborder+nonviolent_resist.l1+density_imputed,data=predictdf))
summary(glm(autonNS~csyear1+csyear2+rr_std+literacy+Portugal+French+British+Netherlands+africa+asia+lamerica+settler_pop+constborder+nonviolent_resist.l1+pred_density,data=predictdf,family=binomial(link="logit")))

summary(glm(autonNS~csyear1+csyear2+rr_std+literacy+
              Portugal+French+British+africa+asia+lamerica+settler_pop+constborder+nonviolent_resist.l1+pred_density,data=predictdf,family=binomial(link="probit")))

summary(rstanarm::stan_glm(autonNS~csyear1+csyear2+rr_std+literacy+
              Portugal+French+British+africa+asia+lamerica+settler_pop+constborder+nonviolent_resist.l1+pred_density,data=predictdf,family=binomial(link="probit")))


#get right standard errros from bootstrap
bootfun<-function(data=predictdf,index){
  stage1<-predict(lm(violent_resist_weighted~csyear1+csyear2+rr_std+density_imputed+Portugal+French+British+africa+asia+settler_pop+constborder+nonviolent_resist.l1,subset=index,data=predictdf))
  coefficients(lm(autonNS~stage1+csyear1+csyear2+rr_std+literacy+Portugal+French+British+africa+asia+settler_pop+constborder+nonviolent_resist.l1,subset=index,data=predictdf))
}
  

boot_output<-boot(predictdf,bootfun,R=5)
boot_output
summary(boot_output)

summary(AER::ivreg(
  autonNS~violent_resist_weighted+csyear1+csyear2+rr_std+Portugal+French+British+africa+asia+lamerica+settler_pop+constborder+nonviolent_resist.l1
  |csyear1+csyear2+rr_std+density_imputed+Portugal+French+British+africa+asia+lamerica+settler_pop+constborder+nonviolent_resist.l1+density_imputed,data=decolonization
),diagnostics = TRUE)
