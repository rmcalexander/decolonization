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

