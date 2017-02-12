#main models
summary(glm(autonNS ~
              violent_resist+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))
#violence is ineffective

summary(glm(autonNS ~
              nonviolent_resist+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))
#nonviolence is effective


#interaction models
#media
summary(glm(autonNS ~
             violent_resist*in_media+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))
#violence is less effective if people are paying attention
summary(glm(autonNS ~
             nonviolent_resist*in_media+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))
#nonviolence is only effect if people are paying attention

#does international condemnation matter?
summary(glm(autonNS ~ violent_resist*ab_inter_con+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))

summary(glm(autonNS ~ nonviolent_resist*ab_inter_con+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))

summary(glm(autonNS ~ violent_resist+ab_inter_con+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))

summary(glm(autonNS ~ nonviolent_resist+ab_inter_con+
              csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
              Netherlands+Portugal+French+British+
              middleeast+africa+lamerica,
            family=binomial(link="probit"),data=decolonization))
#not really


#Bayes
violent<-rstanarm::stan_glm(autonNS ~
               violent_resist+
               csyear1+csyear2+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
               Netherlands+Portugal+French+British+
               middleeast+africa+lamerica,
             family=binomial,data=decolonization)


#try survival models
library(survival)
S<-Surv(time=decolonization$year,event=decolonization$autonNS)
summary(coxph(S~nonviolent_resist+ab_inter_con+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
          Netherlands+Portugal+French+British+
          middleeast+africa+lamerica,control = coxph.control(iter.max = 20),data=decolonization
         ))

summary(coxph(S~violent_resist+ab_inter_con+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
                Netherlands+Portugal+French+British+
                middleeast+africa+lamerica,control = coxph.control(iter.max = 20),data=decolonization
))

summary(coxph(S~nonviolent_resist*in_media+ab_inter_con+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
                Netherlands+Portugal+French+British+
                middleeast+africa+lamerica,control = coxph.control(iter.max = 20),data=decolonization
))

summary(coxph(S~violent_resist*in_media+ab_inter_con+rr_std+literacy+tot_ns+ns5emp+centercap+settler_pop+
                Netherlands+Portugal+French+British+
                middleeast+africa+lamerica,control = coxph.control(iter.max = 20),data=decolonization
))
