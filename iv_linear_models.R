#these models use age or density as an instrument. uses two linear models to model the binary dvs

#all countries

#DV violent resist, instrument density
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
|density_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization
),diagnostics = TRUE)

#DV violent resist, instrument age
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
|age_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization
),diagnostics = TRUE)

#DV nonviolent resist, instrument density
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
|density_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization
),diagnostics = TRUE)

#DV nonviolent resist, instrument age
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
|age_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization
),diagnostics = TRUE)

#DV nonviolent resist, instrument age and density
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
                   |age_log +density_log+violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization
),diagnostics = TRUE)

#DV violent resist, instrument age and density
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
                   |age_log +density_log+nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization
),diagnostics = TRUE)

#just resisting countries
#DV violent resist, instrument density
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
|density_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization_only_resist
),diagnostics = TRUE)

#DV violent resist, instrument age
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
|age_log +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization_only_resist
),diagnostics = TRUE)

#DV nonviolent resist, instrument density ****
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
|density_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization_only_resist
),diagnostics = TRUE)

#DV nonviolent resist, instrument age
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
|age_log +violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization_only_resist
),diagnostics = TRUE)

#DV nonviolent resist, instrument age and density
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
                   |age_log +density_log+violent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization_only_resist
),diagnostics = TRUE)

#DV violent resist, instrument age and density
summary(AER::ivreg(autonNS~violent_resist +nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy
                   |age_log +density_log+nonviolent_resist+ time1+time2+French+British+africa+settler_pop+rr_std+AREA+literacy,data=decolonization_only_resist
),diagnostics = TRUE)
