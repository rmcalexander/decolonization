#THIS SCRIPT ESTIMATES A NUMBER OF LINEAR FIXED EFFECTS MODELS WITH VARIOUS INSTRUMENTS.
#NOT USED

library(lfe)

#full decolonizations
#DENSITY
#time trend no country fixed effects
  summary(felm(autonNS ~nonviolent_resist+time1+time2+time3+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa|(violent_resist~(density_scaled)),data=decolonization_only_resist,exactDOF = TRUE))
summary(felm(autonNS ~nonviolent_resist+time1+time2+time3+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa|(violent_resist~(density_imputed)),data=decolonization,exactDOF = TRUE))
#year fixed effects no country fixed effects
summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa+year|(violent_resist~(density_scaled)),data=decolonization,exactDOF = TRUE))
summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa+year|(violent_resist~(density_imputed)),data=decolonization,exactDOF = TRUE))
#year fixed effects, country fixed effects
summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh|country+year|(violent_resist~(density_scaled)),data=decolonization,exactDOF = TRUE))
summary(felm(autonNS ~nonviolent_resist+rr_std+nsf5neigh+centercap|country+year|(violent_resist~(log(density_imputed))),data=decolonization,exactDOF = TRUE))

summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh+time1+time2+time3|country|(violent_resist~(density_imputed)),data=decolonization,exactDOF = TRUE))
summary(felm(autonNS ~nonviolent_resist+rr_std+nsf5neigh|country+year|(violent_resist~(age_imputed)),data=decolonization,exactDOF = TRUE))

#age imputed with full decolonizations
#time trend no country fixed effects
summary(felm(autonNS ~nonviolent_resist+time1+time2+time3+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa|(violent_resist~(age_imputed)),data=decolonization,exactDOF = TRUE)) #****#
summary(felm(autonNS ~nonviolent_resist+time1+time2+time3+rr_std+literacy+nsf5neigh+settler_pop+AREA+centercap|French+British+middleeast+africa|(violent_resist~(age_imputed)),data=decolonization,exactDOF = TRUE)) #****#
#year fixed effects no country fixed effects
summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa+year|(violent_resist~(age_imputed)),data=decolonization,exactDOF = TRUE)) #****#
#year fixed effects, country fixed effects
summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh|country+year|(violent_resist~(age_imputed)),data=decolonization,exactDOF = TRUE))

#JUST RESISTING COUNTRIES
#DENSITY
#time trend no country fixed effects
summary(felm(autonNS ~nonviolent_resist+time1+time2+time3+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa|(violent_resist~(density_scaled)),data=decolonization_only_resist,exactDOF = TRUE))
summary(felm(autonNS ~nonviolent_resist+time1+time2+time3+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa|(violent_resist~(density_imputed)),data=decolonization_only_resist,exactDOF = TRUE))
#year fixed effects no country fixed effects
summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa+year|(violent_resist~(density_scaled)),data=decolonization_only_resist,exactDOF = TRUE))
summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa+year|(violent_resist~(density_imputed)),data=decolonization_only_resist,exactDOF = TRUE))
#year fixed effects, country fixed effects
summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh|country+year|(violent_resist~(density_scaled)),data=decolonization_only_resist,exactDOF = TRUE))
summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh|country+year|(violent_resist~(density_imputed)),data=decolonization_only_resist,exactDOF = TRUE))

#AGE imputed with full decolonizations
#time trend no country fixed effects
summary(felm(autonNS ~nonviolent_resist+time1+time2+time3+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa|(violent_resist~(age_imputed)),data=decolonization_only_resist)) #****#
#year fixed effects no country fixed effects
summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh+settler_pop+AREA|French+British+middleeast+africa+year|(violent_resist~(age_imputed)),data=decolonization_only_resist)) #****#
#year fixed effects, country fixed effects
summary(felm(autonNS ~nonviolent_resist+rr_std+literacy+nsf5neigh|country+year|(violent_resist~log(age_imputed)),data=decolonization_only_resist)) #****#





#IV MODELS
summary(AER::ivreg(autonNS~nonviolent_resist+rr_std+nsf5neigh+violent_resist+as.factor(country)+time1
|nonviolent_resist+rr_std+nsf5neigh+as.factor(country)+time1+density_imputed,data=decolonization_only_resist),diagnostics = TRUE)
####DING DING
summary(AER::ivreg(autonNS~
                     nonviolent_resist+rr_std+nsf5neigh+violent_resist+as.factor(country)+as.factor(year)+literacy
                   |nonviolent_resist+rr_std+nsf5neigh+as.factor(country)+as.factor(year)+density_scaled+literacy,data=decolonization_only_resist
),diagnostics = TRUE)


summary(AER::ivreg(autonNS~
                     nonviolent_resist+rr_std+nsf5neigh+violent_resist+as.factor(country)+as.factor(year)
                   |nonviolent_resist+rr_std+nsf5neigh+as.factor(country)+as.factor(year)+log(density_imputed),data=decolonization_only_resist
),diagnostics = TRUE)


summary(AER::ivreg(autonNS~
                     nonviolent_resist+rr_std+nsf5neigh+violent_resist+as.factor(country)+as.factor(year)
                   |nonviolent_resist+rr_std+nsf5neigh+as.factor(country)+as.factor(year)+age_imputed,data=decolonization_only_resist
),diagnostics = TRUE)

summary(lm(autonNS~nonviolent_resist+rr_std+nsf5neigh+as.factor(country)+as.factor(year)+(age_imputed),data=decolonization_only_resist))

summary(AER::ivreg(autonNS~nonviolent_resist+ rr_std+nsf5neigh+violent_resist+French+British+middleeast+africa+time1+time2+time3
|nonviolent_resist+rr_std+nsf5neigh+French+British+middleeast+africa+time1+time2+time3+(age_imputed),data=decolonization_only_resist
),diagnostics = TRUE)


summary(AER::ivreg(autonNS~nonviolent_resist+rr_std+nsf5neigh+violent_resist+as.factor(country)+time1
                   |nonviolent_resist+rr_std+nsf5neigh+as.factor(country)+time1+density_imputed,data=decolonization),diagnostics = TRUE)
####DING DING
summary(AER::ivreg(autonNS~
                     nonviolent_resist+rr_std+nsf5neigh+violent_resist+as.factor(country)+as.factor(year)+literacy
                   |nonviolent_resist+rr_std+nsf5neigh+as.factor(country)+as.factor(year)+density_scaled+literacy,data=decolonization
),diagnostics = TRUE)

summary(AER::ivreg(autonNS~
                     nonviolent_resist+rr_std+nsf5neigh+violent_resist_weighted+French+British+middleeast+africa+literacy+time1
                   |nonviolent_resist+rr_std+nsf5neigh+French+British+middleeast+africa+density_imputed+literacy+time1,data=decolonization
),diagnostics = TRUE)

summary(AER::ivreg(autonNS~
                     nonviolent_resist+rr_std+nsf5neigh+violent_resist+as.factor(country)+as.factor(year)
                   |nonviolent_resist+rr_std+nsf5neigh+as.factor(country)+as.factor(year)+density_scaled,data=decolonization
),diagnostics = TRUE)
summary(AER::ivreg(autonNS~
                     nonviolent_resist+rr_std+nsf5neigh+violent_resist+as.factor(country)+as.factor(year)
                   |nonviolent_resist+rr_std+nsf5neigh+as.factor(country)+as.factor(year)+age_imputed,data=decolonization
),diagnostics = TRUE)

summary(lm(autonNS~nonviolent_resist+rr_std+nsf5neigh+as.factor(country)+as.factor(year)+(age_imputed),data=decolonization))

summary(AER::ivreg(autonNS~rr_std+nsf5neigh+violent_resist+French+British+middleeast+africa+time1+time2+time3
                   |rr_std+nsf5neigh+French+British+middleeast+africa+time1+time2+time3+log(age_imputed),data=decolonization
),diagnostics = TRUE)


#instrument for nonviolnet resist

summary(AER::ivreg(autonNS~
                     rr_std+nsf5neigh+nonviolent_resist+French+British+AREA+settler_pop+middleeast+africa+time1+time2+time3+violent_resist
                   |violent_resist+rr_std+nsf5neigh+French+British+AREA+settler_pop+middleeast+africa+time1+time2+time3+(age_imputed)+violent_resist,data=decolonization_only_resist
),diagnostics = TRUE)

summary(AER::ivreg(autonNS~
                     rr_std+nsf5neigh+nonviolent_resist+as.factor(country)+violent_resist+time1+time2+time3+age_imputed
                   |violent_resist+rr_std+nsf5neigh+as.factor(country)+density_imputed+time1+time2+time3+age_imputed,data=decolonization_only_resist
),diagnostics = TRUE)

#age is instument for violent
#density is instrument for nonviolent

summary(lm(violent_resist~age_imputed+rr_std+nsf5neigh+as.factor(country)+log(density_imputed)+as.factor(year),data=decolonization))
summary(lm(nonviolent_resist~age_imputed+rr_std+nsf5neigh+as.factor(country)+age_imputed+as.factor(year),data=decolonization))
summary(lm(nonviolent_resist~rr_std+nsf5neigh+as.factor(country)+log(density_imputed)+as.factor(year),data=decolonization))
summary(lm(violent_resist~age_imputed+rr_std+nsf5neigh+as.factor(country)+as.factor(year),data=decolonization))

summary(lm(autonNS~rr_std+nsf5neigh+as.factor(country)+density_imputed+as.factor(year),data=dplyr::filter(decolonization_only_resist,nonviolent_resist!=1)))
summary(lm(autonNS~age_imputed+rr_std+nsf5neigh+as.factor(country)+as.factor(year),data=dplyr::filter(decolonization_only_resist,violent_resist!=1)))

summary(felm(autonNS ~time1+time2+time3+rr_std+literacy+nsf5neigh+age_imputed|country,data=dplyr::filter(decolonization_only_resist,violent_resist!=1)))

summary(felm(autonNS ~rr_std+literacy+nsf5neigh+age_imputed+density_scaled|country+year,data=decolonization_only_resist))

#intrument for nonviolent. ATE= 0.9
summary(AER::ivreg(autonNS~
                     rr_std+nsf5neigh+nonviolent_resist+French+British+AREA+settler_pop+middleeast+africa++violent_resist+time1+time2+time3
                   |violent_resist+rr_std+nsf5neigh+French+British+AREA+settler_pop+middleeast+africa++log(density_imputed)+time1+time2+time3,data=decolonization
),diagnostics = TRUE)

#instrument for violent. ATE .37
summary(AER::ivreg(autonNS~
                     rr_std+nsf5neigh+nonviolent_resist+French+British+AREA+settler_pop+middleeast+africa++violent_resist+time1+time2+time3
                   |nonviolent_resist+rr_std+nsf5neigh+French+British+AREA+settler_pop+middleeast+africa+log(density_imputed)+time1+time2+time3,data=decolonization
),diagnostics = TRUE)
#violent with fes
summary(AER::ivreg(autonNS~
                     rr_std+nsf5neigh+nonviolent_resist+as.factor(country)+violent_resist+time1+time2+time3
                   |rr_std+nsf5neigh+nonviolent_resist+as.factor(country)+time1+time2+time3+log(density_imputed)+time1+time2+time3,data=decolonization
),diagnostics = TRUE)
#nonviolent with fes
summary(AER::ivreg(autonNS~
                     rr_std+nsf5neigh+nonviolent_resist+as.factor(country)+violent_resist+as.factor(year)
                   |rr_std+nsf5neigh+nonviolent_resist+as.factor(country)+as.factor(year)+log(density_imputed)+age_imputed,data=decolonization
),diagnostics = TRUE)


summary(lm(violent_resist~rr_std+nsf5neigh+French+British+AREA+settler_pop+middleeast+africa+nonviolent_resist+time1+time2+time3+log(density_imputed),data=decolonization))
