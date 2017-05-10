#NOT USED

library(rstan)
library(tidyverse)
bayes_iv_df<-dplyr::select(decolonization,year,
                    country,
                    time1,
                    time2,
                    autonNS,
                    rr_std,
                    literacy,
                    constborder,
                    nsf5neigh,
                    middleeast,
                    africa,
                    lamerica,
                    asia,
                    oceania,
                    eeurop,
                    Netherlands,
                    Portugal,
                    Spain,
                    French,
                    British,
                    violent_resist,
                    nonviolent_resist.l1,
                    violent_resist_weighted,
                    age_imputed,
                    density_imputed,
                    settler_pop,
                    nsf5neigh,cown,AREA) %>% na.omit() #%>% filter(density_imputed<200)
#bayes_iv_df$density_imputed<-log(bayes_iv_df$density_imputed+1)
bayes_iv_code<-"
data{
  int N; //number of observations
  int J; //number of covariates
  vector[N] density_imputed; //instrument
  vector[N] violent_resist_weighted; //endogenous variable
  vector[N] Y; //dependent variable
  matrix[N,J] X;
}
parameters{
  real alpha1;
  real alpha2;
  real pi;
  real gamma;
  real zeta;
  vector[N] density_imputed_measured;
  vector[J] beta1;
  vector[J] beta2;
   real<lower=0> sigma1;
  real<lower=0> sigma2;
}
model{
//density_imputed_measured ~ normal(density_imputed,25);  
violent_resist_weighted ~ normal( alpha1 + density_imputed*pi + X*beta1,sigma1);
Y ~ normal( alpha2 + density_imputed *pi*zeta + X*beta2 + density_imputed*0,sigma2); 
//priors
  //gamma ~ normal(0,0.01091*.2);
  alpha1 ~ normal(0,5);
  alpha2 ~ normal(0,5);
  beta1 ~ normal(0,5);
  beta2 ~ normal(0,5);
  pi ~ normal(0,5);
  zeta ~ normal(0,5);
  sigma1 ~ cauchy(0,1);
  sigma2 ~ cauchy(0,1);
}"

#no_violent_dens_countries<-(rstanarm::stan_glm(autonNS~csyear1+csyear2+rr_std+literacy+nsf5neigh+French+British+africa+asia+settler_pop+nonviolent_resist.l1+constborder+density_imputed,data=filter(bayes_iv_df,cown!=615 & cown!=540 & cown!=471 & cown!=352 & cown!=404&cown!=850& cown!=666&cown!=501&cown!=580& cown!=820&cown!=600& cown!=541&  cown!=616&cown!=816& cown!=679),family=binomial("logit"))) #this is for countries that never experienced a violent resistance


#no_violent_dens<-rstanarm::stan_glm(autonNS~csyear1+csyear2+rr_std+literacy+nsf5neigh+French+British+africa+asia+settler_pop+nonviolent_resist.l1+constborder+density_imputed,data=filter(bayes_iv_df,violent_resist==0),family=binomial("logit"))


#first_stage_bayes<-(rstanarm::stan_glm(violent_resist~csyear1+csyear2+rr_std+literacy+nsf5neigh+French+British+africa+asia+settler_pop+nonviolent_resist.l1+constborder+density_imputed,data=filter(bayes_iv_df),family=binomial("logit")))

print(first_stage_bayes,digits=5)
print(no_violent_dens,digits=5)
print(no_violent_dens_countries,digits=5)

#this is for countries that only experienced a violent resistance
summary(glm(autonNS~csyear1+csyear2+rr_std+literacy+nsf5neigh+French+British+africa+asia+settler_pop+nonviolent_resist.l1+constborder+density_imputed,data=filter(bayes_iv_df,cown==615|
              cown==540|
              cown==471|
              cown==352|
              cown==404|
              cown==850|
              cown==666|
              cown==501|
              cown==580|
              cown==820|
              cown==600|
              cown==541|
              cown==616|
              cown==816|
              cown==679),family=binomial("probit")))

summary(glm(violent_resist~csyear1+csyear2+rr_std+literacy+nsf5neigh+British+settler_pop+nonviolent_resist.l1+constborder+density_imputed+AREA,data=filter(bayes_iv_df),family=binomial("probit")))

bayes_iv_list<-list(
  N<-nrow(ivprobitdf),
  J<-10,
  density_imputed<-(ivprobitdf$density_log),
  violent_resist_weighted<-as.vector(ivprobitdf$nonviolent_resist),
  X<-ivprobitdf[,c(10,3,4,16,17,19,15,7,23,8)],
  Y<-as.integer(ivprobitdf$autonNS)
)
colnames(ivprobitdf)
colnames(bayes_iv_df[,c(3:4,6:9,10,11,19,20,26,28)])
bayes_iv_model<-stan(model_code = bayes_iv_code,
                     data = bayes_iv_list,
                     seed = 123,
                     chains = 1,cores=1,
                     iter = 500)

print(bayes_iv_model,pars=c("zeta","beta2"), digits = 4)
#launch_shinystan(bayes_iv_model)


