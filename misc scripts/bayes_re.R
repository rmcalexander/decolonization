#NOT USED
#bayesian random effects model

library(rstan)
library(rstanarm )
library(tidyverse)
bayes_iv_randomeffects_df<-dplyr::select(decolonization_only_resist,year,
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
                                         violent_resist_weighted,
                                         nsf5neigh,density_scaled,violent_resist_weighted,density_imputed,age_imputed,centercap) %>% na.omit() 
bayes_iv_randomeffects_df$density_imputed<-log(bayes_iv_randomeffects_df$density_imputed)
bayes_iv_randomeffects_df<-bayes_iv_randomeffects_df[order(bayes_iv_randomeffects_df$time1),]
bayes_iv_re_code<-"
data{
int N; //number of observations
int J; //number of covariates
int Y[N]; //dependent variable
int K; //number of countries
vector[N] violent_resist; //endogenous variable
matrix[N,J] X;
int country_ids[N];
}
parameters{
real alpha;
vector[J] beta;
//real<lower=0> sigma;
vector[K] zeta;
real<lower=0> zeta_sig;
real zeta_mean;
}
model{
for(i in 1:N){
Y[i] ~ bernoulli_logit( alpha + X[i,]*beta + violent_resist[i]*zeta[country_ids[i]]); 
}

alpha ~ normal(0,1);
zeta ~ normal(zeta_mean,zeta_sig);
zeta_mean ~ normal(0,1);
zeta_sig ~ cauchy(0,1);
beta ~ normal(0,1);
}"


bayes_iv_re_list<-list(
  N<-nrow(bayes_iv_randomeffects_df),
  J<-6,
  K<-length(unique(as.factor(bayes_iv_randomeffects_df$country))),
  violent_resist<-(bayes_iv_randomeffects_df$violent_resist),
  country_ids<-as.numeric(factor(bayes_iv_randomeffects_df$country)),
  X<-bayes_iv_randomeffects_df[,c(7,8,9,4,3,11)],
  Y<-as.integer(bayes_iv_randomeffects_df$autonNS)
)
colnames(bayes_iv_randomeffects_df[,c(7,8,9,4,3,11)])
colnames(bayes_iv_randomeffects_df)


bayes_iv_re_model<-stan(model_code = bayes_iv_re_code,
                        data = bayes_iv_re_list,
                        seed = 123,
                        chains = 4,cores=1,
                        iter = 500)

print(bayes_iv_re_model,pars=c("beta","zeta","zeta_mean"), digits = 4) #pi should be around -0.8
launch_shinystan(bayes_iv_re_model)
#age imputed:
#density imputed:
