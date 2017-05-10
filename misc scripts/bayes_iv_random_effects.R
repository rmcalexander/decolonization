#random effects model
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
bayes_iv_randomeffects_df<-bayes_iv_randomeffects_df[order(bayes_iv_randomeffects_df$country,bayes_iv_randomeffects_df$year),]
bayes_iv_re_code<-"
data{
int N; //number of observations
int J; //number of covariates
vector[N] Y; //dependent variable
int K; //number of countries
vector[N] density_imputed; //instrument
vector[N] violent_resist; //endogenous variable
matrix[N,J] X;
int country_ids[N];
}
parameters{
vector[K] alpha;
vector[J] beta;
vector[K] alpha0;
vector[J] beta0;
real alpha_mean;
real<lower=0> alpha_sig;
real alpha_mean0;
real<lower=0> alpha_sig0;
real pi;
real zeta;
real global;
real global0;
//real gamma;
real<lower=0> sigma0;
real<lower=0> sigma;
}
model{
for(i in 1:N){
violent_resist[i] ~ normal( alpha0[country_ids[i]] + X[i,]*beta0 + density_imputed[i]*pi , sigma0 );
Y[i] ~ normal( alpha[country_ids[i]] + X[i,]*beta + density_imputed[i]*pi*zeta +density_imputed[i]*0, sigma); 
}

alpha ~ normal(alpha_mean,alpha_sig);
alpha_mean ~ normal(0,5);
alpha_sig ~ cauchy(0,5);
alpha0 ~ normal(alpha_mean0,alpha_sig0);
alpha_mean0 ~ normal(0,5);
alpha_sig0 ~ cauchy(0,5);

//global ~ normal(0,1);
//global0 ~ normal(0,1);
beta ~ normal(0,5);
beta0 ~ normal(0,5);
pi ~ normal(0,5);
zeta ~ normal(0,5);
sigma0 ~ cauchy(0,5);
sigma ~ cauchy(0,5);
//gamma ~ normal(0, 5.868e-02*.2);
}"


bayes_iv_re_list<-list(
  N<-nrow(ivprobitdf),
  J<-10,
  K<-length(unique(as.factor(ivprobitdf$country))),
  density_imputed<-(ivprobitdf$density_imputed),
    violent_resist<-(ivprobitdf$nonviolent_resist),
  country_ids<-as.numeric(factor(ivprobitdf$country)),
  X<-ivprobitdf[,c(10,3,4,16,17,19,15,7,23,8)],
  Y<-(ivprobitdf$autonNS)
)
colnames(ivprobitdf[,c(11,3,4,16,17,19,15,7,23,8)])
colnames(ivprobitdf)

summary(lm(autonNS~as.factor(country)+age_imputed+nsf5neigh+time1+time2+rr_std+nonviolent_resist+literacy,data=bayes_iv_randomeffects_df))
summary(AER::ivreg(autonNS~nonviolent_resist+rr_std+literacy+nsf5neigh+violent_resist+time1+time2+as.factor(country)
                   |density_imputed+rr_std+literacy+nsf5neigh+nonviolent_resist+time1+time2+as.factor(country),
                   data=bayes_iv_randomeffects_df),diagnostics=TRUE)
                     
bayes_iv_re_model<-stan(model_code = bayes_iv_re_code,
                        data = bayes_iv_re_list,
                        seed = 123,
                        chains = 1,cores=1,
                        iter = 500)

print(bayes_iv_re_model,pars=c("beta0","pi","alpha_mean0","alpha_sig0","zeta"), digits = 4) #pi should be around -0.8

#age imputed:
#density imputed:
