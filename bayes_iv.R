library(rstan)

bayes_iv_df<-dplyr::select(decolonization,year,
                    country,
                    csyear1,
                    csyear2,
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
                    nonviolent_resist,
                    violent_resist_weighted,
                    age_imputed,
                    density_imputed,
                    settler_pop) %>% na.omit()

bayes_iv_code<-"
data{
  int N; //number of observations
  int J; //number of covariates
  vector[N] density_imputed; //instrument
  int violent_resist_weighted[N]; //endogenous variable
  int Y[N]; //dependent variable
  matrix[N,J] X; //covariates
}
parameters{
  vector[J] beta1;
  vector[J] beta2;
  real alpha1;
  real alpha2;
  real pi;
  real zeta;
}
model{
  
for(i in 1:N){
violent_resist_weighted[i] ~ bernoulli_logit( alpha1 + density_imputed[i]*pi + X[i,]*beta1);
Y[i] ~ bernoulli_logit( alpha2 + density_imputed[i]*pi*zeta + X[i,]*beta2 ); 
}
  

//priors
  alpha1 ~ normal(0,5);
  alpha2 ~ normal(0,5);
  beta1 ~ normal(0,5);
  beta2 ~ normal(0,5);
  pi ~ normal(0,5);
  zeta ~ normal(0,5);
}"

bayes_iv_list<-list(
  N<-nrow(bayes_iv_df),
  J<-12,
  density_imputed<-bayes_iv_df$density_imputed,
  violent_resist_weighted<-as.integer(bayes_iv_df$violent_resist),
  X<-bayes_iv_df[,c(4,5,7,9,10,12:14,18,20,21,23)],
  Y<-as.integer(bayes_iv_df$autonNS)
)
colnames(bayes_iv_df)
colnames(bayes_iv_df[,c(4,5,7,9,10,12:14,18,20,21,23)])
bayes_iv_model<-stan(model_code = bayes_iv_code,
                     data = bayes_iv_list,
                     seed = 123,
                     chains = 1,cores=1,
                     iter = 5000)

print(bayes_iv_model, digits = 4)
