library(rstan)

slope_vary_df<-dplyr::select(decolonization,year,autonNS,
                             country,rr_std,
                             violent_resist,
                             nonviolent_resist,
                             violent_resist_weighted) %>% na.omit()

slope_vary_df<-slope_vary_df[order(slope_vary_df$year),]
slope_vary_code<-"
data{
int N; //number of observations
int J; //number of covariates
int Y[N]; //dependent variable
matrix[N,J] X; //covariates
int K; //number of countries
int country_ids[N];
vector[N] violent_resist_data;
}
parameters{
vector[J] beta;
real alpha;
vector[K] violent_resist;
//vector[K] country_intercept;
//real country_mean;
//real<lower=0> country_sig;
//real violent_resist_mean;
//real<lower=0> violent_resist_sigma;
}
model{
for(i in 1:N){
Y[i] ~ bernoulli_logit( alpha + violent_resist[country_ids[i]]*violent_resist_data[i] + X[i,]*beta ); 
}

//priors
alpha ~ normal(0,5);
//beta ~ normal(0,5);
violent_resist ~ normal(0,1);
//violent_resist_mean ~ normal(0,2.5);
//violent_resist_sigma ~ cauchy(0,1);
//country_intercept ~ normal(country_mean,country_sig);
//country_mean ~ normal(0,.5);
//country_sig ~ cauchy(0,.5);

}"

slope_vary_list<-list(
  N<-nrow(slope_vary_df),
  country_ids <- as.numeric(factor(slope_vary_df$year)),
  K<- length(unique(as.numeric(factor(slope_vary_df$year)))),
  violent_resist_data<-as.integer(slope_vary_df$violent_resist),
  X<-slope_vary_df[,c(7)],
  J<-1,
  Y<-as.integer(slope_vary_df$autonNS)
)


slope_vary_model<-stan(model_code = slope_vary_code,
                     data = slope_vary_list,
                     seed = 123,
                     chains = 4,cores=4,
                     iter = 200)

print(slope_vary_model,pars=c("violent_resist","beta"))
