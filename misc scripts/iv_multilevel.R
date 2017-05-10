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
                           nonviolent_resist.l1,
                           violent_resist_weighted,
                           age_imputed,
                           density_imputed,
                           settler_pop) %>% na.omit()

bayes_iv_code<-"
data{
int N; //number of observations
int J; //number of covariates
vector[N] density_imputed; //instrument
int country_ids[N];
int violent_resist_weighted[N]; //endogenous variable
int Y[N]; //dependent variable
vector[N] csyear1;
vector[N] csyear2;
vector[N] rr_std;
vector[N] literacy;
vector[N] Netherlands;
vector[N] Portugal;
vector[N] French;
vector[N] British;
vector[N] africa;
vector[N] asia;
vector[N] lamerica;
vector[N] settler_pop;
vector[N] constborder;
vector[N] nonviolent_resist;
}
parameters{
real alpha1;
real alpha2;
real pi;
real gamma;
real zeta;
vector[N] density_imputed_measured;

real csyear1_mean_fs;
real csyear2_mean_fs;
real rr_std_mean_fs;
real literacy_mean_fs;
real Netherlands_mean_fs;
real Portugal_mean_fs;
real French_mean_fs;
real British_mean_fs;
real africa_mean_fs;
real asia_mean_fs;
real lamerica_mean_fs;
real settler_pop_mean_fs;
real constborder_mean_fs;
real nonviolent_resist_mean_fs;

real<lower=0> csyear1_sigma_fs;
real<lower=0> csyear2_sigma_fs;
real<lower=0> rr_std_sigma_fs;
real<lower=0> literacy_sigma_fs;
real<lower=0> Netherlands_sigma_fs;
real<lower=0> Portugal_sigma_fs;
real<lower=0> French_sigma_fs;
real<lower=0> British_sigma_fs;
real<lower=0> africa_sigma_fs;
real<lower=0> asia_sigma_fs;
real<lower=0> lamerica_sigma_fs;
real<lower=0> settler_pop_sigma_fs;
real<lower=0> constborder_sigma_fs;
real<lower=0> nonviolent_resist_sigma_fs;

real csyear1_mean_ss;
real csyear2_mean_ss;
real rr_std_mean_ss;
real literacy_mean_ss;
real Netherlands_mean_ss;
real Portugal_mean_ss;
real French_mean_ss;
real British_mean_ss;
real africa_mean_ss;
real asia_mean_ss;
real lamerica_mean_ss;
real settler_pop_mean_ss;
real constborder_mean_ss;
real nonviolent_resist_mean_ss;

real<lower=0> csyear1_sigma_ss;
real<lower=0> csyear2_sigma_ss;
real<lower=0> rr_std_sigma_ss;
real<lower=0> literacy_sigma_ss;
real<lower=0> Netherlands_sigma_ss;
real<lower=0> Portugal_sigma_ss;
real<lower=0> French_sigma_ss;
real<lower=0> British_sigma_ss;
real<lower=0> africa_sigma_ss;
real<lower=0> asia_sigma_ss;
real<lower=0> lamerica_sigma_ss;
real<lower=0> settler_pop_sigma_ss;
real<lower=0> constborder_sigma_ss;
real<lower=0> nonviolent_resist_sigma_ss;

vector[J] csyear1_mean_vec;
vector[J] csyear2_mean_vec;
vector[J] rr_std_mean_vec;
vector[J] literacy_mean_vec;
vector[J] Netherlands_mean_vec;
vector[J] Portugal_mean_vec;
vector[J] French_mean_vec;
vector[J] British_mean_vec;
vector[J] africa_mean_vec;
vector[J] asia_mean_vec;
vector[J] lamerica_mean_vec;
vector[J] settler_pop_mean_vec;
vector[J] constborder_mean_vec;
vector[J] nonviolent_resist_mean_vec;

vector[J] csyear1_mean_vec2;
vector[J] csyear2_mean_vec2;
vector[J] rr_std_mean_vec2;
vector[J] literacy_mean_vec2;
vector[J] Netherlands_mean_vec2;
vector[J] Portugal_mean_vec2;
vector[J] French_mean_vec2;
vector[J] British_mean_vec2;
vector[J] africa_mean_vec2;
vector[J] asia_mean_vec2;
vector[J] lamerica_mean_vec2;
vector[J] settler_pop_mean_vec2;
vector[J] constborder_mean_vec2;
vector[J] nonviolent_resist_mean_vec2;

}
model{
//density_imputed_measured ~ normal(density_imputed,25);  

for(i in 1:N){
violent_resist_weighted[i] ~ bernoulli_logit( alpha1 + density_imputed*pi + 
csyear1_mean_vec[country_ids[i]]*csyear1[i]+
csyear2_mean_vec[country_ids[i]]*csyear2[i]+
rr_std_mean_vec[country_ids[i]]*rr_std[i]+
literacy_mean_vec[country_ids[i]]*literacy[i]+
Netherlands_mean_vec[country_ids[i]]*Netherlands[i]+
Portugal_mean_vec[country_ids[i]]*Portugal[i]+
French_mean_vec[country_ids[i]]*French[i]+
British_mean_vec[country_ids[i]]*British[i]+
africa_mean_vec[country_ids[i]]*africa[i]+
asia_mean_vec[country_ids[i]]*asia[i]+
lamerica_mean_vec[country_ids[i]]*lamerica[i]+
settler_pop_mean_vec[country_ids[i]]*settler_pop[i]+
constborder_mean_vec[country_ids[i]]*constborder[i]+
nonviolent_resist_mean_vec[country_ids[i]]*nonviolent_resist[i] );

Y[i] ~ bernoulli_logit( alpha2 + density_imputed*pi*zeta + 
csyear1_mean_vec2[country_ids[i]]*csyear1[i]+
csyear2_mean_vec2[country_ids[i]]*csyear2[i]+
rr_std_mean_vec2[country_ids[i]]*rr_std[i]+
literacy_mean_vec2[country_ids[i]]*literacy[i]+
Netherlands_mean_vec2[country_ids[i]]*Netherlands[i]+
Portugal_mean_vec2[country_ids[i]]*Portugal[i]+
French_mean_vec2[country_ids[i]]*French[i]+
British_mean_vec2[country_ids[i]]*British[i]+
africa_mean_vec2[country_ids[i]]*africa[i]+
asia_mean_vec2[country_ids[i]]*asia[i]+
lamerica_mean_vec2[country_ids[i]]*lamerica[i]+
settler_pop_mean_vec2[country_ids[i]]*settler_pop[i]+
constborder_mean_vec2[country_ids[i]]*constborder[i]+
nonviolent_resist_mean_vec2[country_ids[i]]*nonviolent_resist[i]); 
}


csyear1_mean_vec ~ normal(csyear1_mean_fs,csyear1_sigma_fs);
csyear2_mean_vec ~ normal(csyear2_mean_fs,csyear2_sigma_fs);
rr_std_mean_vec ~ normal(rr_std_mean_fs, rr_std_sigma_fs);
literacy_mean_vec ~ normal(literacy_mean_fs, literacy_sigma_fs);
Netherlands_mean_vec ~ normal(Netherlands_mean_fs, Netherlands_sigma_fs);
Portugal_mean_vec ~ normal(Portugal_mean_fs, Portugal_sigma_fs);
French_mean_vec ~ normal(French_mean_fs, French_sigma_fs);
British_mean_vec ~ normal(British_mean_fs, British_sigma_fs);
africa_mean_vec ~ normal(africa_mean_fs, africa_sigma_fs);
asia_mean_vec ~ normal(asia_mean_fs, asia_sigma_fs);
lamerica_mean_vec ~ normal(lamerica_mean_fs, lamerica_sigma_fs);
settler_pop_mean_vec ~ normal(settler_pop_mean_fs, settler_pop_sigma_fs);
constborder_mean_vec ~ normal(constborder_mean_fs, constborder_sigma_fs);
nonviolent_resist_mean_vec ~ normal(nonviolent_resist_mean_fs, nonviolent_resist_sigma_fs);

csyear1_mean_vec2 ~ normal(csyear1_mean_ss,csyear1_sigma_ss);
csyear2_mean_vec2 ~ normal(csyear2_mean_ss,csyear2_sigma_ss);
rr_std_mean_vec2 ~ normal(rr_std_mean_ss, rr_std_sigma_ss);
literacy_mean_vec2 ~ normal(literacy_mean_ss, literacy_sigma_ss);
Netherlands_mean_vec2 ~ normal(Netherlands_mean_ss, Netherlands_sigma_ss);
Portugal_mean_vec2 ~ normal(Portugal_mean_ss, Portugal_sigma_ss);
French_mean_vec2 ~ normal(French_mean_ss, French_sigma_ss);
British_mean_vec2 ~ normal(British_mean_ss, British_sigma_ss);
africa_mean_vec2 ~ normal(africa_mean_ss, africa_sigma_ss);
asia_mean_vec2 ~ normal(asia_mean_ss, asia_sigma_ss);
lamerica_mean_vec2 ~ normal(lamerica_mean_ss, lamerica_sigma_ss);
settler_pop_mean_vec2 ~ normal(settler_pop_mean_ss, settler_pop_sigma_ss);
constborder_mean_vec2 ~ normal(constborder_mean_ss, constborder_sigma_ss);
nonviolent_resist_mean_vec2 ~ normal(nonviolent_resist_mean_ss, nonviolent_resist_sigma_ss);



//priors
//gamma ~ normal(0,0.0125*.2);
alpha1 ~ normal(0,5);
alpha2 ~ normal(0,5);
pi ~ normal(0,5);
zeta ~ normal(0,5);
}"

bayes_iv_list<-list(
  N<-nrow(bayes_iv_df),
  J<-46,
  density_imputed<-bayes_iv_df$density_imputed,
  country_ids <- as.numeric(factor(bayes_iv_df$country)),
  violent_resist_weighted<-as.integer(bayes_iv_df$violent_resist),
  Y<-as.integer(bayes_iv_df$autonNS),
  csyear1<-(bayes_iv_df$csyear1),
  csyear2<- bayes_iv_df$csyear2,
  rr_std <- bayes_iv_df$rr_std,
  literacy <- bayes_iv_df$literacy,
  Netherlands <- bayes_iv_df$Netherlands,
  Portugal <- bayes_iv_df$Portugal,
  French <- bayes_iv_df$French,
  British <- bayes_iv_df$British,
  africa <- bayes_iv_df$africa,
  asia <- bayes_iv_df$asia,
  lamerica <- bayes_iv_df$lamerica,
  settler_pop <-bayes_iv_df$settler_pop,
  constborder <- bayes_iv_df$constborder,
  nonviolent_resist <- bayes_iv_df$nonviolent_resist.l1
)
colnames(bayes_iv_df)
colnames(bayes_iv_df[,c(4,5,7,8,17,18,20,21,12,14,13,27,9,23)])
bayes_iv_model<-stan(model_code = bayes_iv_code,
                     data = bayes_iv_list,
                     seed = 123,
                     chains = 1,cores=1,
                     iter = 1000)

print(bayes_iv_model,pars=c("zeta","csyear1_mean_vec2","csyear1_mean_ss"), digits = 4)
#launch_shinystan(bayes_iv_model)


