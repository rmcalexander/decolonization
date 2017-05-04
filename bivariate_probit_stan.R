library(rstan)
bv_mat<-ivprobitdf
ivprobitdf[,ncol(ivprobitdf)+1]<-1

bv_data<-list(
  K<-as.integer(11),
  D<-as.integer(2),
  N<-as.integer(308),
  y<-as.matrix(cbind(as.integer(ivprobitdf$autonNS),as.integer(ivprobitdf$violent_resist))),
  x<-ivprobitdf[,c(11,3,17,15,8,9,23,16,4,7,ncol(ivprobitdf))],
  instr<-exp(ivprobitdf$age_log)
)
colnames(ivprobitdf)
colnames(ivprobitdf[,c(11,3,17,15,8,9,23,16,4,7,ncol(ivprobitdf))])
bv_code<-"functions { // define sum function
  int sum(int[,] a) {
    int s;
    s = 0;
    for (i in 1:size(a))
      for (j in 1:size(a[i]))
        s = s + a[i,j];
      return s;
  }
}
data {
  int<lower=1> K;/// number of independent variables, including intercept
  int<lower=1> D; //number of equations, in our case 2 (first and second stage)
  int<lower=0> N; // number of observations in dataset
  int<lower=0,upper=1> y[N,D]; // DVs in first and second stage, i.e. Outcome and Treatment
  vector[K] x[N]; // matrix of independent variables (including an intercept vector)
  real instr[N]; //instrument
}
transformed data { // counting how many in outcome and treatment vectors are 0 and 1s
  //create matrix with indicator for each
  matrix[N,D] y2; //copy of y
  int<lower=0> N_pos;
  int<lower=1,upper=N> n_pos[sum(y)];
  int<lower=1,upper=D> d_pos[size(n_pos)];
  int<lower=0> N_neg;
  int<lower=1,upper=N> n_neg[(N *D) - size(n_pos)];
  int<lower=1,upper=D> d_neg[size(n_neg)];
  N_pos = size(n_pos);
  N_neg = size(n_neg);
  {
    int i;
    int j;
    i = 1;
    j = 1;
    for (n in 1:N) {
      for (d in 1:D) {
        if (y[n,d] == 1) {
          n_pos[i] = n;
          d_pos[i] = d;
          i = i + 1;
        } else {
          n_neg[j] = n;
          d_neg[j] = d;
          j = j + 1;
        }
      }
    }
  }
  y2 = to_matrix(y);
}
parameters {
  matrix[D,K] beta; // coefficients for independent variables
  real beta2; // coefficient for treatment
  real pi; // coefficient for instrument
  cholesky_factor_corr[D] L_Omega; // cholesky decomposition of correlation matrix
  vector<lower=0>[N_pos] z_pos;
  // latent parameter of outcomes & treatment = 1, constrained so that minimum 0
  vector<upper=0>[N_neg] z_neg;
  // latent parameter of outcomes & treatment = 0, constrained so that maximum 0
}
transformed parameters {
  vector[D] z[N];
  // vector z is the transformed data for the normal draw, i.e. negative if y = 0, positive if y = 1
  for (n in 1:N_pos)
    z[n_pos[n], d_pos[n]] = z_pos[n];
  for (n in 1:N_neg)
    z[n_neg[n], d_neg[n]] = z_neg[n];
}
model {
  vector[N] treatment;
  treatment = col(y2,1);
  // assigning treatment to new vector, so it can be used for calculation of second stage
  to_vector(beta) ~ normal(0, 5); // prior on coefficients on IVs
  beta2 ~ normal(0, 5); // prior on treatment effect
  pi ~ normal(0, 5); // prior on instrument
  L_Omega ~ lkj_corr_cholesky(D); // prior on covariance decomp
  {
    vector[D] beta_x[N];
    //transposing the mean vector by hand so it can be used in the multi normal draw
    for(d in 1:D){
      for (n in 1:N){ //creating means for draw
        beta_x[n,1] = row(beta,1)
        *
          x[n] + pi
        *
          instr[n]; // first stage
        beta_x[n,2] = row(beta,2)
        *
          x[n] + treatment[n]
        *
          beta2; //second stage
      }
    }
    z ~ multi_normal_cholesky(beta_x, L_Omega); // draw from multinormal
  }
}

"

bv_model<-stan(model_code = bv_code,
                     data = bv_data,
                     seed = 123,
                     chains = 8,cores=1,
                     iter = 5000)

print(bv_model,pars=c("beta","beta2"), digits = 4)
