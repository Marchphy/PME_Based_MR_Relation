functions {
  real MixlogN_lpdf(vector y, vector w, vector mu, real sigma) {
    real lprob = 0.0;
    int N = num_elements(y);
    int K = num_elements(w);
    vector[K] contributions;
    for(i in 1:N) {
      for(k in 1:K) {
        contributions[k] = log(w[k]) + lognormal_lpdf(y[i] | mu[k], sigma);
      }
      lprob += log_sum_exp(contributions);
    }
    return lprob;
  }
  
  real GN_lpdf(real y, real mu, real alpha, real beta) {
    real lprob = log(beta) - log(2*alpha) - lgamma(1./beta) - (fabs(y - mu) / alpha)^beta;
    return lprob;
  }
}

data {
  int<lower=1> K; // number of mixture components
  int<lower=1> N; // number of observations
  int<lower=1> N_obs; //number of observed M_obs
  vector<lower=0>[N] R_obs; 
  vector[K] q_log_R_obs; 
  vector<lower=0>[N_obs] M_obs; 
  vector<lower=0>[N] tau_R; 
  vector<lower=0>[N] tau_M; 
}



transformed data {
  int<lower=1> N_mis;
  N_mis = N - N_obs;
}

parameters {
  simplex[K] w; // mixing proportions
  real<lower=0> sigma_sq;
  vector<lower=0>[N] R;
  real<lower=min(R_obs)-0.02> B1;
  vector<lower=0>[N] M;
  vector<lower=0>[N_mis] M_mis;
  real<lower=0> C1;
  real<lower=0> gamma1;
  real<lower=0> gamma2;
}


transformed parameters{
  real<lower=0> sigma;
  real<lower=0> C2;
  vector<lower=0>[N] lambda;
  sigma = sqrt(sigma_sq);
  C2 = C1 / B1^(gamma2 - gamma1);
  for(i in 1:N){
    if(R[i] <= B1) {
      lambda[i] = 1 / (C1 * R[i] ^ gamma1);
    }
    else {
      lambda[i] = 1 / (C2 * R[i] ^ gamma2);
    }
  }
}



model {
  // priors
  w ~ dirichlet(rep_vector(1, K));
  sigma_sq ~ cauchy(0, 5)T[0,];
  gamma1 ~ GN(2, 2, 6);
  gamma2 ~ GN(2, 2, 6);
  C1 ~ normal(0, 5)T[0,];
  R ~ MixlogN(w, q_log_R_obs, sigma);
  B1 ~ GN(2, 4, 6);

  
  M ~ exponential(lambda);
  for(i in 1:N){
    R_obs[i] ~ normal(R[i], tau_R[i]);
  }
  for(i in 1:N_obs){
    M_obs[i] ~ normal(M[i], tau_M[i]);
  }
  for(i in 1:N_mis){
    M_mis[i] ~ normal(M[i+N_obs], tau_M[i+N_obs]);
  }
}

