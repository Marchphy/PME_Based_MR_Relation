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
  vector[N] R_obs; 
  vector[K] q_log_R_obs;
  vector<lower=0>[N_obs] M_obs;
  vector<lower=0>[N] tau_R;
  vector<lower=0>[N] tau_M;
}



transformed data {
  int<lower=1> N_mis;
  real<lower=0> B_lower;
  real<lower=0> B_upper;
  N_mis = N - N_obs;
  B_lower = min(R_obs) - 0.02;
  B_upper = max(R_obs) + 0.62;
  
}


parameters {
  simplex[K] w;
  real<lower=0> sigma_sq;
  vector<lower=0>[N] R; 
  simplex[3] B_s;
  vector<lower=0>[N] M;
  vector<lower=0>[N_mis] M_mis;
  real<lower=0> C1;
  real<lower=0> gamma1;
  real<lower=0> gamma2;
  real<lower=0> gamma3;
}


transformed parameters{
  real<lower=0> sigma;
  positive_ordered[2] B; 
  real<lower=0> C2;
  real<lower=0> C3;
  vector<lower=0>[N] lambda;
  sigma = sqrt(sigma_sq);
  B = B_lower + head(cumulative_sum(B_s), 2) * (B_upper - B_lower); 
  C2 = C1 * B[1]^(gamma1 - gamma2);
  C3 = C2 * B[2]^(gamma2 - gamma3);
  
  for(i in 1:N){
    if(R[i] <= B[1])
      lambda[i] = 1 / (C1 * R[i] ^ gamma1);
    else if(R[i] <= B[2])
      lambda[i] = 1 / (C2 * R[i] ^ gamma2);
    else
      lambda[i] = 1 / (C3 * R[i] ^ gamma3);
  }
}



model {
  vector[3] a;
  a[1] = 1; a[2] = 2; a[3] = 3;
  
  w ~ dirichlet(rep_vector(1, K));
  sigma_sq ~ cauchy(0, 5)T[0,];

  gamma1 ~ GN(2, 2, 6);
  gamma2 ~ GN(2, 2, 6);
  gamma3 ~ GN(2, 2, 6);
  C1 ~ normal(0, 5)T[0,];
  
  R ~ MixlogN(w, q_log_R_obs, sigma);
  B_s ~ dirichlet(a);
  M ~ exponential(lambda);
  
  R_obs ~ normal(R, tau_R);
  for(i in 1:N_obs){
    M_obs[i] ~ normal(M[i], tau_M[i]);
  }
  for(i in 1:N_mis){
    M_mis[i] ~ normal(M[i+N_obs], tau_M[i+N_obs]);
  }
}
