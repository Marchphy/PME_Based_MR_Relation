K = 5
# function for parameter initialization
init_WRF16_J2 = function()
{
  list(w = rep(1/K, K),
       sigma_sq = 0.5,
       R = runif(length(R_obs), min(R_obs), max(R_obs)),
       M = runif(length(R_obs), 0,  20),
       C1 = runif(1, 0, 20),
       B1 = runif(1, 2, 4),
       gamma1 = runif(1, 2, 4),
       gamma2 = runif(1, 2, 4))
} 


model_WRF16_J2 = stan_model("./WRF16/J2/model_WRF16_J2.stan")
fit_WRF16_J2 = sampler(model_WRF16_J2, M_obs, R_obs, tau_M, tau_R, K, pars = c('w', 'sigma_sq', 'R', 'M', 'C1', 'C2', 'gamma1', 'gamma2', 'B1'),
                        init_WRF16_J2, 70000, 50000, 5)
launch_shinystan(fit_WRF16_J2)









