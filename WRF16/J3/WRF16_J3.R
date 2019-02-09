K = 5
# function for parameter initialization
init_WRF16_J3 = function()
{
  list(w = rep(1/K, K),
       sigma_sq = 0.5,
       R = runif(length(R_obs), min(R_obs), max(R_obs)),
       M = runif(length(R_obs), 0,  30),
       C1 = runif(1, 0, 10),
       B_s = c(1/3, 1/3, 1/3),
       alpha = runif(1, 0.5, 1),
       gamma1 = runif(1, 2, 4),
       gamma2 = runif(1, 2, 4),
       gamma3 = runif(1, 2, 4))
}

model_WRF16_J3 = stan_model("./WRF16/J3/model_WRF16_J3.stan")


fit_WRF16_J3 = sampler(model_WRF16_J3, M_obs, R_obs, tau_M, tau_R, K, 
                       pars = c('w', 'sigma_sq', 'R', 'M', 'C1', 'C2', 'C3', 'gamma1', 'gamma2', 'gamma3', 'B_s', 'B'),
                       init_WRF16_J3, 20000, 10000, 5)

# launch_shinystan(fit_WRF16_J3)


