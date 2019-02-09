sampler = function(stan_model, M_obs, R_obs, tau_M, tau_R, K, 
                   pars = NA, inits = "random", iter = 50000, warmup = 20000, chains = 1)
{
  N = length(R_obs)
  N_obs = length(M_obs)
  #quantiles of the observed log(R)
  log_R_obs = log(R_obs)
  prob_list = seq(1,K)/(K+1)
  q_log_R_obs = quantile(log_R_obs, prob_list)
  observed_data = list(K = K, N = N, N_obs = N_obs, q_log_R_obs = q_log_R_obs,
                       R_obs = R_obs, M_obs = M_obs, tau_R = tau_R, tau_M = tau_M)
  #generate posterior information
  return(sampling(stan_model,
                  data = observed_data, 
                  iter = iter,
                  warmup = warmup,
                  init = inits,
                  pars = pars,
                  chains = chains,
                  control = list(adapt_delta = 0.99, max_treedepth = 10)))
}


#' calculate the posterior mean/median
#'
#' @param var the parameter to be estimated, e.g. "M"
#' @param stan_fit the fitted stan model
#' @param stan_model the compiled stan model
#' @return a data frame est_dat, with the second colume indicating the typr of estimates(mean/median/mode)
pos_estimate = function(var, stan_fit)
{
  require(rstan)
  require(dplyr)
  est_list = list()
  mean_median = summary(stan_fit, pars = c(var), probs = c(0.5))$summary
  n_rows = dim(mean_median)[1]
  est_list[[1]] = data.frame(cbind(var = mean_median[,"mean"],  method = rep("mean", n_rows)), stringsAsFactors=FALSE)
  est_list[[2]] = data.frame(cbind(var = mean_median[,"50%"],  method = rep("median", n_rows)), stringsAsFactors=FALSE)
  
  est_dat = bind_rows(est_list)
  est_dat$var = as.numeric(est_dat$var)
  return(est_dat)
}


MR_plotData_J2 = function(stan_fit, Rlist)
{
  require(foreach)
  require(doParallel)
  require(rstan)
  paras = as.matrix(stan_fit, pars = c("C1", "gamma1", "C2", "gamma2", "B1"))
  #set up parallel enviroment
  cores = detectCores()
  cl = makeCluster(cores[1]-1, type = "FORK", outfile = "monitor.txt")
  registerDoParallel(cl)
  estimates = foreach(i = 1:length(Rlist), .combine = cbind) %dopar%
  {
    R = Rlist[i]
    M_R_1 = function(x)
    {
      if(R <= x[5])
        return(x[1] * R ** x[2])
      else
        return(x[3] * R ** x[4])
    }
    expect_M = apply(paras, 1, M_R_1)
    
    M_R_2 = function(x)
    {
      if(R <= x[5])
        return(rexp(1, 1/(x[1] * R ** x[2])))
      else
        return(rexp(1, 1/(x[3] * R ** x[4])))
    }
    pred_M = apply(paras, 1, M_R_2)
    c(expect_M, pred_M)
  }
  stopCluster(cl)
  
  size = dim(paras)[1]
  expect_M = estimates[1:size,]
  pred_M = estimates[(size+1):(2*size),]
  
  
  expect_M_mean = colMeans(expect_M)
  expect_M_Q50 = apply(expect_M, 2, median)
  expect_M_Q16 = apply(expect_M, 2, quantile, probs=c(0.16))
  expect_M_Q84 = apply(expect_M, 2, quantile, probs=c(0.84))
  expect_M_est = data.frame(R = Rlist,
                            expect_M_mean = expect_M_mean,
                            expect_M_Q50 = expect_M_Q50,
                            expect_M_Q16 = expect_M_Q16,
                            expect_M_Q84 = expect_M_Q84)
  
  pred_M_mean = colMeans(pred_M)
  pred_M_Q50 = apply(pred_M, 2, median)
  pred_M_Q16 = apply(pred_M, 2, quantile, probs=c(0.16))
  pred_M_Q84 = apply(pred_M, 2, quantile, probs=c(0.84))
  pred_M_est = data.frame(R = Rlist,
                          pred_M_mean = pred_M_mean,
                          pred_M_Q50 = pred_M_Q50,
                          pred_M_Q16 = pred_M_Q16,
                          pred_M_Q84 = pred_M_Q84)

  return(list(expect_M_est, pred_M_est))
}



MR_plotData_J3 = function(stan_fit, Rlist)
{
  require(foreach)
  require(doParallel)
  require(rstan)
  paras = as.matrix(stan_fit, pars = c("C1", "gamma1", "C2", "gamma2", "C3", "gamma3", "B"))
  #set up parallel enviroment
  cores = detectCores()
  cl = makeCluster(cores[1]-1, type = "FORK", outfile = "monitor.txt")
  registerDoParallel(cl)
  estimates = foreach(i = 1:length(Rlist), .combine = cbind) %dopar%
  {
    R = Rlist[i]
    M_R_1 = function(x)
    {
      if(R <= x[7])
        return(x[1] * R ** x[2])
      else if(R <= x[8])
        return(x[3] * R ** x[4])
      else
        return(x[5] * R ** x[6])
    }
    expect_M = apply(paras, 1, M_R_1)
    
    M_R_2 = function(x)
    {
      if(R <= x[7])
        return(rexp(1, 1/(x[1] * R ** x[2])))
      else if(R <= x[8])
        return(rexp(1, 1/(x[3] * R ** x[4])))
      else
        return(rexp(1, 1/(x[5] * R ** x[6])))
    }
    pred_M = apply(paras, 1, M_R_2)
    c(expect_M, pred_M)
  }
  stopCluster(cl)
  
  size = dim(paras)[1]
  expect_M = estimates[1:size,]
  pred_M = estimates[(size+1):(2*size),]
  
  
  expect_M_mean = colMeans(expect_M)
  expect_M_Q50 = apply(expect_M, 2, median)
  expect_M_Q16 = apply(expect_M, 2, quantile, probs=c(0.16))
  expect_M_Q84 = apply(expect_M, 2, quantile, probs=c(0.84))
  expect_M_est = data.frame(R = Rlist,
                            expect_M_mean = expect_M_mean,
                            expect_M_Q50 = expect_M_Q50,
                            expect_M_Q16 = expect_M_Q16,
                            expect_M_Q84 = expect_M_Q84)
  
  pred_M_mean = colMeans(pred_M)
  pred_M_Q50 = apply(pred_M, 2, median)
  pred_M_Q16 = apply(pred_M, 2, quantile, probs=c(0.16))
  pred_M_Q84 = apply(pred_M, 2, quantile, probs=c(0.84))
  pred_M_est = data.frame(R = Rlist,
                          pred_M_mean = pred_M_mean,
                          pred_M_Q50 = pred_M_Q50,
                          pred_M_Q16 = pred_M_Q16,
                          pred_M_Q84 = pred_M_Q84)
  
  return(list(expect_M_est, pred_M_est))
}
