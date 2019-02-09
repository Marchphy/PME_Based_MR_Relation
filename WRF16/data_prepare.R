library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(foreach)
library(doParallel)
library(shinystan)
library(goftest)
cores = detectCores()


data = read.csv("./WRF16/WRF16.csv")
data= data[1:73,] # exclude TVV

# missing values in the data are put last
data = data[order(data$M_obs),]
data = data[data$M_obs >= 0 | is.na(data$M_obs),]
M_obs = data[,2][!is.na(data[,2])]
R_obs = data[,4]
tau_R = data[,5]
tau_M = data[,3]


