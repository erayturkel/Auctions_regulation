library(rstan)

options(mc.cores = parallel::detectCores())

myiters<-100
mywarmup<-50
myadapt<-0.8
n_chains <- 2

fitlong<-stan(file ="stan_model.stan",data = stan_input,seed=123,chains = n_chains,
              iter = myiters,warmup = mywarmup,init=inits,verbose = TRUE,save_warmup = TRUE,
              control = list(adapt_delta = myadapt,max_treedepth = 15))
saveRDS(fitlong, file = "fitlong.rds")