roxygen2::roxygenize()
devtools::document()
devtools::check()
devtools::install()
devtools::build()


usethis::use_mit_license()

library(magrittr)
library(tidyverse)
library(qordstan)
library(rstan)




cmat <- cbind(rnorm(3, 10), sqrt(rchisq(3, 12)))
cmat <- cbind(cmat, cmat[, 1]/cmat[, 2])
cmat <- cbind(cmat, 2*pnorm(-cmat[, 3]))
colnames(cmat) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")
cmat
printCoefmat(cmat[, 1:3])
printCoefmat(cmat)


data = gen_data_example()

x = data$x
y = data$y
q = 0.5
beta_scale = 1
delta_scale = 0.25
mcmc_samples = 2000
warmup_samples = 1000
thin = 1
chains = 1

stan_data_list = list(
  x = x,
  y = y,
  q = q,
  k = length(unique(y)),
  p = ncol(x), n = nrow(x),
  beta_scale = beta_scale,
  delta_scale = delta_scale
)

model = stan_model("inst/stan/model.stan", model_name = 'qord')


model_fit = rstan::sampling(
  model, data = stan_data_list,
  pars = c("beta", "gamma", "log_lik"), iter = mcmc_samples,
  warmup = warmup_samples, thin = thin, chains = chains
)

am = model_fit %>% rstan::extract()
am$log_lik %>% dim()

library(loo)
library(magrittr)
log_lik = loo::extract_log_lik(model_fit)
dim(log_lik)
waic = loo::waic(log_lik)


?loo::waic

fit = qord_fit(data$x, data$y, q = 0.5)
log_lik = loo::extract_log_lik(fit$stan_fit)
waic = loo::waic(log_lik)
waic = 100

stable = function(x, k, waic) {
  cat("Ordinal quantilic regression fit with stan\n")
  cat("Number of categoies:", k, "\n")
  cat("WAIC:" , waic, "\n\n")
  printCoefmat(x)
}

stable(x, 5, 100)

x = summary(fit)
x %>% class()

y = printCoefmat(x)




y %>% class()
qordstan::

summary(fit)

fit
am = fit$stan_fit %>% rstan::extract()

beta_post_mean = am$beta %>% colMeans()
gamma_post_mean = am$gamma %>% colMeans()

posterior_resume = function(x, cred_mass = 0.95) {
  hdi = HDInterval::hdi(x, cred_mass) %>% as.numeric()
  c("mean" = mean(x), "std" = sd(x),"cred LI" = hdi[1], "cred UI" = hdi[2])
}

beta_res = apply(am$beta, posterior_resume, MARGIN = 2) %>% t()
rownames(beta_res) = paste0("beta ", 1:nrow(beta_res))
beta_res
gamma_res = apply(am$gamma, posterior_resume, MARGIN = 2) %>% t()
rownames(gamma_res) = paste0("gamma", 1:nrow(gamma_res))


res = rbind(beta_res, gamma_res)
res

fit
fit$

attributes(fit)

fit@p


print(fit)

class(fit)

class(fit)

fit$stan_fit %>% class()

summary(fit)


summary(fit)


am = fit %>% rstan::extract()
am$gammas %>% colMeans()
data$b
data$deltas
data$gammas
plot(fit)



new_difftime <- function(x = double(), units = "secs") {
  stopifnot(is.double(x))
  units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))

  structure(x,
            class = "difftime",
            units = units
  )
}








