roxygen2::roxygenize()
devtools::document()
devtools::check()
devtools::install()
devtools::build()
devtools::use_testthat()


usethis::use_testthat()
usethis::use_github_actions()
usethis::use_coverage(type = c("codecov"))

usethis::use_test(name = "gen_data_example")

use_github_actions_badge(name = "R-CMD-check")
usethis::use_mit_license()

library(magrittr)
library(tidyverse)
library(qordstan)
library(rstan)
library(covr)

codecov(token = "cbaab2b6-52fb-43c4-bfcb-c5cfb849ae6a")

data = gen_data_example()
fit = qord_fit(data$x, data$y, q = 0.5)

fit
a = summary(fit)
a$summary_table
a
class(summary(fit))





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
  p = ncol(x),
  n = nrow(x),
  beta_scale = beta_scale,
  delta_scale = delta_scale
)

model = stan_model("inst/stan/model.stan", model_name = 'qord')


model_fit = rstan::sampling(
  model, data = stan_data_list,
  pars = c("beta", "gamma", "log_lik", "linear_pred"), iter = mcmc_samples,
  warmup = warmup_samples, thin = thin, chains = chains
)

theta = (1-2*q)/(q*(1-q))
tau = sqrt(2/(q*(1-q)))
am = model_fit %>% rstan::extract()

w = replicate(n = dim(am$linear_pred)[2], expr = {rexp(n=dim(am$linear_pred)[1])})
u = replicate(n = dim(am$linear_pred)[2], expr = {rnorm(n=dim(am$linear_pred)[1])})


z = am$linear_pred + theta*w + tau*sqrt(w)*u
z[,1]

am$log_lik %>% dim()
am$linear_pred %>% dim()
am$z[,1]

am$z[,1]
y_hat = matrix(NA, 1000, 900)


am$z %>% dim()

for(i in 1:1000) {
  y_hat[i, ] = am$z[i,] %>%
    cut(breaks = c(-Inf, 0, am$gamma[i,], Inf), labels = 1:5)
}

get_cut = function(z, gamma) {
  cut_breaks = c(-Inf, 0, gamma, Inf)
  z %>%
    cut(breaks = cut_breaks, labels = 1:(length(cut_breaks)-1)) %>%
    as.character() %>%
    as.numeric()
}


lapply(1:1000, FUN = function(i){get_cut(z[i,], gamma[i,])})

poster

i=1
get_cut(z[i,], am$gamma[i,])

lapply(1:)

lapply(1:1000, FUN=function(i) get_cut(am$z[i,], am$gamma[i,]))

get_cut = Vectorize(get_cut)

get_cut(am$z, am$gamma)

am$beta %>% dim()

data$x %>% dim()
head(data$x%*%am$beta[1,])

linear_pred = data$x%*%t(am$beta)


am$linear_pred[1:10, 1:3]
pred_linear = t(pred_linear_c)

alpha = 0.05
q = y_hat %>% apply(quantile, MARGIN = 2, p = c(alpha/2, 1-alpha/2)) %>% t()
pred = cbind(q, data$y)

mean((pred[,3] <= pred[,2]) & (pred[,3] >= pred[,1]))


am$gamma[1,]

am$y_sim %>% dim()
am$y_sim[,4] %>% table() %>% prop.table()
data$y[4]

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








