rm(list = ls())
roxygen2::roxygenize()
devtools::document()
devtools::test()
codecov(token = "cbaab2b6-52fb-43c4-bfcb-c5cfb849ae6a")
devtools::check()
devtools::install()
devtools::build()
devtools::use_testthat()
usethis::use_test("gen_data_example")
usethis::use_github_actions()
usethis::use_coverage(type = c("codecov"))
dir.create("/home/pedro/tmp/")
Sys.setenv(TMPDIR="/home/pedro/tmp/")
usethis::use_test(name = "gen_data_example")

usethis::use_github_actions_badge(name = "R-CMD-check")
usethis::use_mit_license()

install.packages('ggplot2')

library(magrittr)
library(tidyverse)
library(qordstan)
library(rstan)
library(covr)
library(bqror)

?quantreg_or1

data$gamma
data$y %>% table() %>% prop.table()


data = gen_data_example(n=3000, k = 3, seed = 1, p = 6)

data$example_df %>% names()

formula = y ~ X1 + X2 + X3 + X1*X2

data_variables = model.frame(formula = formula, data = data$example_df)
response_name = all.vars(formula)[1]
response_name
x = model.matrix(formula, data = data$example_df)[, -1]
y = data_variables[, response_name]

x %>% head()
y
covariate_name = names(x)

data$example_df %>% head()
colnames(data$example_df) = c("aa", "bb", "cc", "dd", "ee", "ff", "my_response")

fit = qord_fit(my_response ~ ., q = 0.5,
               data = data$example_df, iter = 200, warmup = 100)

fit
fit %>% summary()

data$b


model.frame(y ~ x + x2 + x:x2, data = data.frame(x = data$x[,1], y = data$y, x2 = data$x[,2]))

model.frame(formula = y  ~ X1*X2, data = data$example_df) %>% head()

model.matrix(lm(y ~ X1*X2, data = data$example_df)) %>% head()

ff <- log(Volume) ~ log(Height) + log(Girth)
utils::str(m <- model.frame(ff, trees))


mat <- model.matrix(y ~ X1*X2, data$example_df)
mat %>% head()
x <- data$x
y <- data$y
k <- dim(x)[2]
J <- length(unique(as.numeric(y)))
D0 <- 0.25*diag(J - 2)



output <- quantreg_or1(y = y,x = x, B0 = 10*diag(k), D0 = D0,
                       mcmc = 1000, p = 0.5, tune = 1)

output$logMargLikelihood
output$postMeanbeta
data$b

fit = qord_fit(x = x, y = as.numeric(y), q = 0.5, beta_scale = 10, delta_scale = 0.25,
               iter = 100, warmup = 50)

sm = summary(fit)
sm$summary_table
sm$beta_mean
x = sm$waic
sm$waic$estimates['waic', 'Estimate']

sm


sm$summary_table[, 'mean']

log_lik = loo::extract_log_lik(fit$stan_fit)

x = rstan::get_logposterior(fit$stan_fit)
x %>% class()


x %>% unlist() %>% mean()
loo::waic(log_lik)

lo

mean(output$postStdbeta > sm$summary_table[, 'std'][1:6])
output$beta[1,] %>% acf()
fit$posterior_sample$beta[,1] %>% acf()

mean(abs(data$b - output$postMeanbeta))
mean(abs(data$b - sm$summary_table[1:6]))


quantile(resp, probs = c(0.25, 0.5, 0.75))

devtools::load_all()




data = gen_data_example(n=3000, k = 6, seed = 1, p = 6)



data$y %>% table()
fit = qord_fit(data$x, data$y, q = 0.5)

fit
a = summary(fit)
a$summary_table
a
class(summary(fit))
summary(fit)

data$b
data$gamma


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


stable(x, 5, 100)

x = summary(fit)
x %>% class()

y = printCoefmat(x)

attributes(fit)










