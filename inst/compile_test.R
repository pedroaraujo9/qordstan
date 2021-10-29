rm(list = ls())
devtools::document()
devtools::test()
covr::codecov(token = "cbaab2b6-52fb-43c4-bfcb-c5cfb849ae6a")
devtools::check()
devtools::install()
devtools::build()
devtools::use_testthat()
usethis::use_test("print.summary.qordstan")
usethis::use_test("new_qordstan")
usethis::use_test("predict.qordstan")


usethis::use_github_actions()
usethis::use_coverage(type = c("codecov"))
dir.create("/home/pedro/tmp/")
Sys.setenv(TMPDIR="/home/pedro/tmp/")
usethis::use_test(name = "qord_fit")

usethis::use_github_actions_badge(name = "R-CMD-check")
usethis::use_mit_license()

install.packages('ggplot2')

library(magrittr)
library(tidyverse)
library(qordstan)
library(rstan)
library(covr)
library(bqror)


devtools::load_all()

data = gen_data_example(n=3000, k = 5, seed = 1, p = 6, q = 0.5)


fit = qord_fit(y ~ X1 + X2, q = 0.1, delta_scale = 2,
               data = data$example_df, iter = 500)


fit$posterior_sample %>% head()
sm = summary(fit)
sm
fit

fit$posterior_sample %>% head()


pred = predict(fit, type='cat')

pred %>% apply(MARGIN = 2, FUN = function(x){
  table(x) %>% which.max()
}) %>% table()

object = fit
new_data = object$x
#coefficients posterior sample
beta = object$posterior_sample$beta
#cutpoints posterior sample
gamma = object$posterior_sample$gamma
#quantile
q = object$q

library(bayesplot)

fit$posterior_sample$beta

mcmc_areas(fit$posterior_sample, pars = c("X1", "X2"),
           prob = 0.95)






fit$waic

sm = summary(fit)
sm
sm$summary_table

all.equal(1:max(data$example_df$y), unique(data$example_df$y) %>% sort())

x = c(1, 2, 2, 5, 6, 1.1)

l = loo::loo(fit$stan_fit)
l$estimates
l$looic

?loo::loo

bridgesampling::bridge_sampler(fit$stan_fit)

all.equal(x %>% unique() %>% sort(),
          x %>% unique() %>% sort() %>% as.integer())

