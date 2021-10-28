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

data$gamma
data$y %>% table() %>% prop.table()


data = gen_data_example(n=3000, k = 5, seed = 1, p = 6, q = 0.5)

all.equal(x, as.integer(x)) == T

data$example_df %>% names()

formula = y ~ X1 + X2 + X3 + X1*X2

data$example_df %>% head()
colnames(data$example_df) = c("aa", "bb", "cc", "dd", "ee", "ff", "my_response")

data$example_df %>% head()

all.vars(y ~ .)

all.va

fit = qord_fit(y ~ X1 + X2, q = 0.1, delta_scale = 2,
               data = data$example_df, iter = 1000)

sm = summary(fit)
sm
fit

predict(fit, type='cat') %>% dim()

object = fit
new_data = object$x
#coefficients posterior sample
beta = object$posterior_sample$beta
#cutpoints posterior sample
gamma = object$posterior_sample$gamma
#quantile
q = object$q







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

