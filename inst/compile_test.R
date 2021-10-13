rm(list = ls())
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


devtools::load_all()

data$gamma
data$y %>% table() %>% prop.table()


data = gen_data_example(n=3000, k = 3, seed = 1, p = 6, q = 0.5)

all.equal(x, as.integer(x)) == T

data$example_df %>% names()

formula = y ~ X1 + X2 + X3 + X1*X2

data$example_df %>% head()
colnames(data$example_df) = c("aa", "bb", "cc", "dd", "ee", "ff", "my_response")

data$example_df %>% head()

all.vars(y ~ .)

all.va

fit = qord_fit(y ~ X1 + X2, q = 0.1, delta_scale = 2,
               data = data$example_df, iter = 100)


summary(fit)



