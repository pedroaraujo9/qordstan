roxygen2::roxygenize()
devtools::install()

library(tidyverse)
library(qordstan)
library(rstan)

data = gen_data_example()
fit = qord_fit(data$x, data$y, p = 0.5)


am = fit %>% rstan::extract()
am$gammas %>% colMeans()
data$b
data$deltas
data$gammas
plot(fit)
