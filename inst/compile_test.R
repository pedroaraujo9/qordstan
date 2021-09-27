roxygen2::roxygenize()
devtools::install()

library(qordstan)

data = gen_data_example()
fit = qord_fit(data$x, data$y, p = 0.5)

fit
data$b
data$deltas
data$gammas
plot(fit)
