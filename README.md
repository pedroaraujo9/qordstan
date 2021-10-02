<!-- badges: start -->
[![R-CMD-check](https://github.com/pedroaraujo9/qordstan/workflows/R-CMD-check/badge.svg)](https://github.com/pedroaraujo9/qordstan/actions)
<!-- badges: end -->
# qordstan
Bayesian Quantile Regression for Ordinal Models with Stan

### Installation 
```r
devtools::install_github("pedroaraujo9/qordstan", ref = 'main')
```
### Example
```r
library(qordstan)
#simulated data
data = gen_data_example()
#stan fit
fit = qord_fit(data$x, data$y, q = 0.5)
```
