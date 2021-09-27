# qordstan
Bayesian Quantile Regression for Ordinal Models with Stan

### Installation 
```r
devtools::devtools::install_github("pedroaraujo9/qordstan")
```
### Example
```r
library(qordstan)
#simulated data
data = gen_data_example()
#stan fit
fit = qord_fit(data$x, data$y, p = 0.5)
```
