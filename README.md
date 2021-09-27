# qordstan
Bayesian Quantile Regression for Ordinal Models with Stan

# installing 
```{r}
devtools::devtools::install_github("pedroaraujo9/qordstan")
```
# example
```{r}
library(qordstan)
#simulated data
data = gen_data_example()
#stan fit
fit = qord_fit(data$x, data$y, p = 0.5)
```
