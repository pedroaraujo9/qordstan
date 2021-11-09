<!-- badges: start -->
[![R-CMD-check](https://github.com/pedroaraujo9/qordstan/workflows/R-CMD-check/badge.svg)](https://github.com/pedroaraujo9/qordstan/actions)
[![Codecov test coverage](https://codecov.io/gh/pedroaraujo9/qordstan/branch/dev/graph/badge.svg)](https://codecov.io/gh/pedroaraujo9/qordstan?branch=main)
<!-- badges: end -->

# qordstan

This package implements the Bayesian quantile ordinal model described in [(Rahman, 2016)](https://projecteuclid.org/journals/bayesian-analysis/volume-11/issue-1/Bayesian-Quantile-Regression-for-Ordinal-Models/10.1214/15-BA939.full) using `rstan`.


### Installing the package:

```r
devtools::install_github("pedroaraujo9/qordstan", ref = 'main')
```

### Simulating data

The `gen_data_example` function returns a list with a `data.frame` containing the simulated data and the true model parameters values. 

Example Generating data with 1000 observations, 5 categories, 0.3 quantile and 4 covariates: 
```r
example_data = gen_data_example(n = 1000, k = 5, q = 0.3, p = 4)
```
### Fiting the model 

The model can be fitted with the function `qord_fit`:

```r
model_fit = qord_fit(y ~ X1 + X2 + X3 + X4, q = 0.3, 
                     data = example_data$example_df, 
                     iter = 1000, verbose = F)
```

The function returns a `qordstan` object. We can access the original `stanfit` object, WAIC, posterior samples, etc.


Summarizing the results:

```r
summary(model_fit)
```

The beta and gamma true values are in the example_data object:

```r
example_data[c("b", 'gamma')]
```

### Predicting

We can also get a posterior predictive sample from the model and choose between the latent variable z_i or the observed response y_i:

```r
#sampling y_i
pred = predict(model_fit)
#bar plot for of the posterior predictive sample of the first observation
pred[,1] %>% table() %>% barplot()
```

## References
- Mohammad Arshad Rahman "Bayesian Quantile Regression for Ordinal Models," Bayesian Analysis, Bayesian Anal. 11(1), 1-24, (March 2016).

- Stan Development Team (2020). “RStan: the R interface to Stan.” R package version 2.21.2, http://mc-stan.org/.


