<!-- badges: start -->
[![R-CMD-check](https://github.com/pedroaraujo9/qordstan/workflows/R-CMD-check/badge.svg)](https://github.com/pedroaraujo9/qordstan/actions)
[![Codecov test coverage](https://codecov.io/gh/pedroaraujo9/qordstan/branch/dev/graph/badge.svg)](https://codecov.io/gh/pedroaraujo9/qordstan?branch=main)
<!-- badges: end -->

# qordstan

This package implements the Bayesian quantile ordinal model described in [(Rahman, 2016)](https://projecteuclid.org/journals/bayesian-analysis/volume-11/issue-1/Bayesian-Quantile-Regression-for-Ordinal-Models/10.1214/15-BA939.full) using `rstan`.

## The model
In resume, defining $\boldsymbol{\beta}_p$ as a $k\times 1$ vector with unknown coefficients at the $p$ quantile and $\mathbf{x}_i$ as a $k\times 1$ vector with fixed covariates of individual $i$, the ordinal quantile model can be expressed in terms of a latent variable $z_i$, where:

$$z_i = \mathbf{x}_i'\boldsymbol{\beta}_p + \epsilon_i.$$

The residual $\epsilon_i$ follows the asymmetric Laplace distribution $\text{AL}(0, 1, p)$ with p.d.f:


\[f_p(\epsilon_i)= p(1-p)\begin{cases} 
      \exp\{-\epsilon_i(p-1)\} & \epsilon < 0 \\
      \exp\{-\epsilon_i p\} & \epsilon \geq 0
      \end{cases}
\]


The observed ordinal response $y_i$ are related with $z_i$ through a vector of cut-points $\boldsymbol{\gamma}_p$, which $\gamma_{p, j-1}<z_i \leq \gamma_{p,j}$, implies in $y_i = j$, for $j=1,\dots, J$, setting $\gamma_{p,0}=-\infty$, $\gamma_{p,J}=\infty$ and $\gamma_{p,1}=0$.


## Fitting the model

Installing the package:

```{r eval = F}
devtools::install_github("pedroaraujo9/qordstan", ref = 'main')
```

The `gen_data_example` function returns a list with a `data.frame` containing the simulated data and the true model parameters values. Generating data with 1000 observations, $5$ categories, $0.3$ quantile and $4$ covariates: 

```{r}
example_data = gen_data_example(n = 1000, k = 5, q = 0.3, p = 4)
```

The model can be fitted with the function `qord_fit`:

```{r}
model_fit = qord_fit(y ~ X1 + X2 + X3 + X4, q = 0.3, 
                     data = example_data$example_df, 
                     iter = 1000, verbose = F)
```

The function returns a `qordstan`object. We can access the original `stanfit` object, WAIC, posterior samples, etc.


We can summary the results with:

```{r}
sm = summary(model_fit)
sm
```

The $\boldsymbol{\beta}_p$ and $\boldsymbol{\gamma}_p$ true values are in example_data object:

```{r}
example_data[c("b", 'gamma')]
```

We can also get a posterior predictive sample from the model and choose between the latent variable $z_i$ or the observed response $y_i$:

```{r}
#sampling y_i
pred = predict(model_fit)
#bar plot for of the posterior predictive sample of the first observation
pred[,1] %>% table() %>% barplot()
```

### References

Mohammad Arshad Rahman "Bayesian Quantile Regression for Ordinal Models," Bayesian Analysis, Bayesian Anal. 11(1), 1-24, (March 2016)  


