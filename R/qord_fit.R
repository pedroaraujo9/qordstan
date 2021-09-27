
#' Generate random data to test function
#'
#' @return list containing X, y, b (real coefs), delta's and gamma's
#' @export
#' @importFrom magrittr `%>%`
#'
#' @examples
gen_data_example = function() {
  set.seed(20)
  #observations
  n = 900
  #covariáveis
  x = cbind(
    runif(n, 0, 5),
    runif(n, 0, 3)
  )

  #number of categories
  k = 5
  #percentile
  p = 0.5
  #coefs
  b = cbind(c(1.5, -0.7))


  #parameters transform
  theta = (1-2*p)/(p*(1-p))
  tau = sqrt(2/(p*(1-p)))

  w = rexp(n)
  u = rnorm(n)

  #latent variable
  z = as.numeric(x%*%b) + theta*w + tau*sqrt(w)*u

  #cutpoints
  gammas = c(-Inf, 0, 2, 4.5, 6, Inf)
  #generating response variable
  y = cut(z, gammas, labels = c(1, 2, 3, 4, 5)) %>%
    as.character() %>%
    as.numeric()

  deltas = log(gammas[-c(1, length(gammas))] %>% diff())

  return(list(x=x, y=y, b=b, deltas=deltas, gammas=gammas))

}

#' Fit the ordinal quantilic regression model using stan
#'
#' @param x numeric matrix with covariates
#' @param y vector with categories
#' @param p fixed quantile
#' @param beta_scale standard deviation for the coefs prior
#' @param delta_scale standard deviation for the deltas prior (cutpoints)
#'
#' @return stan model
#' @export
#'
#' @examples
#' data = gen_data_example()
#' fit = qord_fit(data$x, data$y, p = 0.5)
qord_fit = function(x, y, p, beta_scale = 1, delta_scale = 0.25, ...) {
  #stan model data
  standata = list(
    x = x,
    y = y,
    p = p,
    J = length(unique(y)),
    k = ncol(x), n = nrow(x),
    sigma_beta = beta_scale,
    sigma_delta = delta_scale
  )

  model_fit = rstan::sampling(stanmodels$model, data = standata, ...)
  return(model_fit)
}


