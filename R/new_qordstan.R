#' Creates qordstan object
#'
#' @param stan_fit stan model fit object
#' @param x numeric matrix with covariates
#' @param y vector with categories
#' @param q quantile
#' @param beta_scale standard deviation for the coefs prior
#' @param delta_scale standard deviation for the deltas prior (cutpoints)
#' @return
#'
#' @examples
#'
new_qordstan = function(stan_fit, x, y, q, beta_scale, delta_scale) {
  #get posterior sample
  posterior_sample = stan_fit %>% rstan::extract()

  value = list(
    stan_fit = stan_fit,
    posterior_sample = posterior_sample,
    x = x,
    y = y,
    q = q,
    beta_scale = beta_scale,
    delta_scale = delta_scale
  )
  #add class qordstan
  attr(value, "class") = "qordstan"
  return(value)
}

