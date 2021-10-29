#' Creates qordstan object
#'
#' @param stan_fit stan model fit object
#' @param formula model formula
#' @param x numeric matrix with covariates
#' @param y vector with categories
#' @param q quantile
#' @param beta_scale standard deviation for the coefs prior
#' @param delta_scale standard deviation for the deltas prior (cut-points)
#' @import loo
#' @return qordstan object
#' @examples
#' if(FALSE) {
#'     new_qordstan = function(stan_fit, formula, x, y, q, beta_scale, delta_scale)
#' }
#'
#'
new_qordstan = function(stan_fit, formula, x, y, q, beta_scale, delta_scale) {
  #check if is stan fit
  assertthat::assert_that(
    class(stan_fit) == 'stanfit',
    msg = '`stan_fit` should be a stanfit object'
  )

  #check beta_scale
  assertthat::assert_that(
    is.scalar(beta_scale),
    beta_scale > 0,
    msg = "`beta_scale` should be a real number greater than 0"
  )

  #check delta_scale
  assertthat::assert_that(
    is.scalar(delta_scale),
    delta_scale > 0,
    msg = "`delta_scale` should be a real number greater than 0"
  )

  #check q
  assertthat::assert_that(
    is.numeric(q),
    assertthat::is.scalar(q),
    (q > 0) & (q < 1),
    msg = "Quantile `q` should be a real number between 0 and 1 (exclusive)"
  )

  #number of classes
  k = length(unique(y))
  #params posterior sample
  posterior_sample = stan_fit %>%
    rstan::extract(pars = c(colnames(x), "gamma"), permuted=TRUE) %>%
    do.call(cbind, .)

  colnames(posterior_sample) = c(colnames(x),  paste0("gamma[", 1:(k-2), "]"))

  #posterior log_lik
  posterior_log_lik = loo::extract_log_lik(stanfit = stan_fit)

  #waic
  waic_est = posterior_log_lik %>% loo::waic()

  value = list(
    stan_fit = stan_fit,
    formula = formula,
    posterior_sample = posterior_sample,
    posterior_log_lik = posterior_log_lik,
    x = x,
    y = y,
    q = q,
    beta_scale = beta_scale,
    delta_scale = delta_scale,
    waic = waic_est
  )
  #add class qordstan
  attr(value, "class") = "qordstan"
  return(value)
}

