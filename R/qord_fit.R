
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
qord_fit = function(x, y, p, beta_scale = 1, delta_scale = 0.25,
                    pars = c("beta", "gamma"), ...) {
  #stan model data
  standata = list(
    x = x,
    y = y,
    p = p,
    J = length(unique(y)),
    k = ncol(x), n = nrow(x),
    beta_scale = beta_scale,
    delta_scale = delta_scale
  )

  model_fit = rstan::sampling(stanmodels$model, data = standata, ...)
  return(model_fit)
}


