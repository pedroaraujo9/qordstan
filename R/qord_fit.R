#' Fit the ordinal quantile regression model using Stan
#'
#' @param x numeric matrix with covariates
#' @param y vector with categories
#' @param q fixed quantile
#' @param beta_scale standard deviation for the coefs prior
#' @param delta_scale standard deviation for the delta priors (cutpoints)
#' @param iter number of iterations
#' @param warmup length of warmup sample
#' @param thin period of saving samples
#' @param chains number of mcmc chains
#' @param ... aditional rstan parameters
#'
#' @return qordstan object
#' @export
#' @import magrittr
#' @examples
#' data = gen_data_example()
#' fit = qord_fit(data$x, data$y, q = 0.5, iter = 10, warmup = 5)
qord_fit = function(x, y, q,
                    beta_scale = 1, delta_scale = 0.25,
                    iter = 2000,
                    warmup = 1000,
                    thin = 1,
                    chains = 1,
                    ...) {

  #model data and parameters
  stan_data_list = list(
    x = x,
    y = y,
    q = q,
    k = length(unique(y)),
    p = ncol(x), n = nrow(x),
    beta_scale = beta_scale,
    delta_scale = delta_scale
  )

  #sampling from posterior
  model_fit = rstan::sampling(
    stanmodels$model, data = stan_data_list,
    pars = c("beta", "gamma", "log_lik"), iter = iter,
    warmup = warmup, thin = thin, chains = chains,
    ...
  )


  #model output
  out = new_qordstan(
    model_fit,
    x = x, y = y, q = q,
    beta_scale = beta_scale,
    delta_scale = delta_scale
  )

  return(out)
}


