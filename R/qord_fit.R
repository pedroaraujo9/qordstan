#' Fit bayesian ordinal quantile model
#'
#' fit a bayesian ordinal quantile model using NUTS algorithm through Stan. This function return an \code{qordstan} object with the model \code{stanfit} object and other posterior informations.
#'
#'
#' @param formula model formula
#' @param q fixed quantile
#' @param data data.frame with response variable and covariates
#' @param beta_scale standard deviation for the coefs prior
#' @param delta_scale standard deviation for the delta priors (cutpoints)
#' @param iter number of iterations
#' @param warmup length of warmup samples
#' @param thin period of the chain to retain
#' @param chains number of mcmc chains
#' @param verbose boolean (default is TRUE) indicating whether to print rstan progress bar or not
#' @param ... additional rstan parameters (see more in ?rstan::sampling)
#' @details this function fit the model described in the paper \emph{Mohammad Arshad Rahman "Bayesian Quantile Regression for Ordinal Models," Bayesian Analysis, Bayesian Anal. 11(1), 1-24, (March 2016)} using \code{rstan}.
#' @family qordstan
#'
#' @author Pedro Araujo
#'
#' @references Mohammad Arshad Rahman "Bayesian Quantile Regression for Ordinal Models," Bayesian Analysis, Bayesian Anal. 11(1), 1-24, (March 2016)
#'
#' @return a qordstan object with components:\tabular{ll}{
#'    \code{stan_fit} \tab \code{stanfit} object with the fitted model \cr
#'    \tab \cr
#'    \code{posterior_sample} \tab mixed posterior sample \cr
#'    \tab \cr
#'    \code{posterior_log_lik} \tab posterior log likelihood \cr
#'    \code{x} \tab matrix with model covariates \cr
#'    \code{y} \tab numeric vector with model response \cr
#'    \code{q} \tab model quantile \cr
#'    \code{beta_scale} \tab prior beta scale \cr
#'    \code{gamma_scale} \tab prior gamma scale \cr
#'    \code{waic} \tab \code{loo} object with WAIC \cr
#' }
#'
#' @export
#' @importFrom rlang is_formula
#' @import magrittr assertthat
#' @examples
#' data = gen_data_example()
#' fit = qord_fit(y ~ ., q = 0.5, data = data$example_df, iter = 10, warmup = 5)
qord_fit = function(formula,
                    q,
                    data,
                    beta_scale = 100,
                    delta_scale = 0.25,
                    iter = 2000,
                    warmup = floor(iter/2),
                    thin = 1,
                    chains = 1,
                    verbose = T,
                    ...) {

  #check formula
  assertthat::assert_that(
    is_formula(formula),
    msg = "`formula` should be a formula"
  )

  #check q
  assertthat::assert_that(
    is.numeric(q),
    assertthat::is.scalar(q),
    (q > 0) & (q < 1),
    msg = "Quantile `q` should be a real number between 0 and 1 (exclusive)"
  )

  #check data
  assertthat::assert_that(
    is.data.frame(data),
    nrow(data) > 0,
    nrow(data) > ncol(data),
    msg = "`data` should be a data.frame with rows greater than 0 and more observations than columns"
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

  #all data variables
  data_variables = model.frame(formula = formula, data = data)
  #categorical response columns name
  response_name = all.vars(formula)[1]

  #get covariates and response
  x = model.matrix(formula, data = data)[, -1]
  y = data_variables[, response_name]
  #covariate
  covariate_name = names(x)

  #### checking data ####
  #na
  assertthat::assert_that(
    !any(is.na(data_variables)),
    msg = "`data` should not have NA's"
  )

  #response
  assertthat::assert_that(
    is.numeric(y),
    msg = 'the response variable should be numeric'
  )

  assertthat::assert_that(
    length(unique(y)) > 2,
    msg = 'the response shoulbe have more than 2 categories'
  )

  #check if the response vary from 1 to k
  assertthat::assert_that(
    all.equal(1:max(y), unique(y) %>% sort()),
    msg = 'the response variable should vary from 1 to the number of categories'
  )

  #check if the response only have integers
  assertthat::assert_that(
    all.equal(y %>% unique() %>% sort(), y %>% unique() %>% sort() %>% as.integer()) == T,
    msg = 'the response variable should only have integers'
  )

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
    warmup = warmup, thin = thin, chains = chains, open_progress = verbose,
    ...
  )

  names(model_fit)[1:ncol(x)] = colnames(x)

  #model output
  out = new_qordstan(
    model_fit,
    formula = formula,
    x = x,
    y = y,
    q = q,
    beta_scale = beta_scale,
    delta_scale = delta_scale
  )

  return(out)
}


