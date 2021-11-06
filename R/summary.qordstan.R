#' Return vector with average, standard deviation and HDI
#'
#' @param x numeric vector
#' @param cred_mass length of creditive interval. Must be between 0 and 1
#' @import HDInterval
#' @return numeric vector with mean, standard deviation and HDI
#'
posterior_resume = function(x, cred_mass = 0.95) {
  hdi = HDInterval::hdi(x, cred_mass) %>% as.numeric()
  c("mean" = mean(x), "std" = sd(x),"HPD LI" = hdi[1], "HPD UI" = hdi[2])
}

#' Summary method for qordstan objects
#'
#' Summary method to qordstan models containing posterior mean, standard deviation
#' and lower and upper bond of the high density credible interval and other useful quantities
#'
#'
#' @aliases summary.qordstan
#' @param object a qorstan object
#' @param cred_mass length of credibility interval, must be between 0 and 1
#' @param ... additional arguments
#' @family qordstan
#' @examples
#' #data = gen_data_example()
#' #fit = qord_fit(y ~ ., q = 0.5, data = data$example_df, iter = 10, warmup = 5)
#' #summary(fit)
#'
#' @method summary qordstan
#' @author Pedro Araujo
#' @export
#' @return summary.qordstan object with components:\tabular{ll}{
#'    \code{summary_table} \tab matrix with posterior posterior mean, standard deviation and HDI \cr
#'    \tab \cr
#'    \code{beta_mean} \tab posterior mean of model coefficients \cr
#'    \tab \cr
#'    \code{waic} \tab loo waic object  \cr
#'    \code{n} \tab number of observations \cr
#'    \code{k} \tab number of categories \cr
#' }
#'
summary.qordstan = function(object, cred_mass = 0.95, ...) {
  #check cred_mass
  assertthat::assert_that(
    (cred_mass > 0) & (cred_mass < 1),
    msg = '`cred_mass` should be a number between 0 and 1'
  )

  #get posterior sample
  posterior_sample = object$posterior_sample
  waic = object$waic
  coef_names = object$x %>% colnames()

  #resume for beta
  beta_posterior = posterior_sample[, colnames(object$x)]
  beta_res = apply(beta_posterior, posterior_resume, MARGIN = 2) %>% t()
  #rownames(beta_res) = coef_names

  #resume for gamma
  gamma_posterior = posterior_sample[, grepl("gamma", colnames(posterior_sample))]
  gamma_res = apply(gamma_posterior, posterior_resume, MARGIN = 2) %>% t()
  #rownames(gamma_res) = paste0("gamma[", 1:nrow(gamma_res)) %>% paste0("]")

  #binding resumes
  res = rbind(beta_res, gamma_res)

  #means
  beta_mean = beta_res[, 'mean']
  gamma_mean = gamma_res[, 'mean']

  #attributes
  value = list(
    summary_table = res,
    beta_mean = beta_mean,
    gamma_mean = gamma_mean,
    waic = waic,
    n = object$x %>% nrow(),
    k = object$y %>% unique() %>% length()
  )
  attr(value, "class") = "summary.qordstan"
  return(value)
}


