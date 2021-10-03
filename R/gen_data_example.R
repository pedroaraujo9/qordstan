#' Generate random data to test estimation procedure
#'
#' Generate random from a specific quantile to fit the model and check if
#' estimation procedure is working properly
#'
#' @aliases gen_data_example
#'
#' @param n number of observations
#' @param k number of categories
#' @param q quantile
#' @param p number of covariates
#' @param seed random seed
#'
#' @return list containing x (covariates), y (response), b (real coefs),
#' delta's and gamma's
#'
#' @details
#' x ~ runif
#' beta ~ normal
#' the fist gamma parameter is 0, the rest is chooose in order to non baseline
#' categories have the same number of observations
#'
#' @export
#' @import magrittr
#' @importFrom stats rexp rnorm runif sd
#' @examples
#' data = gen_data_example()
#'
gen_data_example = function(n = 1000, k = 5, q = 0.5, p = 3, seed = 20) {
  set.seed(seed)
  #covariáveis
  x = replicate(n = p, expr = {runif(n, min = 0, max = 1)})
  #coefs
  b = cbind(rnorm(n=p, mean = 1, sd = 2))
  #parameters transform
  theta = (1-2*q)/(q*(1-q))
  tau = sqrt(2/(q*(1-q)))
  #auxiliary variables
  w = rexp(n)
  u = rnorm(n)

  #latent variable
  z = as.numeric(x%*%b) + theta*w + tau*sqrt(w)*u

  #proportion of 1's
  freq_base = mean(z <= 0)
  #proportion of each non baseline response
  freq_nom_base = (1-freq_base)/(k-1)
  #get quantiles from z to ensure that proportion
  qfreq_nom_base = (freq_base) + freq_nom_base*1:(k-2)
  qgamma = quantile(z, qfreq_nom_base) %>% as.numeric()

  #cutpoints
  gamma = c(-Inf, 0, qgamma, Inf) %>% sort()
  #generating response variable
  y = cut(z, gamma, labels = 1:k) %>%
    as.character() %>%
    as.numeric()

  deltas = log(gamma[-c(1, length(gamma))] %>% diff())

  return(list(x=x, y=y, b=b, deltas=deltas, gamma=gamma))

}
