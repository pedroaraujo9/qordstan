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
#' delta's, gamma's and final example_df data.frame with covariates and response
#'
#' @details
#' x ~ runif
#' beta ~ normal
#' the fist gamma parameter is 0, the rest is chooose in order to non baseline
#' categories have the same number of observations
#'
#' @export
#' @import magrittr assertthat
#' @importFrom stats rexp rnorm runif sd quantile model.frame model.matrix
#' @examples
#' data = gen_data_example()
#'
gen_data_example = function(n = 1000, k = 5, q = 0.5, p = 3, seed = 20) {
  #check n
  assertthat::assert_that(
    is.numeric(n),
    assertthat::is.scalar(n),
    all.equal(n, as.integer(n)) == T,
    n > p,
    n > 0,
    msg = "Number of observations `n` should be a positive integer greater than `p` (number of covariates) and 0"
  )

  #check k
  assertthat::assert_that(
    is.numeric(k),
    assertthat::is.scalar(k),
    all.equal(k, as.integer(k)) == T,
    k > 2,
    msg = "Number of categories `k` should be a positive integer greater than 3"
  )

  #check q
  assertthat::assert_that(
    is.numeric(q),
    assertthat::is.scalar(q),
    (q > 0) & (q < 1),
    msg = "Quantile `q` should be a real number between 0 and 1 (exclusive)"
  )

  #check p
  assertthat::assert_that(
    is.numeric(p),
    assertthat::is.scalar(p),
    all.equal(p, as.integer(p)) == T,
    p < n,
    msg = "Number of covariates `p` should be positive integer lower than `n` (number of observations)"
  )

  #check seed
  assertthat::assert_that(
    is.numeric(seed),
    length(seed) == 1,
    all.equal(seed, as.integer(seed)) == T,
    msg = "`seed` should be an integer"
  )

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

  example_df = data.frame(x, y)

  values = list(example_df=example_df, x=x, y=y, b=b, deltas=deltas, gamma=gamma)
  return(values)
}
