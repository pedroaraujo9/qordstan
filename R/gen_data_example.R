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
