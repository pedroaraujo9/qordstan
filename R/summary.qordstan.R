#' Title
#'
#' @param x numeric vector
#' @param cred_mass length of creditive interval. Must be between 0 and 1
#' @import HDInterval
#' @return
#'
posterior_resume = function(x, cred_mass = 0.95) {
  hdi = HDInterval::hdi(x, cred_mass) %>% as.numeric()
  c("mean" = mean(x), "std" = sd(x),"cred LI" = hdi[1], "cred UI" = hdi[2])
}

#' Summary method to qordstan models containing posterior mean, standard deviation
#' and lower and upper bond for creditiva interval
#'
#' @param x qorstan object
#' @param cred_mass length of creditive interval, must be between 0 and 1
#' @param ... aditional arguments
#' @export
#' @return
#' @examples
#' data = gen_data_example()
#' qord_model = qord_fit(data$x, data$y, q = 0.5, iter = 10, warmup = 5)
#' summary(qord_model)
#'
summary.qordstan = function(x, ..., cred_mass) {
  #params = list(...)
  #cred_mass = params[['cred_mass']]
  #get posterior sample
  posterior_sample = x$posterior_sample
  #resume for beta
  beta_res = apply(posterior_sample$beta, posterior_resume, MARGIN = 2) %>% t()
  rownames(beta_res) = paste0("beta ", 1:nrow(beta_res))
  #resume for gamma
  gamma_res = apply(posterior_sample$gamma, posterior_resume, MARGIN = 2) %>% t()
  rownames(gamma_res) = paste0("gamma", 1:nrow(gamma_res))
  #binding resumes
  res = rbind(beta_res, gamma_res)
  res
}


