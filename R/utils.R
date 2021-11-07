#' Return vector with average, standard deviation and HDI
#'
#' @param x numeric vector
#' @param cred_mass length of creditive interval. Must be between 0 and 1
#' @import HDInterval
#' @return numeric vector with mean, standard deviation and HDI
#'
posterior_resume = function(x, cred_mass = 0.95) {
  assertthat::assert_that(
    (cred_mass > 0) & (cred_mass < 1),
    msg = "`cred_mass` should be greater than 0 and less than 1."
  )
  hdi = HDInterval::hdi(x, cred_mass) %>% as.numeric()
  c("mean" = mean(x), "std" = sd(x),"HDI LI" = hdi[1], "HDI UI" = hdi[2])
}


#' Transform underlying latent variable in category
#'
#' @param z latent variable
#' @param gamma cut points
#'
#' @return vector with numeric categories
#'
.z_to_cat = function(z, gamma) {
  assertthat::assert_that(
    all(gamma > 0),
    msg = "all `gamma` values should be greater than 0."
  )

  cut_breaks = c(-Inf, 0, gamma, Inf)
  z %>%
    cut(breaks = cut_breaks, labels = 1:(length(cut_breaks)-1)) %>%
    as.character() %>%
    as.numeric()
}
