#' Print method for summary method
#'
#' @aliases print.summary.qordstan
#' @param x summary.qordstan object
#' @param ... additional parameters
#'
#' @method print summary.qordstan
#' @return NULL
#' @export
#' @importFrom stats printCoefmat
#'
#' @examples
#' #data = gen_data_example()
#' #fit = qord_fit(y ~ ., q = 0.5, data = data$example_df, iter = 10, warmup = 5)
#' #summary(fit)
#'
print.summary.qordstan = function(x, ...) {
  cat("Bayesian ordinal quantile model with stan\n")
  cat("Number of observations:", x$n, "\n")
  cat("Number of categories:", x$k, "\n\n")
  cat("Posterior summary:\n")
  printCoefmat(x$summary_table, digits = 4)
  cat("\n")
  cat("WAIC:", round(x$waic$estimates['waic', 'Estimate'], 4), "\n\n")
}
