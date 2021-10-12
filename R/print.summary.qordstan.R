#' Print method for summary method
#'
#' @aliases print.summary.qordstan
#' @param x summary.qordstan object
#' @param ... additional parameters
#'
#' @method print summary.qordstan
#' @return None
#' @export
#' @importFrom stats printCoefmat
#'
#' @examples
#' #data = gen_data_example()
#' #qord_model = qord_fit(data$x, data$y, q = 0.5, iter = 10, warmup = 5)
#' #summary(qord_model)
#'
print.summary.qordstan = function(x, ...) {
  cat("Ordinal quantile model\n")
  cat("WAIC: ", round(x$waic$estimates['waic', 'Estimate'], 4), "\n\n")
  cat("Posterior estimates:\n")
  printCoefmat(x$summary_table, digits = 4)
}
