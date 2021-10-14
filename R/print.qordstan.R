#' Print summary for qordstan object
#'
#' @param x qordstan object
#' @param ... aditional arguments
#' @return NULL
#' @export
#' @examples
#' #data = gen_data_example()
#' #fit = qord_fit(y ~ ., q = 0.5, data = data$example_df, iter = 10, warmup = 5)
#' #fit
print.qordstan = function(x, ...) {
  cat('qordstan fit')
}
