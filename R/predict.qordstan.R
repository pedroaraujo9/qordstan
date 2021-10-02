#' Transform underlyng latent variable in categorie
#'
#' @param z latent variable
#' @param gamma cut points
#'
#' @return categories
#'
#' @examples
#' z = rnorm(100)
#' gm = rexp(100, 1/2)
#' z_to_cat(z, gm)
.z_to_cat = function(z, gamma) {
  cut_breaks = c(-Inf, 0, gamma, Inf)
  z %>%
    cut(breaks = cut_breaks, labels = 1:(length(cut_breaks)-1)) %>%
    as.character() %>%
    as.numeric()
}


#' Get posterior predictive sample from the model
#'
#' @param object qordstan object
#' @param ... aditional parameters
#' @param type type of prediction. It will be 'cat' by default, returning
#' the categorie. If 'z' returns the underlyng latent variable
#' @param new_data new covariates for prediction.
#' if null, predicts from model data
#' @return
#' @export
#'
#' @examples
#' #' data = gen_data_example()
#' fit = qord_fit(data$x, data$y, q = 0.5, iter = 200, warmup = 100)
#' predict(fit, type='cat')
predict.qordstan = function(object, ...) {
  #aditional parameters
  kwargs = list(...)

  #get type of response if provided
  if(is.null(pred_type)) {
    pred_type = 'cat'
  }else{
    pred_type = kwargs[['type']]
  }

  #get new data if provided
  if(is.null(kwargs[['new_data']])) {
    x = object$x
  }else{
    x = kwargs[['new_data']]
  }

  beta = object$posterior_sample$beta
  gamma = object$posterior_sample$gamma
  linear_pred = x%*%t(beta)
  dim_preds = dim(linear_pred)
  w = replicate(n = dim_preds[2], expr = {rexp(n=dim_preds[1])})
  u = replicate(n = dim_preds[2], expr = {rnorm(n=dim_preds[1])})
  z = linear_pred + theta*w + tau*sqrt(w)*u

  if(pred_type == 'z') {
    return(z)
  }

  if(pred_type == 'cat') {
    y_pred = matrix(NA, dim_preds[1], dim_preds[2])
    for(i in 1:dim_preds[2]) y_pred[i,] = .z_to_cat(z[i,], gamma[i])
    return(y_pred)
  }


}
