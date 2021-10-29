#' Transform underlying latent variable in category
#'
#' @param z latent variable
#' @param gamma cut points
#'
#' @return vector with numeric categories
#'
.z_to_cat = function(z, gamma) {
  cut_breaks = c(-Inf, 0, gamma, Inf)
  z %>%
    cut(breaks = cut_breaks, labels = 1:(length(cut_breaks)-1)) %>%
    as.character() %>%
    as.numeric()
}


#' Get posterior predictive sample from the model
#'
#' Generate sample from posterior predictive distribution.
#'
#'
#' @aliases predict.qordstan
#'
#' @param object a qordstan object
#' @param type the type of prediction. It will be 'cat' by default, returning
#' the category. If 'z' returns the underlying latent variable
#' @param new_data new covariates for prediction. if null, predicts from model data
#' @param ... additional parameters
#' @examples
#' #data = gen_data_example()
#' #fit = qord_fit(data$x, data$y, q = 0.5, iter = 10, warmup = 5)
#' #predict(fit)
#'
#' @method predict qordstan
#' @export
#'

predict.qordstan = function(object, type = "cat", new_data = NULL, ...) {
  #check type of prediction
  assertthat::assert_that(
    type %in% c("cat", "z"),
    msg = "`type` only acceept `cat` and `z` as argument"
  )

  #if new data is null, get model data
  if(is.null(new_data)) {
    new_data = object$x
    #if is not null, transform data.frame in X
  }else{
    new_data = model.matrix(object$formula, data = new_data)[, -1]
  }

  posterior_sample = object$posterior_sample
  #coefficients posterior sample
  beta = posterior_sample[, colnames(object$x)]
  #cutpoints posterior sample
  gamma = posterior_sample[, grepl("gamma", colnames(posterior_sample))]
  #quantile
  q = object$q

  #sample from z
  theta = (1-2*q)/(q*(1-q))
  tau = sqrt(2/(q*(1-q)))
  linear_pred = new_data%*%t(beta)
  dim_preds = dim(linear_pred)

  w = replicate(n = dim_preds[2], expr = {rexp(n=dim_preds[1])})
  u = replicate(n = dim_preds[2], expr = {rnorm(n=dim_preds[1])})

  z = linear_pred + theta*w + tau*sqrt(w)*u
  z = t(z)

  #return z if required
  if(type == 'z') {
    return(z)
  }

  #find category and return category if required
  if(type == 'cat') {

    y_pred = matrix(NA, dim_preds[2], dim_preds[1])

    for(i in 1:dim_preds[2]) {
      y_pred[i,] = .z_to_cat(z[i,], gamma[i,])
    }
    return(y_pred)
  }


}
