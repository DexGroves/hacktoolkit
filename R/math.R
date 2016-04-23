#' Calculate weighted variance.
#' @export
weighted_var <- function(x, w = rep(1, length(w))) {
  sum(w * (x)^2)/sum(w) - (sum(x * w)/sum(w))^2
}

#' Calculate binomial deviance.
#'
#' @param y numeric vector of actuals
#' @param u numeric vector of predictions
#' @param w optional weight vector
#' @export
binomial_deviance <- function(y, u, w = rep(1, length(y))){
  l <- sum(w[y == 1] * y[y == 1] * log(y[y == 1] / u[y == 1])) +
       sum(w[y == 0] * log(1/(1 - u[y == 0])))
  2 * l
}

#' Apply logit transform on a vector x.
#' @export
logit_to_prob <- function(x){
  1 / (1 + exp(-x))
}

#' Apply the inverse logit transform on a vector x.
#' @export
prob_to_logit <- function(x){
  - 1 * log(1 / x - 1)
}

#' Classic, binomial AUC.
#'
#' @import data.table
#' @export
#' @param actual Vector of binomaial actuals
#' @param predicted Vector of numeric predicted in (-Inf, +Inf)
#' @param weight Optional vector of numeric weights in (0, +Inf)
#' @return AUC statistic
auc <- function(actual, predicted, weight = rep(1, length(actual))) {

  auc_dt <- data.table(actual, predicted = -1 * predicted,
                       weight, key = "predicted")

  auc_dt[, xaxis := weight * (actual == 0)]
  auc_dt[, yaxis := weight * (actual == 1)]
  auc_dt[, remaining_xaxis := sum(xaxis) - cumsum(xaxis)]

  total_area <- sum(auc_dt$xaxis) * sum(auc_dt$yaxis)

  sum(auc_dt$yaxis * auc_dt$remaining_xaxis) / total_area
}
