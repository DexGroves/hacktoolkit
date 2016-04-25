#' Multivariate binomial deviance.
#'
#' @export
#' @param y Numeric vector of actual classes
#' @param U Numeric matrix of response-scale predictions, classes as columns
#' @param cap Optional two-element vector of the min and max caps for
#' predictions in U. Useful for neural networks.
#' @return sum of deviances across all classes
mv_binomial_deviance <- function(y, U, cap = c(0, 1)) {
  U[U < cap[0]] <- cap[0]
  U[U > cap[1]] <- cap[1]

  deviances <- sapply(
    1:ncol(U), function(n) binomial_deviance(as.numeric(y == n), U[n, ]))
  sum(deviances)
}

#' Multivariate AUC.
#'
#' @export
#' @param y Numeric vector of actual classes
#' @param U Numeric matrix of response-scale predictions, classes as columns
#' @return AUC statistic
multivariate_auc <- function(y, U) {
  n_classes <- ncol(U)

  y_binary <- matrix(0, nrow = length(y), ncol = n_classes)
  fold_index <- cbind(seq_along(y), y)
  y_binary[fold_index] <- 1

  auc(as.numeric(y_binary), as.numeric(U))
}
