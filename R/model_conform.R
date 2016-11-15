#' Form a model matrix with structure forced to correspond to target.
#'
#' @param object formula to be used with `model.matrix`
#' @param data data.frame to be used with `model.matrix`
#' @param target matrix to which the output matrix structure will be aligned,
#' or a character vector of colnames that will be forced.
#'
model_conform <- function(object, data, target, ...) {
  if (!is.character(target)) {
    target <- colnames(target)
  }

  mm <- model.matrix(object, data, ...)

  missing_cols <- setdiff(target, colnames(mm))
  for (col in missing_cols) {
    mm[[col]] <- 0
  }

  mm[, target]
}
