#' Add an interaction, poly, one-hot style effect that can be created 
#' with formula objects and `model.matrix`. 
#' @param formula formula describing the transformation. 
#' Of the form: `~ <stuff>`
#' @param data data.frame to which encoding will be created and appended
#' @return copy of data with new columns
#' @export
add_encoding <- function(formula, data) { 
  extra_cols <- model.matrix(
    model.frame(formula, data, na.action = na.pass),
    data 
  ) 
  cbind(data, extra_cols[, colnames(extra_cols) != "(Intercept)"])
}

