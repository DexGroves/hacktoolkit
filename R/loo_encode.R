#' Do Owen Zhang style leave-one-out encoding of a categorical.
#'
#' Take the mean of a variable for all rows with the same id except
#' for the current row, so as to avoid leakage.
#'
#' @import data.table
#' @export
#'
#' @param id vector of identifiers to group over
#' @param resp vector of response to summarise
#' @return vector of one-left-out summarised response over id
#'
#' @examples
#' test_data <- data.frame(
#'   id = c(rep("a", 5), rep("b", 3), rep("c", 2), "d"),
#'   resp = c(1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1)
#' )
#'
#' loo_encode(test_data$id, test_data$resp)
loo_encode <- function(id, resp) {
  working_df <- data.table(id, resp)
  working_df[, encoded := loo_grouped_vector(resp), by = id]
  working_df[is.nan(encoded), encoded := NA]
  working_df$encoded
}

loo_grouped_vector <- function(resp) {
  total <- sum(resp)
  sapply(resp, function(x) (total - x)/(length(resp) - 1))
}
