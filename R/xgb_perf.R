#' Imitate gbm::gbm.perf for an xgboost::xgb.cv object.
#'
#' Plots the train and mean CV error by nrounds.
#'
#' @import ggplot2
#' @export
#'
#' @param xgb_cv_model output data.table of an xgboost::xgb.cv call
#' @return ggplot object
xgb_perf <- function(xgb_cv_model) {
  melt_df <- xgb_cv_model[, colnames(xgb_cv_model)[c(1, 3)], with = FALSE] %>%
    {melt(., measure.vars = colnames(.))}

  melt_df[, tree := 1:.N, by = variable]
  ggplot(melt_df, aes(x = tree, y = value, group = variable, color = variable)) +
    geom_line() +
    labs(fill = "") +
    xlab("Number of trees") +
    ylab("")
}

