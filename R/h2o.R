#' Parse the h2o cv grid into something less ridiculous. 
#' @import stringr
#' @import dplyr 
#' @import tibble
#' @export
#' @param grid H2OGrid of H2ORegressionModel objects
#' @return tibble of cv performance ordered by lambda
h2o_cvdf <- function(grid) { 
  as_tibble(grid@summary_table) %>% 
    mutate(lambda = as.numeric(str_extract(lambda, "[0-9|\\.]+")),
           model_ids = as.numeric(str_extract(model_ids, "[0-9]+$")),
           residual_deviance = as.numeric(residual_deviance)) %>% 
    arrange(lambda)
}

#' Plot the CV error vis-a-vis plot.cv.glmnet. Except without standard
#' errors, because h2o doesn't store them?
#' @import ggplot2 
#' @export
#' @param sane_grid Output of h2o_cvdf
#' @return ggplot object 
h2o_plotcv <- function(sane_grid) { 
  ggplot(sane_grid, aes(x = lambda, y = residual_deviance)) + 
    geom_line() + geom_point() + 
    scale_x_log10()
}

