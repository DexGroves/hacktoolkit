#' Get pseudo marginal effects from any model.
#'
#' Works by setting everyone in the data to a range of values in the
#' variable of interest and scoring repeatedly. Very slow. Only
#' compatible with data.tables.
#'
#' @import data.table
#' @export
#'
#' @param data data.table object on which to score
#' @param var string naming the variable of interest
#' @param model target model object
#' @param predict_model a function with signature
#' (model, data) => predictions. Defaults to predict.
#' @param range of values to test. Defaults to 10 evenly spaced
#' points between the min and max of the target variable.
#'
#' @return data.table of the variable and its derived marginal effect
#'
#' @examples
#' library("xgboost")
#' library("ggplot2")
#' library("foreach")
#' library("doMC")
#'
#' data(diamonds)
#'
#' set.seed(1234)
#'
#' predict_model <- function(model, data) {
#'   score_mm <- model.matrix(price ~ ., data)
#'   score_dm <- xgb.DMatrix(score_mm)
#'   predict(model, newdata = score_dm)
#' }
#'
#' train <- data.table(diamonds[order(runif(nrow(diamonds))), ][1:10000, ])
#' train_mm <- model.matrix(price ~ ., train)
#' train_y  <- train$price
#' train_dm <- xgb.DMatrix(train_mm, label = train$price)
#'
#' model <- xgb.train(params = list(max_depth = 2,
#'                                  subsample = 0.5,
#'                                  eta = 0.1,
#'                                  colsample_bytree = 0.5,
#'                                  objective = "reg:linear"),
#'                    nrounds = 10,
#'                    data = train_dm)
#'
#' marginal_effect(train, "x", model, predict_model)
marginal_effect <- function(data, var, model, predict_model = predict,
                            range = -1) {
  if (!is.data.table(data)) {
    stop("Incompatible with non data.tables!", call. = FALSE)
  }

  if (length(range) <= 1) {
    range <- seq(min(data[[var]]), max(data[[var]]), length.out = 10)
  }

  working_data <- copy(data)

  # Handling this with a for vs an apply allows use of `:=` without pain
  out_vec <- double(length(range))
  for (i in seq_along(range)) {
    working_data[, var := range[i], with = FALSE]
    prediction <- predict_model(model, working_data)
    out_vec[i] <- mean(prediction)
  }

  data.table(var = range, effect = out_vec)
}
