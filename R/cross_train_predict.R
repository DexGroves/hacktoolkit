#' Train and predict out-of-fold so that predictions can be used in
#' downstream models. Parallelise by calling registerDoMC(n_cores)
#' before the cross_train_predict call.
#'
#' @import foreach
#' @import doMC
#' @export
#'
#' @param data any training data format that can be indexed by [i, ]
#' @param fold_id integer sequence denoting folds starting at 1
#' @param train_model a function with signature data => model
#' @param predict_model a function with signature
#' (model, data) => predictions. Defaults to predict.
#'
#' @return out of fold model predictions
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
#' train <- diamonds[order(runif(nrow(diamonds))), ][1:10000, ]
#'
#' train_model <- function(data) {
#'   train_mm <- model.matrix(price ~ ., data)
#'   train_y  <- train$price
#'   train_dm <- xgb.DMatrix(train_mm, label = data$price)
#'
#'   xgb.train(params = list(max_depth = 2,
#'                           subsample = 0.5,
#'                           eta = 0.1,
#'                           colsample_bytree = 0.5,
#'                           objective = "reg:linear"),
#'             nrounds = 100,
#'             data = train_dm)
#' }
#'
#' predict_model <- function(model, data) {
#'   score_mm <- model.matrix(price ~ ., data)
#'   score_dm <- xgb.DMatrix(score_mm)
#'
#'   predict(model, newdata = score_dm)
#' }
#'
#' fold_id <- sample(1:10, nrow(train), TRUE)
#' ct_preds <- cross_train_predict(train, fold_id, train_model, predict_model)
cross_train_predict <- function(data, fold_id,
                                train_model, predict_model = predict) {
  models <- train_folds(data, fold_id, train_model)
  prediction_matrix <- predict_folds(data, fold_id, models, predict_model)

  fold_index <- cbind(seq(nrow(prediction_matrix)), fold_id)

  prediction_matrix[fold_index]
}

#' Train crossfold models. Parallelise with registerDoMC.
#' @inheritParams cross_train_predict
#' @export
#' @return list of models
train_folds <- function(data, fold_id, train_model) {
  foreach (heldout_fold = seq(max(fold_id))) %dopar% {
    train_rows <- fold_id != heldout_fold
    train_model(data[train_rows, ])
  }
}

#' Get a matrix of predictions for a list of models. Parallelise
#' with registerDoMC (if you want, I guess).
#' @inheritParams cross_train_predict
#' @export
#' @return matrix of predictions
predict_folds <- function(data, fold_id, models, predict_model) {
  foreach (heldout_fold = seq(max(fold_id)), .combine = cbind) %dopar% {
    predict_model(models[[heldout_fold]], data)
  }
}
