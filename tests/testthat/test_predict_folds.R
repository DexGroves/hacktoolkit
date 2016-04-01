context("predict_folds")


library("ggplot2")
library("doMC")
library("foreach")

registerDoMC(1)

test_that("predict_folds indexes properly", {
  train_model <- function(data) {
    glm(price ~ ., data = data)
  }

  predict_model <- function(model, data) {
    predict(model, newdata = data)
  }

  set.seed(1234)
  train <- diamonds[order(runif(nrow(diamonds))), ][1:1000, ]

  fold_id <- sample(1:4, nrow(train), TRUE)
  models <- train_folds(train, fold_id, train_model)

  pred_matrix <- predict_folds(train, fold_id, models, predict_model)

  expect_equal(pred_matrix[, 1], predict(models[[1]], train))
  expect_equal(pred_matrix[, 2], predict(models[[2]], train))
  expect_equal(pred_matrix[, 3], predict(models[[3]], train))
  expect_equal(pred_matrix[, 4], predict(models[[4]], train))
})
