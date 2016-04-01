context("train_folds")


library("ggplot2")
library("doMC")
library("foreach")

registerDoMC(1)

test_that("train_folds doesn't leak data", {
  train_model <- function(data) {
    glm(price ~ ., data = data)
  }

  set.seed(1234)
  train <- diamonds[order(runif(nrow(diamonds))), ][1:1000, ]

  fold_id <- sample(1:4, nrow(train), TRUE)
  models <- train_folds(train, fold_id, train_model)

  expect_equal(models[[1]]$data, train[fold_id != 1, ])
  expect_equal(models[[2]]$data, train[fold_id != 2, ])
  expect_equal(models[[3]]$data, train[fold_id != 3, ])
  expect_equal(models[[4]]$data, train[fold_id != 4, ])
})
