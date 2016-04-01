context("marginal_effect")

library("data.table")

test_that("marginal_effect reproduces a simple model", {
  simple_model <- function(data) {
    data[[2]]
  }

  predict_simple_model <- function(model, data) {
    model(data)
  }

  test_data <- data.table(a = runif(10),
                          b = seq(10),
                          c = runif(10))

  me_a <- marginal_effect(data = test_data,
                          var  = "a",
                          model = simple_model,
                          predict_model = predict_simple_model,
                          range = seq(10))
  me_b <- marginal_effect(data = test_data,
                          var  = "b",
                          model = simple_model,
                          predict_model = predict_simple_model,
                          range = seq(10))

  expect_equal(me_a$effect, rep(5.5, 10))
  expect_equal(me_b$effect, seq(10))
})
