# context("marginal_effect")


# library("ggplot2")

# registerDoMC(1)

# test_that("marginal_effect reproduces a simple model", {
#   #' Returns the second column of the data
#   simple_model <- function(data) {
#     data[, 2]
#   }

#   predict_simple_model <- function(model, data) {
#     model(data)
#   }

#   test_data <- data.frame(a = runif(10),
#                           b = seq(10),
#                           c = runif(10))

#   me_a <- marginal_effect(data = test_data,
#                           var  = "a",
#                           range = seq(10),
#                           model = simple_model,
#                           predict_fn = predict_simple_model)
#   me_b <- marginal_effect(data = test_data,
#                           var  = "b",
#                           range = seq(10),
#                           model = simple_model,
#                           predict_fn = predict_simple_model)

#   expect_equal(me_a$effect, rep(5, 10))
#   expect_equal(me_b$effect, seq(10))
# })
