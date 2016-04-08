context("loo_encode")


test_that("loo_encode behaving as expected", {
  test_data <- data.frame(
    id = c(rep("a", 5), rep("b", 3), rep("c", 2), "d"),
    resp = c(1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1)
  )

  encoding <- loo_encode(test_data$id, test_data$resp)
  expected_encoding <- c(0, 0.25, 0.25, 0.25, 0.25, 0, 0.5, 0.5, 0, 1, NA)

  expect_equal(encoding, expected_encoding)
})
