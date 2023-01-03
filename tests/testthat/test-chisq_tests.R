test_that("chisq frequency ", {
  O <- c(100, 55, 45)
  E <- c(130, 50, 20)
  output <- chisq_test(O, E)
  expect_equal(output$chisq_value, 38.6730769)
  expect_equal(output$df, 2)
  expect_equal(output$p_value, 4.001732e-09)
})


test_that("contingency table", {
  data <- matrix(c(2, 3, 11, 16, 76, 86, 73, 76), nrow = 4, ncol = 2)
  output <- chisq_test(data)
  expect_equal(output$chisq_value, 16.4328864)
  expect_equal(output$df, 3)
  expect_equal(output$p_value, 0.0009242615)
})
