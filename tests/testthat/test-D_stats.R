test_that("simple sumetric variable", {
  set.seed(2023)
  x <- rnorm(1e3)
  d_stats <- D_stats(x)
  expected <- c(D1 = 0.02233401, D2 = 0.01622636, D3 = 0.01000683)
  expect_equal(d_stats, expected, tolerance = T)
})
