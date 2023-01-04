test_that("CI_t_dist test", {
  set.seed(2022)
  data <- rnorm(1e2)
  ci <- CI_t_dist(data)
  expect_equal(
    ci,
    structure(
      list(
        sample_mean = 0.138746011519751,
        t = 1.98421695158642,
        e.s.e = 0.102009311527046,
        interval_min = -0.0636625936318743,
        interval_max = 0.341154616671376,
        level = 0.95
      ),
      class = "CI_t_dist"
    )
  )
})

test_that("CI_t_dist priynout", {
  set.seed(2023)
  data <- rnorm(1e2)
  ci <- CI_t_dist(data)
  expect_equal(capture_output(print(ci)), "0.95% confidence interval is: (-0.129516907933618, 0.264553514407626)
0.0675183032370042 +- 1.98421695158642 * 0.0993012437541613")
})
