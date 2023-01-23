test_that("create residuals", {
  mtcars.mod <- lm(mpg ~ wt, data = mtcars)
  fromFunction <- residuals_list(mtcars.mod)

  created <- structure(
    data.frame(
      fitted = stats::fitted.values(mtcars.mod),
      raw = stats::residuals(mtcars.mod),
      standardized = stats::rstandard(mtcars.mod),
      studentized = stats::rstudent(mtcars.mod)
    ),
    class = "residuals.data"
  )

  expect_equal(fromFunction, created)
})

test_that("Only for lm", {
  expect_error(residuals_list("Not a lm"))
})

test_that("plot works", {
  lm(mpg ~ wt, data = mtcars) |>
    residuals_list() |>
    plot() |>
    expect_s3_class("ggplot")
})

test_that("print works", {
  lm(mpg ~ wt, data = mtcars) |>
    residuals_list() |>
    print() |>
    expect_s3_class("data.frame")
})
