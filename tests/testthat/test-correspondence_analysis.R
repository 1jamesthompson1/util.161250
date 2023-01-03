test_that("row profile", {
  tabledata <- structure(c(
    18, 4, 10, 25, 4, 57, 10, 13, 22, 5, 13, 4, 2, 4,
    2
  ), dim = c(5L, 3L))

  observed <- data_profile(tabledata)

  expect_equal(observed, structure(c(
    0.204545454545455, 0.222222222222222, 0.4, 0.490196078431373,
    0.363636363636364, 0.647727272727273, 0.555555555555556, 0.52,
    0.431372549019608, 0.454545454545455, 0.147727272727273, 0.222222222222222,
    0.08, 0.0784313725490196, 0.181818181818182
  ), dim = c(5L, 3L), class = "table_profile", rowprofile = TRUE))
})

test_that("col profile", {
  tabledata <- structure(c(
    18, 4, 10, 25, 4, 57, 10, 13, 22, 5, 13, 4, 2, 4,
    2
  ), dim = c(5L, 3L))

  observed <- data_profile(tabledata, FALSE)

  expect_equal(observed, structure(c(
    0.295081967213115, 0.0373831775700935, 0.4, 0.409836065573771,
    0.0373831775700935, 2.28, 0.163934426229508, 0.121495327102804,
    0.88, 0.0819672131147541, 0.121495327102804, 0.16, 0.0327868852459016,
    0.0373831775700935, 0.08
  ), dim = c(5L, 3L), class = "table_profile", rowprofile = FALSE))
})

test_that("plot without error", {
  tabledata <- structure(c(
    18, 4, 10, 25, 4, 57, 10, 13, 22, 5, 13, 4, 2, 4,
    2
  ), dim = c(5L, 3L))

  observed <- data_profile(tabledata)

  expect_no_warning(plot(observed))
})

test_that("plot without error", {
  tabledata <- structure(c(
    18, 4, 10, 25, 4, 57, 10, 13, 22, 5, 13, 4, 2, 4,
    2
  ), dim = c(5L, 3L))

  observed <- data_profile(tabledata, FALSE)

  expect_no_warning(plot(observed))
})
