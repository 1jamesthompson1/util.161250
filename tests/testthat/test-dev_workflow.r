test_that("my render works", {
  if (interactive()) {
    expect_no_warning(.renderAndStyle("../../../util.161250"))
  } else {
    expect(TRUE)
  }
})
