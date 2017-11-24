context('test-global.R')

test_that("GeometricMean() works", {
  expect_equal(GeometricMean(0:4), exp(mean(log(1:4))))
  expect_equal(GeometricMean(0:4, zero.rm = FALSE), 0)
  expect_equal(GeometricMean(c(NA, 0:4), na.rm = TRUE, zero.rm = FALSE), 0)
  expect_equal(GeometricMean(c(NA, 0:4), na.rm = FALSE, zero.rm = FALSE), as.numeric(NA))
  expect_equal(GeometricMean(0:4, na.rm = FALSE, zero.rm = TRUE), exp(mean(log(1:4))))
  expect_equal(GeometricMean(c(NA, 0:4), na.rm = FALSE, zero.rm = TRUE), as.numeric(NA))
  expect_equal(GeometricMean(0, zero.rm = TRUE), NaN)
})
