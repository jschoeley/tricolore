context('test-global.R')

test_that('GeometricMean() works', {
  expect_equal(GeometricMean(0:4), exp(mean(log(1:4))))
  expect_equal(GeometricMean(0:4, zero.rm = FALSE), 0)
  expect_equal(GeometricMean(c(NA, 0:4), na.rm = TRUE, zero.rm = FALSE), 0)
  expect_equal(GeometricMean(c(NA, 0:4), na.rm = FALSE, zero.rm = FALSE), as.numeric(NA))
  expect_equal(GeometricMean(0:4, na.rm = FALSE, zero.rm = TRUE), exp(mean(log(1:4))))
  expect_equal(GeometricMean(c(NA, 0:4), na.rm = FALSE, zero.rm = TRUE), as.numeric(NA))
  expect_equal(GeometricMean(0, zero.rm = TRUE), NaN)
})

test_that('Centre() works', {
  P <- prop.table(matrix(runif(300), 100), margin = 1)
  expect_equal(prop.table(apply(Centre(P), 2, GeometricMean)), rep(1/3, 3))
  expect_equal(NROW(Centre(P)), 100)
  expect_equal(NCOL(Centre(P)), 3)
})

test_that('GetCentroids() works', {
  k = sample(1:100, size = 1)
  expect_equal(NROW(GetCentroids(k)), k^2)
  expect_equal(GetCentroids(k)[,'id'], 1:k^2)
  expect_equal(rowSums(GetCentroids(k)[,2:4]), rep(1, k^2))
  expect_equivalent(prop.table(apply(GetCentroids(k)[,2:4], 2, GeometricMean)), rep(1/3, 3))
})
