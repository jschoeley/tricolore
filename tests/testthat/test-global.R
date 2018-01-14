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
  expect_equal(prop.table(apply(t(t(P)/Centre(P)), 2, GeometricMean)), rep(1/3, 3))
  expect_equal(NROW(Centre(P)), 3)
  expect_equal(NCOL(Centre(P)), 1)
})


test_that('Pertube() works', {
  P <- prop.table(matrix(runif(300), 100), margin = 1)
  expect_equal(Pertube(P, rep(1/3, 3)), P)
  expect_equal(Centre(Pertube(P, 1/Centre(P))), rep(1/3, 3))
  expect_equal(NROW(Pertube(P, rep(1/3, 3))), 100)
  expect_equal(NCOL(Pertube(P, rep(1/3, 3))), 3)
})

test_that('TernaryMeshCentroids() works', {
  k = sample(1:100, size = 1)
  expect_equal(NROW(TernaryMeshCentroids(k)), k^2)
  expect_equal(TernaryMeshCentroids(k)[,'id'], 1:k^2)
  expect_equal(rowSums(TernaryMeshCentroids(k)[,2:4]), rep(1, k^2))
  expect_equivalent(prop.table(apply(TernaryMeshCentroids(k)[,2:4], 2, GeometricMean)), rep(1/3, 3))
})

test_that('Argument checks work', {
  P <- as.data.frame(prop.table(matrix(runif(300), 100), margin = 1))
  expect_error(Tricolore(as.matrix(P), p1 = V1, p2 = V2, p3 = V3),
               'df is not a data frame')
})
