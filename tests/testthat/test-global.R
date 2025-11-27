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
  k = sample(2:100, size = 1)
  expect_equal(NROW(TernaryMeshCentroids(k)), k^2)
  expect_equal(TernaryMeshCentroids(k)[,'id'], 1:k^2)
  expect_equal(rowSums(TernaryMeshCentroids(k)[,2:4]), rep(1, k^2))
  expect_equivalent(prop.table(apply(TernaryMeshCentroids(k)[,2:4], 2, GeometricMean)), rep(1/3, 3))
})

test_that('Argument checks work', {
  P <- as.data.frame(prop.table(matrix(runif(300), 100), margin = 1))
  # missing main arguments
  expect_error(Tricolore(p1 = 'V1', p2 = 'V2', p3 = 'V3'),
               'main argument missing')
  expect_error(Tricolore(P, p2 = 'V2', p3 = 'V3'),
               'main argument missing')
  expect_error(Tricolore(P, p1 = 'V1', p3 = 'V3'),
               'main argument missing')
  expect_error(Tricolore(P, p1 = 'V1', p2 = 'V2'),
               'main argument missing')
  expect_error(Tricolore(P, p1 = 'Foo1', p2 = 'V2', p3 = 'V3'),
               'Foo1 not found')
  expect_error(Tricolore(P, p1 = 'V1', p2 = 'Foo2', p3 = 'V3'),
               'Foo2 not found')
  expect_error(Tricolore(P, p1 = 'V1', p2 = 'V2', p3 = 'Foo3'),
               'Foo3 not found')
  # type checks for main arguments
  expect_error(Tricolore(as.matrix(P), p1 = 'V1', p2 = 'V2', p3 = 'V3'),
               'df is not a data frame')
  expect_error(Tricolore(P, p1 = 1, p2 = 2, p3 = 3),
               'not a string')
  expect_error(Tricolore(data.frame(V1 = as.character(P$V1), V2 = P$V2, V3 = P$V3),
                         p1 = 'V1', p2 = 'V2', p3 = 'V3'),
               'variable V1 is not numeric')
  expect_error(Tricolore(data.frame(V1 = P$V1, V2 = as.character(P$V2), V3 = P$V3),
                         p1 = 'V1', p2 = 'V2', p3 = 'V3'),
               'variable V2 is not numeric')
  expect_error(Tricolore(data.frame(V1 = P$V1, V2 = P$V2, V3 = as.character(P$V3)),
                         p1 = 'V1', p2 = 'V2', p3 = 'V3'),
               'variable V3 is not numeric')
  expect_error(Tricolore(data.frame(V1 = -P$V1, V2 = P$V2, V3 = P$V3),
                         p1 = 'V1', p2 = 'V2', p3 = 'V3'),
               'variable V1 contains negative values')
  expect_error(Tricolore(data.frame(V1 = P$V1, V2 = -P$V2, V3 = P$V3),
                         p1 = 'V1', p2 = 'V2', p3 = 'V3'),
               'variable V2 contains negative values')
  expect_error(Tricolore(data.frame(V1 = P$V1, V2 = P$V2, V3 = -P$V3),
                         p1 = 'V1', p2 = 'V2', p3 = 'V3'),
               'variable V3 contains negative values')
})

# NA, Inf, NaN are allowed and are expected to return NA as color
test_that('NA, Inf, NaNs in input return NA in output', {
  P <- data.frame(a = c(1, NA), b = c(0, 0.5), c = c(0, 0.2))
  tric <- Tricolore(P, 'a', 'b', 'c', breaks = Inf)
  expect_equal(tric$rgb, c('#F0C500', NA))
  expect_true(all(c('gg', 'ggplot') %in% class(tric$key)))
  P <- data.frame(a = c(1, Inf), b = c(0, 0.5), c = c(0, 0.2))
  tric <- Tricolore(P, 'a', 'b', 'c', breaks = Inf)
  expect_equal(tric$rgb, c('#F0C500', NA))
  expect_true(all(c('gg', 'ggplot') %in% class(tric$key)))
  P <- data.frame(a = c(1, NaN), b = c(0, 0.5), c = c(0, 0.2))
  tric <- Tricolore(P, 'a', 'b', 'c', breaks = Inf)
  expect_equal(tric$rgb, c('#F0C500', NA))
  expect_true(all(c('gg', 'ggplot') %in% class(tric$key)))
})
