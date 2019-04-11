# Misc --------------------------------------------------------------------

# from nnet::which.is.max()
MaxIndex <- function (x) {
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) { sample(y, 1L) } else { y }
}

#' Validate Main Arguments
#'
#' Validate main arguments of tricolore function.
#'
#' @param df Data frame of compositions.
#' @param p1 Column name for variable in df giving first proportion
#'           of ternary composition (string).
#' @param p2 Column name for variable in df giving second proportion
#'           of ternary composition (string.
#' @param p3 Column name for variable in df giving third proportion
#'           of ternary composition (string).
#'
#' @importFrom assertthat assert_that is.string
#'
#' @keywords internal
ValidateMainArguments <- function (df, p1, p2, p3) {

  # missing arguments
  assert_that(!missing(df), !missing(p1), !missing(p2), !missing(p3),
              msg = 'main argument missing')
  # compositional data is data frame
  assert_that(is.data.frame(df))
  # variable names as strings
  assert_that(is.string(p1), is.string(p2), is.string(p3))
  # missing variables in data frame
  assert_that(p1 %in% names(df), msg = paste('variable', p1 ,'not found in df'))
  assert_that(p2 %in% names(df), msg = paste('variable', p2 ,'not found in df'))
  assert_that(p3 %in% names(df), msg = paste('variable', p3 ,'not found in df'))
  # compositional data is numeric
  assert_that(is.numeric(df[[p1]]), msg = paste('variable', p1 ,'is not numeric'))
  assert_that(is.numeric(df[[p2]]), msg = paste('variable', p2 ,'is not numeric'))
  assert_that(is.numeric(df[[p3]]), msg = paste('variable', p3 ,'is not numeric'))
  # compositional data is not negative
  assert_that(!any(df[[p1]] < 0, na.rm = TRUE),
              msg = paste('variable', p1 ,'contains negative values'))
  assert_that(!any(df[[p2]] < 0, na.rm = TRUE),
              msg = paste('variable', p2 ,'contains negative values'))
  assert_that(!any(df[[p3]] < 0, na.rm = TRUE),
              msg = paste('variable', p3 ,'contains negative values'))
  # NA, Inf, NaN are allowed and are expected to return NA as color

}

#' Validate Shared Parameters
#'
#' Validate parameters shared across tricolore functions.
#'
#' @param pars A named list of parameters.
#'
#' @importFrom assertthat assert_that is.scalar is.flag
#'
#' @keywords internal
ValidateParametersShared <- function (pars) {

  with(pars, {
    # center either NA or three element numeric vector
    # with sum 1 and elements > 0
    assert_that((is.scalar(center) && is.na(center)) ||
                  (length(center) == 3L &&
                     all(is.numeric(center)) &&
                     sum(center) == 1 &&
                     all(center != 0)),
                msg = 'center must be either NA or a three element numeric vector with sum == 1 and all element > 0.')
    # flags
    assert_that(is.flag(legend), is.flag(show_data),
                is.flag(show_center), is.flag(crop))
    # character options
    assert_that(is.scalar(label_as),
                is.character(label_as),
                label_as %in% c('pct', 'pct_diff'),
                msg = 'label_as must be either "pct" or "pct_diff".')
  })

}

#' Validate Tricolore Parameters
#'
#' Validate parameters of Tricolore function.
#'
#' @param pars A named list of parameters.
#'
#' @importFrom assertthat assert_that is.number is.scalar
#'
#' @keywords internal
ValidateParametersTricolore <- function (pars) {

  # a modified version of assertthat::is.count that regards
  # infinite values as counts
  is.count2 <- function (x) {
    if (length(x) != 1) return(FALSE)
    integerish <- is.integer(x) || (is.numeric(x) && (x == trunc(x)))
    if (!integerish) return(FALSE)
    x > 0
  }

  with(pars, {
    # breaks is count scalar > 1 (can't use is.count() because
    # it throws an error when encountering infinite values)
    assert_that(is.scalar(breaks), is.count2(breaks), breaks > 1)
    # hue is numeric scalar in range [0, 1]
    assert_that(is.number(hue), hue >= 0 && hue <= 1)
    # chroma is numeric scalar in range [0, 1]
    assert_that(is.number(chroma), chroma >= 0 && chroma <= 1)
    # lightness is numeric scalar in range [0, 1]
    assert_that(is.number(lightness), lightness >= 0 && lightness <= 1)
    # contrast is numeric scalar in range [0, 1]
    assert_that(is.number(contrast), contrast >= 0 && contrast <= 1)
    # spread is positive numeric scalar
    assert_that(is.number(spread), spread > 0, is.finite(spread))
  })

  ValidateParametersShared(pars)

}

#' Validate TricoloreSextant Parameters
#'
#' Validate parameters of TricoloreSextant function.
#'
#' @param pars A named list of parameters.
#'
#' @importFrom assertthat assert_that is.number is.scalar
#'
#' @keywords internal
ValidateParametersTricoloreSextant <- function (pars) {

  with(pars, {
    assert_that(is.character(values), length(values) == 6)
  })

  ValidateParametersShared(pars)

}

# Compositional Data Analysis ---------------------------------------------

#' Geometric Mean
#'
#' Calculate the geometric mean for a numeric vector.
#'
#' @param x Numeric vector.
#' @param na.rm Should NAs be removed? (default=TRUE)
#' @param zero.rm Should zeros be removed? (default=TRUE)
#'
#' @return The geometric mean as numeric scalar.
#'
#' @examples
#' tricolore:::GeometricMean(0:100)
#' tricolore:::GeometricMean(0:100, zero.rm = FALSE)
#'
#' @keywords internal
GeometricMean <- function (x, na.rm = TRUE, zero.rm = TRUE) {
  # the geometric mean can't really deal with elements equal to 0
  # this option removes 0 elements from the vector
  if (zero.rm) { x <- x[x!=0] }
  return(exp(mean(log(x), na.rm = na.rm)))
}

#' Compositional Centre
#'
#' Calculate the centre of a compositional data set.
#'
#' @param P n by m matrix of compositions {p1, ..., pm}_i for i=1,...,n.
#'
#' @return The centre of P as an m element numeric vector.
#'
#' @examples
#' P <- prop.table(matrix(runif(300), 100), margin = 1)
#' tricolore:::Centre(P)
#'
#' @references
#' Von Eynatten, H., Pawlowsky-Glahn, V., & Egozcue, J. J. (2002).
#' Understanding perturbation on the simplex: A simple method to better
#' visualize and interpret compositional data in ternary diagrams.
#' Mathematical Geology, 34(3), 249-257.
#'
#' Pawlowsky-Glahn, V., Egozcue, J. J., & Tolosana-Delgado, R. (2007). Lecture
#' Notes on Compositional Data Analysis. Retrieved from
#' https://dugi-doc.udg.edu/bitstream/handle/10256/297/CoDa-book.pdf
#'
#' @keywords internal
Centre <- function (P) {
  # calculate the geometric mean for each element of the composition
  g <- apply(P, MARGIN = 2, FUN = GeometricMean)
  # the closed vector of geometric means is the mean (centre)
  # of the compositional data set
  return(g/sum(g))
}

#' Compositional Pertubation
#'
#' Pertubate a compositional data set by a compositional vector.
#'
#' @param P n by m matrix of compositions {p1, ..., pm}_i for i=1,...,n.
#' @param c Compositional pertubation vector {c1, ..., cm}.
#'
#' @return n by m matrix of pertubated compositions.
#'
#' @examples
#' P <- prop.table(matrix(runif(12), 4), margin = 1)
#' cP <- tricolore:::Pertube(P, 1/tricolore:::Centre(P))
#' tricolore:::Centre(cP)
#'
#' @references
#' Von Eynatten, H., Pawlowsky-Glahn, V., & Egozcue, J. J. (2002).
#' Understanding perturbation on the simplex: A simple method to better
#' visualize and interpret compositional data in ternary diagrams.
#' Mathematical Geology, 34(3), 249-257.
#'
#' Pawlowsky-Glahn, V., Egozcue, J. J., & Tolosana-Delgado, R. (2007). Lecture
#' Notes on Compositional Data Analysis. Retrieved from
#' https://dugi-doc.udg.edu/bitstream/handle/10256/297/CoDa-book.pdf
#'
#' @keywords internal
Pertube <- function (P, c = rep(1/3, 3)) {
  return(prop.table(t(t(P)*c), margin = 1))
}

#' Compositional Powering
#'
#' Raise a compositional data-set to a given power.
#'
#' @param P n by m matrix of compositions {p1, ..., pm}_i for i=1,...,n.
#' @param scale Power scalar.
#'
#' @return n by m numeric matrix of powered compositions.
#'
#' @examples
#' P <- prop.table(matrix(runif(12), 4), margin = 1)
#' tricolore:::PowerScale(P, 2)
#'
#' @references
#' Pawlowsky-Glahn, V., Egozcue, J. J., & Tolosana-Delgado, R. (2007). Lecture
#' Notes on Compositional Data Analysis. Retrieved from
#' https://dugi-doc.udg.edu/bitstream/handle/10256/297/CoDa-book.pdf
#'
#' @keywords internal
PowerScale <- function (P, scale = 1) {
  return(prop.table(P^scale, margin = 1))
}

# Ternary Geometry --------------------------------------------------------

# T(K=k^2):   Equilateral triangle subdivided into K equilateral sub-triangles.
#             Each side of T is divided into k intervals of equal length.
# (p1,p2,p3): Barycentric coordinates wrt. T(K).
# id:         One-dimensional index of sub-triangles in T(K).
#
#                  p2           id index
#                  /\               9
#                 /  \            6 7 8
#                /____\         1 2 3 4 5
#              p1      p3

#' Centroid Coordinates of Sub-Triangles in Segmented Equilateral Triangle
#'
#' Segment an equilateral triangle into k^2 equilateral sub-triangles and return
#' the barycentric centroid coordinates of each sub-triangle.
#'
#' @param k Number of rows in the segmented equilateral triangle.
#'
#' @return A numeric matrix of with index and barycentric centroid coordinates
#'   of regions id=1,...,k^2.
#'
#' @references
#' S. H. Derakhshan and C. V. Deutsch (2009): A Color Scale for Ternary Mixtures.
#'
#' @examples
#' tricolore:::TernaryMeshCentroids(1)
#' tricolore:::TernaryMeshCentroids(2)
#' tricolore:::TernaryMeshCentroids(3)
#'
#' @keywords internal
TernaryMeshCentroids <- function (k) {
  # total number of centroids and centroid id
  K = k^2; id = 1:K

  # centroid coordinates as function of K and id
  g <- floor(sqrt(K-id)); gsq <- g^2
  c1 <- (((-K + id + g*(g+2) + 1) %% 2) - 3*gsq - 3*id + 3*K + 1) / (6*k)
  c2 <- -(((-K + gsq + id + 2*g + 1) %% 2) + 3*g - 3*k + 1) / (3*k)
  c3 <- (((-K + gsq + id + 2*g + 1) %% 2) + 3*gsq + 6*g + 3*id - 3*K + 1) / (6*k)

  return(cbind(id = id, p1 = c1, p2 = c2, p3 = c3))
}

#' Vertex Coordinates of Sub-Triangles in Segmented Equilateral Triangle
#'
#' Given the barycentric centroid coordinates of the sub-triangles in an
#' equilateral triangle subdivided into k^2 equilateral sub-triangles, return
#' the barycentric vertex coordinates of each sub-triangle.
#'
#' @param C n by 4 matrix of barycentric centroid coordinates of n=k^2
#'          sub-triangles. Column order: id, p1, p2, p3 with id=1,...,k^2.
#' @param k Number of rows in the segmented equilateral triangle.
#'
#' @return A numeric matrix with index, vertex id and barycentric vertex
#'   coordinates for each of the k^2 sub-triangles.
#'
#' @examples
#' k = 2
#' C <- tricolore:::TernaryMeshCentroids(k)
#' tricolore:::TernaryMeshVertices(C)
#'
#' @references
#' S. H. Derakhshan and C. V. Deutsch (2009): A Color Scale for Ternary Mixtures.
#'
#' @keywords internal
TernaryMeshVertices <- function (C) {
  k <- sqrt(nrow(C))
  j <- k - floor(sqrt(k^2-C[,1]))
  i <- C[,1] - (j-1)*(2*k-j+1)
  term1 <- ((-1)^(i %% 2) * 2) / (3*k)
  term2 <- ((-1)^(i %% 2)) / (3*k)

  v1 <- cbind(C[,2] - term1, C[,3] + term2, C[,4] + term2)
  v2 <- cbind(C[,2] + term2, C[,3] - term1, C[,4] + term2)
  v3 <- cbind(C[,2] + term2, C[,3] + term2, C[,4] - term1)

  V <- cbind(C[,1], rep(1:3, each = nrow(C)), rbind(v1, v2, v3))
  colnames(V) <- c('id', 'vertex', 'p1', 'p2', 'p3')

  return(V)
}

#' Distance Between Points in Ternary Coordinates
#'
#' The distances between ternary coordinate p and a set of ternary coordinates C.
#'
#' @param p A vector of ternary coordinates {p1, p2, p3}.
#' @param C n by 3 matrix of ternary coordinates {p1, p2, p3}_i for i=1,...,n.
#'
#' @return A numeric vector of distances between coordinate p and all
#'   coordinates in C.
#'
#' @references
#' https://en.wikipedia.org/wiki/Barycentric_coordinate_system#Distance_between_points
#'
#' @examples
#' p <- c(0.5, 0.2, 0.3)
#' C <- prop.table(matrix(runif(3*10), ncol = 3), 1)
#' tricolore:::TernaryDistance(p, C)
#'
#' @keywords internal
TernaryDistance <- function(p, C) {
  Q <- t(p-t(C))
  return(-Q[,2]*Q[,3]-Q[,3]*Q[,1]-Q[,1]*Q[,2])
}

#' For Ternary Coordinates P Return the Nearest Coordinate in Set C
#'
#' @param P,C n by 3 matrix of ternary coordinates {p1, p2, p3}_i for
#'            i=1,...,n. n may be different for P and C.
#'
#' @return n by 3 matrix of ternary coordinates in C.
#'
#' @examples
#' P <- prop.table(matrix(runif(9), ncol = 3), 1)
#' C <- tricolore:::TernaryMeshCentroids(2)[,-1]
#' tricolore:::TernaryNearest(P, C)
#'
#' @keywords internal
TernaryNearest <- function (P, C) {
  id <- apply(P, 1, function (x) MaxIndex(-TernaryDistance(x, C)))
  return(C[id,])
}

#' Return Ternary Gridlines Centered Around Some Composition
#'
#' @param center The center of the grid.
#'   A vector of ternary coordinates {p1, p2, p3}.
#' @param spacing The spacing of the grid in percent-point increments.
#'   A numeric > 0.
#'
#' @return A list of lists.
#'
#' @examples
#' tricolore:::TernaryCenterGrid(c(1/6, 2/6, 3/6), 10)
#'
#' @keywords internal
TernaryCenterGrid <- function (center, spacing) {

  # -1 to 1 by spacing/100 with 0 point
  div_seq <- seq(0, 1, spacing/100)
  div_seq <- c(-rev(div_seq), div_seq[-1])

  # proportion difference from center for all three ternary axes.
  # keep only possible values
  div_seq <- list(
    p1 = div_seq[div_seq >= -center[1] & div_seq <= 1-center[1]],
    p2 = div_seq[div_seq >= -center[2] & div_seq <= 1-center[2]],
    p3 = div_seq[div_seq >= -center[3] & div_seq <= 1-center[3]]
  )

  # percent-point difference from center composition
  labels <- lapply(div_seq, function(x) formatC(x*100, flag = '+'))
  # label center point as percent share
  center_pct <- paste0(formatC(center*100, digits = 1, format = 'f'), '%')
  labels[['p1']][labels[['p1']] == '-0'] <- center_pct[1]
  labels[['p2']][labels[['p2']] == '-0'] <- center_pct[2]
  labels[['p3']][labels[['p3']] == '-0'] <- center_pct[3]


  # breaks in ternary coordinates
  breaks <- list(
    p1 = div_seq[['p1']] + center[1],
    p2 = div_seq[['p2']] + center[2],
    p3 = div_seq[['p3']] + center[3]
  )

  return(list(breaks = breaks, labels = labels))
}

#' Return the Limits of Ternary Coordinates
#'
#' @param P n by 3 matrix of ternary coordinates {p1, p2, p3}_i for
#'          i=1,...,n.
#' @param na.rm Should NAs be removed? (default=TRUE)
#'
#' @return A 2 by 3 matrix of lower and upper limits for p1, p2 and p3.
#'
#' @examples
#' P <- prop.table(matrix(runif(9), ncol = 3), 1)
#' tricolore:::TernaryLimits(P)
#'
#' @keywords internal
TernaryLimits <- function (P, na.rm = TRUE) {
  limits <- matrix(NA, nrow = 2, ncol = 3,
                   dimnames = list(c('lower', 'upper'),
                                   c('p1', 'p2', 'p3')))
  limits[1,] <- apply(P, 2, min, na.rm = na.rm)
  limits[2,] <- c(1 - (limits[1,2] + limits[1,3]),
                  1 - (limits[1,1] + limits[1,3]),
                  1 - (limits[1,1] + limits[1,2]))
  return(limits)
}

#' Vertex Coordinates of Sextants in Equilateral Triangle
#'
#' Given a barycentric center coordinate return the vertex coordinates of the
#' of the sextant regions.
#'
#' @param center The sextant center.
#'   A vector of ternary coordinates {p1, p2, p3}.
#'
#' @return Index, vertex id and barycentric vertex coordinates for each of the
#'         6 sextants.
#'
#' @examples
#' tricolore:::TernarySextantVertices(rep(1/3, 3))
#'
#' @keywords internal
TernarySextantVertices <- function (center) {

  # define corner points
  p1 = c(1, 0, 0); p2 = c(0, 1, 0); p3 = c(0, 0, 1)
  a1 <- c(center[1], 1-center[1], 0); a2 <- c(center[1], 0, 1-center[1])
  b1 <- c(0, center[2], 1-center[2]); b2 <- c(1-center[2], center[2], 0)
  c1 <- c(1-center[3], 0, center[3]); c2 <- c(0, 1-center[3], center[3])

  # ternary sextant vertices
  V <- cbind(
    id =
      c(rep(1, 5), rep(2, 4),
        rep(3, 5), rep(4, 4),
        rep(5, 5), rep(6, 4)),
    vertex = rep(c(1:5, 1:4), 3),
    matrix(
      c(center, c1, p1, b2, center, # 1
        center, b2, a1, center,     # 2
        center, a1, p2, c2, center, # 3
        center, c2, b1, center,     # 4
        center, b1, p3, a2, center, # 5
        center, a2, c1, center),    # 6
      ncol = 3, nrow = 27, byrow = TRUE,
      dimnames = list(NULL, c('p1', 'p2', 'p3'))
    )
  )

  return(V)

}

#' Return Surrounding Sextant of Barycentric Coordinates
#'
#' Given barycentric coordinates return the id of the surrounding sextant.
#'
#' @param P n by 3 matrix of ternary coordinates {p1, p2, p3}_i for
#'          i=1,...,n.
#' @param center The sextant center.
#'   A vector of ternary coordinates {p1, p2, p3}.
#'
#' @return An n element character vector of sextant id's 1 to 6.
#'
#' @examples
#' P <- prop.table(matrix(runif(9), ncol = 3), 1)
#' tricolore:::TernarySurroundingSextant(P, rep(1/3, 3))
#'
#' @keywords internal
TernarySurroundingSextant <- function (P, center) {
  # six cases, six sextants, NA if at center or NA in input
  is_larger <- t(t(P) > center)
  id <- apply(is_larger, 1, function (x) {
    y <- NA
    if (identical(x, c(TRUE, FALSE, FALSE))) y <- 1
    if (identical(x, c(TRUE, TRUE, FALSE)))  y <- 2
    if (identical(x, c(FALSE, TRUE, FALSE))) y <- 3
    if (identical(x, c(FALSE, TRUE, TRUE)))  y <- 4
    if (identical(x, c(FALSE, FALSE, TRUE))) y <- 5
    if (identical(x, c(TRUE, FALSE, TRUE)))  y <- 6
    y
  })
  return(id)
}

# Ternary Color Maps ------------------------------------------------------

#' CIE-Lch Mixture of Ternary Composition
#'
#' Return the ternary balance scheme colors for a matrix of ternary compositions.
#'
#' @param P n by 3 matrix of ternary compositions {p1, p2, p3}_i for
#'          i=1, ..., n.
#' @param center Ternary coordinates of the grey-point.
#' @param breaks Number of breaks in the discrete color scale. An integer >1.
#'               Values above 99 imply no discretization.
#' @param h_ Primary hue of the first ternary element in angular degrees [0, 360].
#' @param c_ Maximum possible chroma of mixed colors [0, 200].
#' @param l_ Lightness of mixed colors [0, 100].
#' @param contrast Lightness contrast of the color scale [0, 1).
#' @param spread Spread of the color scale around center > 0.
#'
#' @return An n row data frame giving, for each row of the input P, the input
#' proportions (p1, p2, p3), parameters of the color mixture (h, c, l) and the
#' hex-rgb string of the mixed colors (rgb).
#'
#' @examples
#' P <- prop.table(matrix(runif(9), ncol = 3), 1)
#' tricolore:::ColorMapTricolore(P, center = rep(1/3, 3), breaks = 4,
#'                               h_ = 80, c_ = 140, l_ = 80,
#'                               contrast = 0.4, spread = 1)
#'
#' @importFrom grDevices hcl hsv
#'
#' @keywords internal
ColorMapTricolore <- function (P, center, breaks, h_, c_, l_, contrast, spread) {

  ### Discretize ###

  # closing (copy of closed, non-transformed input data for output)
  P <- P_notrans <- prop.table(P, margin = 1)

  # discretize to nearest ternary mesh centroid
  # don't discretize if breaks > 99 to avoid expensive calculations
  # which don't make much of a difference in output
  if (breaks < 100) {
    P <- TernaryNearest(P, TernaryMeshCentroids(breaks)[,-1])
  }

  ### Center and scale ###

  # centering
  P <- Pertube(P, 1/center)
  # scaling
  P <- PowerScale(P, spread)

  ### Colorize ###

  # calculate the chroma matrix C by scaling the row proportions
  # of the input matrix P by the maximum chroma parameter.
  C <- P*c_

  # generate primary colors starting with a hue value in [0, 360) and then
  # picking two equidistant points on the circumference of the color wheel.
  # input hue in degrees, all further calculations in radians.
  phi <- (h_*0.0174 + c(0, 2.09, 4.19)) %% 6.28

  # the complex matrix Z represents each case (i) and group (j=1,2,3) specific
  # color in complex polar form with hue as angle and chroma as radius.
  Z <- matrix(complex(argument = phi, modulus = c(t(C))),
              ncol = 3, byrow = TRUE)

  # adding up the rows gives the CIE-Lab (cartesian) coordinates
  # of the convex color mixture in complex form.
  z <- rowSums(Z)
  # convert the cartesian CIE-Lab coordinates to polar CIE-Luv coordinates
  # and add lightness level.
  M <- cbind(h = (Arg(z)*57.3)%%360, c = Mod(z), l = l_)

  # decrease lightness and chroma towards the center of the color scale
  cfactor <- M[,2]*contrast/c_ + 1-contrast
  M[,3] <- cfactor*M[,3]
  M[,2] <- cfactor*M[,2]

  # convert the complex representation of the color mixture to
  # hex-srgb representation via the hcl (CIE-Luv) color space
  rgb <- hcl(h = M[,1], c = M[,2], l = M[,3],
             alpha = 1, fixup = TRUE)
  # remove alpha information
  rgb <- substr(rgb, 1, 7)

  ### Output ###

  # non-transformed compositions, hcl values of mixtures and rgb code
  result <- data.frame(P_notrans, M[,1], M[,2], M[,3], rgb,
                       row.names = NULL, check.rows = FALSE,
                       check.names = FALSE, stringsAsFactors = FALSE)
  colnames(result) <- c('p1', 'p2', 'p3', 'h', 'c', 'l', 'rgb')
  return(result)
}

#' Sextant Encoding of Ternary Composition
#'
#' Return the sextant scheme colors for a matrix of ternary compositions.
#'
#' @param P n by 3 matrix of ternary compositions {p1, p2, p3}_i for
#'          i=1, ..., n.
#' @param center Ternary coordinates of the sextant meeting point.
#' @param values 6 element character vector of rgb-codes.
#'
#' @return An n row data frame giving, for each row of the input P, the input
#' proportions (p1, p2, p3), sextant id (sextant) and the hex-rgb string of the
#' mixed colors (rgb).
#'
#' @examples
#' P <- prop.table(matrix(runif(9), ncol = 3), 1)
#' tricolore:::ColorMapSextant(P, c(1/3, 1/3, 1/3),
#'                             c('#01A0C6', '#B8B3D8', '#F11D8C', '#FFB3B3',
#'                               '#FFFF00', '#B3DCC3'))
#' @keywords internal
ColorMapSextant <- function (P, center, values) {
  # close composition
  P <- prop.table(P, margin = 1)

  # assign points to sextants and corresponding color codes
  sextant <- TernarySurroundingSextant(P, center)
  rgb <- values[sextant]

  # non-transformed compositions, sextant id and hexsrgb code
  result <- data.frame(P, sextant, rgb,
                       row.names = NULL, check.rows = FALSE,
                       check.names = FALSE, stringsAsFactors = FALSE)
  colnames(result) <- c('p1', 'p2', 'p3', 'sextant', 'rgb')
  return(result)
}

# Ternary Color Keys ------------------------------------------------------

#' Breaks and Labels for Ternary Color Key
#'
#' Return various types of breaks and labels for ternary color keys.
#'
#' @param type   An integer 1, 2, or 3.
#' @param center Ternary coordinates of the grey-point.
#' @param breaks Number of breaks in the discrete color scale. An integer >1.
#'               Values above 99 imply no discretization.
#'
#' @return A list of lists containing breaks and labels for each of the 3
#'   ternary axes.
#'
#' @examples
#' tricolore:::BreaksAndLabels(1, breaks = 3)
#' tricolore:::BreaksAndLabels(2)
#' tricolore:::BreaksAndLabels(3, center = c(1/3, 1/3, 1/3))
#'
#' @keywords internal
BreaksAndLabels <- function (type, center = NULL, breaks = NULL) {
  brk_lab <-
    switch(type,
           list(breaks = list(p1 = seq(0, 1, length.out = breaks+1),
                              p2 = seq(0, 1, length.out = breaks+1),
                              p3 = seq(0, 1, length.out = breaks+1)),
                labels = list(p1 = round(seq(0, 1, length.out = breaks+1)*100, 1),
                              p2 = round(seq(0, 1, length.out = breaks+1)*100, 1),
                              p3 = round(seq(0, 1, length.out = breaks+1)*100, 1))),
           list(breaks = list(p1 = c(0.25, 0.5, 0.75),
                              p2 = c(0.25, 0.5, 0.75),
                              p3 = c(0.25, 0.5, 0.75)),
                labels = list(p1 = c('25', '50', '75'),
                              p2 = c('25', '50', '75'),
                              p3 = c('25', '50', '75'))),
           TernaryCenterGrid(center = center, spacing = 10)
    )
  return(brk_lab)
}

#' Template for Ternary Key
#'
#' Return various types of breaks and labels for ternary color keys.
#'
#' @param legend_surface A data frame with numeric 'id', 'p1', 'p2', 'p3' and
#'                       character column 'rgb'.
#' @param limits A 2 by 3 matrix of lower and upper limits for p1, p2 and p3.
#' @param brklab Breaks and labels as returned by \code{\link{BreaksAndLabels}}.
#' @param show_center Should the center be marked on the legend? (logical)
#' @param center Ternary coordinates of the grey-point.
#' @param lwd A numeric scalar giving the linewidth of the legend surface
#'            polygons.
#'
#' @return A ggtern grob.
#'
#' @importFrom ggplot2 aes_string geom_polygon scale_color_identity
#'   scale_fill_identity element_text
#' @importFrom ggtern ggtern geom_mask theme_classic theme
#'   scale_L_continuous scale_R_continuous scale_T_continuous
#'   geom_Lline geom_Tline geom_Rline
#'
#' @keywords internal
BasicKey <- function(legend_surface, limits, brklab, show_center, center, lwd) {

  key <-
    # basic legend
    ggtern(legend_surface, aes_string(x = 'p1', y = 'p2', z = 'p3')) +
    geom_polygon(aes_string(group = 'id', fill = 'rgb', color = 'rgb'), lwd = lwd) +
    geom_mask() +
    # rgb color input
    scale_color_identity(guide = FALSE) +
    scale_fill_identity(guide = FALSE) +
    # theme
    theme_classic() +
    theme(tern.axis.title.L = element_text(hjust = 0.2, vjust = 1, angle = -60),
          tern.axis.title.R = element_text(hjust = 0.8, vjust = 0.6, angle = 60)) +
    # grid and labels
    list(
      list(
        scale_L_continuous(
          limits = limits[,1],
          breaks = brklab[['breaks']][['p1']],
          labels = brklab[['labels']][['p1']]
        ),
        scale_T_continuous(
          limits = limits[,2],
          breaks = brklab[['breaks']][['p2']],
          labels = brklab[['labels']][['p2']]
        ),
        scale_R_continuous(
          limits = limits[,3],
          breaks = brklab[['breaks']][['p3']],
          labels = brklab[['labels']][['p3']]
        )
      ),
      if (show_center) {
        list(
          geom_Lline(Lintercept = center[1], color = 'black', alpha = 0.5),
          geom_Tline(Tintercept = center[2], color = 'black', alpha = 0.5),
          geom_Rline(Rintercept = center[3], color = 'black', alpha = 0.5)
        )
      }
    )

  return(key)

}

#' Ternary Balance Scheme Legend
#'
#' Plot a ternary balance scheme legend.
#'
#' @inheritParams ColorMapTricolore
#' @param label_as "pct" for percent-share labels or "pct_diff" for
#'   percent-point-difference from center labels.
#' @param show_center Should the center be marked on the legend? (logical)
#' @param limits A 2 by 3 matrix of lower and upper limits for p1, p2 and p3.
#'
#' @return A ggtern grob.
#'
#' @examples
#' tricolore:::ColorKeyTricolore(center = rep(1/3, 3), breaks = 4,
#'                               h_ = 80, c_ = 140, l_ = 80,
#'                               contrast = 0.4, spread = 1,
#'                               label_as = "pct", show_center = FALSE)
#'
#' @keywords internal
ColorKeyTricolore <- function (center, breaks, h_, c_, l_, contrast, spread,
                               label_as, show_center,
                               limits = matrix(0:1, nrow = 2, ncol = 3)) {

  ### Create and colorize legend surface ###

  # don't allow more than 99^2 different colors/regions in the legend
  if (breaks > 99) { breaks = 100 }

  # calculate ternary vertex coordinates and
  # fill color for each sub-triangle
  C <- TernaryMeshCentroids(breaks)
  V <- TernaryMeshVertices(C)
  rgb <- ColorMapTricolore(P = C[,-1], center, breaks = 100, h_, c_, l_,
                           contrast, spread)[['rgb']]

  legend_surface <- data.frame(V, rgb = rep(rgb, 3),
                               row.names = NULL, check.rows = FALSE,
                               check.names = FALSE, stringsAsFactors = FALSE)

  ### Breaks and labels ###

  if (label_as == 'pct' && breaks <= 10) {
    brklab <- BreaksAndLabels(1, center, breaks)
  }
  if (label_as == 'pct' && breaks > 10) {
    brklab <- BreaksAndLabels(2, center, breaks)
  }
  if (label_as == 'pct_diff') {
    brklab <- BreaksAndLabels(3, center, breaks)
  }

  ### Plot key ###

  return(BasicKey(legend_surface, limits, brklab, show_center, center, lwd = 1))

}

#' Sextant Scheme Legend
#'
#' Plot a sextant scheme legend.
#'
#' @inheritParams ColorMapSextant
#' @param label_as "pct" for percent-share labels or "pct_diff" for
#'   percent-point-difference from center labels.
#' @param show_center Should the center be marked on the legend? (logical)
#' @param limits A 2 by 3 matrix of lower and upper limits for p1, p2 and p3.
#'
#' @return A ggtern grob.
#'
#' @examples
#'tricolore:::ColorKeySextant(center = prop.table(runif(3)),
#'                            values = c('#01A0C6', '#B8B3D8', '#F11D8C',
#'                                       '#FFB3B3', '#FFFF00', '#B3DCC3'),
#'                            label_as = 'pct_diff', show_center = TRUE)
#'
#' @keywords internal
ColorKeySextant <- function (center, values, label_as, show_center,
                             limits = matrix(0:1, nrow = 2, ncol = 3)) {

  ### Create and colorize legend surface ###

  # calculate ternary vertex coordinates and
  # fill color for each sub-triangle
  V <- TernarySextantVertices(center)
  rgb <- rep(values, c(5, 4, 5, 4, 5, 4))

  legend_surface <- data.frame(V, rgb = rgb,
                               row.names = NULL, check.rows = FALSE,
                               check.names = FALSE, stringsAsFactors = FALSE)

  ### Breaks and labels ###

  if (label_as == 'pct') {
    brklab <- BreaksAndLabels(2, center)
  }
  if (label_as == 'pct_diff') {
    brklab <- BreaksAndLabels(3, center)
  }

  ### Plot key ###

  return(BasicKey(legend_surface, limits, brklab, show_center, center, lwd = 0))

}

# User functions ----------------------------------------------------------

#' Ternary Balance Color Scale
#'
#' Color-code three-part compositions with a ternary balance color scale and
#' return a color key.
#'
#' @param df Data frame of compositional data.
#' @param p1 Column name for variable in df giving first proportion
#'           of ternary composition (string).
#' @param p2 Column name for variable in df giving second proportion
#'           of ternary composition (string).
#' @param p3 Column name for variable in df giving third proportion
#'           of ternary composition (string).
#' @param center Ternary coordinates of the color scale center.
#'               (default = 1/3,1/3,1/3). NA puts center over the compositional
#'               mean of the data.
#' @param breaks Number of per-axis breaks in the discrete color scale.
#'               An integer >1. Values above 99 imply no discretization.
#' @param hue Primary hue of the first ternary element (0 to 1).
#' @param chroma Maximum possible chroma of mixed colors (0 to 1).
#' @param lightness Lightness of mixed colors (0 to 1).
#' @param contrast Lightness contrast of the color scale (0 to 1).
#' @param spread The spread of the color scale. Choose values > 1 to focus the
#'               color scale on the center.
#' @param legend Should a legend be returned along with the colors? (default=TRUE)
#' @param show_data Should the data be shown on the legend? (default=TRUE)
#' @param show_center Should the center be shown on the legend?
#'   (default=FALSE if center is at c(1/3, 1/3, 1/3), otherwise TRUE)
#' @param label_as "pct" for percent-share labels or "pct_diff" for
#'   percent-point-difference from center labels.
#'   (default='pct' if center is at c(1/3, 1/3, 1/3), otherwise 'pct_diff')
#' @param crop Should the legend be cropped to the data? (default=FALSE)
#' @param input_validation Should the function arguments be validated? (default=TRUE)
#'
#' @return
#' * legend=FALSE: A vector of rgbs hex-codes representing the ternary balance
#'                 scheme colors.
#' * legend=TRUE: A list with elements "rgb" and "key".
#'
#' @examples
#' P <- as.data.frame(prop.table(matrix(runif(3^6), ncol = 3), 1))
#' Tricolore(P, 'V1', 'V2', 'V3')
#'
#' @importFrom ggplot2 aes_string geom_point labs
#'
#' @md
#'
#' @export
Tricolore <- function (df, p1, p2, p3,
                       center = rep(1/3, 3),
                       breaks = ifelse(identical(center, rep(1/3, 3)), 4, Inf),
                       hue = 0.2, chroma = 0.7, lightness = 0.8,
                       contrast = 0.4, spread = 1,
                       legend = TRUE, show_data = TRUE,
                       show_center = ifelse(identical(center, rep(1/3, 3)),
                                            FALSE, TRUE),
                       label_as = ifelse(identical(center, rep(1/3, 3)),
                                         'pct', 'pct_diff'),
                       crop = FALSE, input_validation = TRUE) {

  # validation of main input arguments
  if (input_validation) {
    ValidateMainArguments(df, p1, p2, p3)
    ValidateParametersTricolore(
      list(breaks = breaks, hue = hue, chroma = chroma,
           lightness = lightness, contrast = contrast,
           center = center, spread = spread, legend = legend,
           show_data = show_data, show_center = show_center,
           label_as = label_as, crop = crop)
    )
  }

  # construct 3 column matrix of proportions
  P <- cbind(df[[p1]], df[[p2]], df[[p3]])
  # ensure data is closed
  P <- prop.table(P, 1)

  # center color-scale over data's centre if center==NA
  if ( is.na(center[1]) ) { center = Centre(P) }

  # derive the color mixture
  # the magic numbers rescale the [0,1] color-specification to the
  # cylindrical-coordinates format required by ColorMapTricolore()
  mixture <- ColorMapTricolore(P, center, breaks,
                               hue*360, chroma*200, lightness*100,
                               contrast, spread)

  # if specified, return a legend along with the srgb color mixtures...
  if (legend) {

    # crop legend to to data range if crop==TRUE
    if (crop) {
      limits <- TernaryLimits(P, na.rm = TRUE)
      # else use full range
    } else {
      limits <- matrix(0:1, nrow = 2, ncol = 3)
    }

    key <-
      ColorKeyTricolore(center, breaks, hue*360, chroma*200, lightness*100,
                        contrast, spread, label_as, show_center, limits) +
      list(
        # labels take names from input variables
        labs(x = p1, y = p2, z = p3),
        if (show_data) {
          geom_point(aes_string(x = 'p1', y = 'p2', z = 'p3'),
                     color = 'black', shape = 16, size = 0.5, alpha = 0.5,
                     data = mixture)
        }
      )

    result <- list(rgb = mixture[['rgb']], key = key)
    # ... else just return a vector of hexsrgb codes of the mixed colors
  } else {
    result <- mixture[['rgb']]
  }

  return(result)
}


#' Ternary Sextant Color Scale
#'
#' Color-code three-part compositions with a ternary sextant color scale and
#' return a color key.
#'
#' @param df Data frame of compositional data.
#' @param p1 Column name for variable in df giving first proportion
#'           of ternary composition (string).
#' @param p2 Column name for variable in df giving second proportion
#'           of ternary composition (string).
#' @param p3 Column name for variable in df giving third proportion
#'           of ternary composition (string).
#' @param center Ternary coordinates of the color scale center.
#'               (default = 1/3,1/3,1/3). NA puts center over the compositional
#'               mean of the data.
#' @param values 6 element character vector of rgb-codes.
#' @param legend Should a legend be returned along with the colors? (default=TRUE)
#' @param show_data Should the data be shown on the legend? (default=TRUE)
#' @param show_center Should the center be shown on the legend?
#' (default=FALSE if center is at c(1/3, 1/3, 1/3), otherwise TRUE)
#' @param label_as "pct" for percent-share labels or "pct_diff" for
#'   percent-point-difference from center labels.
#'   (default='pct' if center is at c(1/3, 1/3, 1/3), otherwise 'pct_diff')
#' @param crop Should the legend be cropped to the data? (default=FALSE)
#' @param input_validation Should the function arguments be validated? (default=TRUE)
#'
#' @return
#' * legend=FALSE: A vector of rgbs hex-codes representing the ternary balance
#'                 scheme colors.
#' * legend=TRUE: A list with elements "rgb" and "key".
#'
#' @examples
#' P <- as.data.frame(prop.table(matrix(runif(3^6), ncol = 3), 1))
#' TricoloreSextant(P, 'V1', 'V2', 'V3')
#'
#' @importFrom ggplot2 aes_string geom_point labs
#'
#' @md
#'
#' @export
TricoloreSextant <- function (df, p1, p2, p3,
                              center = rep(1/3, 3),
                              values = c("#FFFF00", "#B3DCC3", "#01A0C6",
                                         "#B8B3D8", "#F11D8C", "#FFB3B3"),
                              legend = TRUE, show_data = TRUE, show_center = TRUE,
                              label_as = ifelse(identical(center, rep(1/3, 3)),
                                                'pct', 'pct_diff'),
                              crop = FALSE, input_validation = TRUE) {

  # validation of main input arguments
  if (input_validation) {
    ValidateMainArguments(df, p1, p2, p3)
    ValidateParametersTricoloreSextant(
      list(values = values,
           center = center,
           legend = legend,
           show_data = show_data,
           show_center = show_center,
           label_as = label_as,
           crop = crop)
    )
  }

  # construct 3 column matrix of proportions
  P <- cbind(df[[p1]], df[[p2]], df[[p3]])
  # ensure data is closed
  P <- prop.table(P, 1)

  # center color-scale over data's centre if center==NA
  if ( is.na(center[1]) ) { center = Centre(P) }


  # derive the color mixture
  mixture <- ColorMapSextant(P, center, values)

  # if specified, return a legend along with the srgb color mixtures...
  if (legend) {

    # crop legend to to data range if crop==TRUE
    if (crop) {
      limits <- TernaryLimits(P, na.rm = TRUE)
      # else use full range
    } else {
      limits <- matrix(0:1, nrow = 2, ncol = 3)
    }

    key <-
      ColorKeySextant(center, values, label_as, show_center, limits) +
      list(
        # labels take names from input variables
        labs(x = p1, y = p2, z = p3),
        if (show_data) {
          geom_point(aes_string(x = 'p1', y = 'p2', z = 'p3'),
                     color = 'black', shape = 16, size = 0.5, alpha = 0.5,
                     data = mixture)
        }
      )

    result <- list(rgb = mixture[['rgb']], key = key)
    # ... else just return a vector of hexsrgb codes of the mixed colors
  } else {
    result <- mixture[['rgb']]
  }

  return(result)

}

#' Interactive Tricolore Demonstration
#'
#' An interactive demonstration of the tricolore color scale inspired by the
#' colorbrewer2.org application. Helps in picking the right color scale for your
#' data.
#'
#' @return Opens a shiny app session.
#'
#' @export
DemoTricolore <- function () {
  app_dir <- system.file('shiny', package = 'tricolore')
  if (app_dir == '') {
    stop("Could not find example directory. Try re-installing 'tricolore'.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = 'normal')
}

# Data --------------------------------------------------------------------

#' Flat Map of European Continent
#'
#' A ggplot object rendering a flat background map of the European continent.
#'
#' @source
#'   Derived from Eurostats European Geodata.
#'   (c) EuroGeographics for the administrative boundaries.
#'   \url{http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/}
'euro_basemap'

#' NUTS-2 Level Geodata and Compositional Data for Europe
#'
#' A simple-features dataframe containing the NUTS-2 level polygons of European
#' regions along with regional compositional data on education and labor-force.
#'
#' @format
#'   A data frame with 312 rows and 9 variables:
#'   \describe{
#'     \item{id}{NUTS-2 code.}
#'     \item{name}{Name of NUTS-2 region.}
#'     \item{ed_0to2}{Share of population with highest attained education "lower secondary or less".}
#'     \item{ed_3to4}{Share of population with highest attained education "upper secondary".}
#'     \item{ed_5to8}{Share of population with highest attained education "tertiary".}
#'     \item{lf_pri}{Share of labor-force in primary sector.}
#'     \item{lf_sec}{Share of labor-force in secondary sector.}
#'     \item{lf_ter}{Share of labor-force in tertiary sector.}
#'     \item{geometry}{Polygon outlines for regions in sf package format.}
#'   }
#'
#' @details
#'   Variables starting with "ed" refer to the relative share of population ages
#'   25 to 64 by educational attainment in the European NUTS-2 regions 2016.
#'
#'   Variables starting with "lf" refer to the relative share of workers by
#'   labor-force sector in the European NUTS-2 regions 2016. The original NACE
#'   (rev. 2) codes have been recoded into the three sectors "primary" (A),
#'   "secondary" (B-E & F) and "tertiary" (all other NACE codes).
#'
#' @source
#'   Derived from Eurostats European Geodata.
#'   (c) EuroGeographics for the administrative boundaries.
#'   \url{http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/}
#'
#'   Education data derived from Eurostats table "edat_lfse_04".
#'
#'   Labor-force data derived from Eurostats table "lfst_r_lfe2en2".
'euro_example'
