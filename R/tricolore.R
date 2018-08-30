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

#' Validate Parameters
#'
#' Validate parameters of tricolore function.
#'
#' @param pars List of parameters.
#'
#' @importFrom assertthat assert_that is.number is.scalar is.flag
#'
#' @keywords internal
ValidateParameters <- function (pars) {

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
    # center either NA or three element numeric vector
    # with sum 1 and elements > 0
    assert_that((is.scalar(center) && is.na(center)) ||
                  (length(center) == 3L &&
                     all(is.numeric(center)) &&
                     sum(center) == 1 &&
                     all(center != 0)),
                msg = 'center must be either NA or a three element numeric vector with sum == 1 and all element > 0.')
    # spread is positive numeric scalar
    assert_that(is.number(spread), spread > 0, is.finite(spread))
    # flags
    assert_that(is.flag(legend), is.flag(show_data),
                is.flag(show_center))
    # character options
    assert_that(is.scalar(label_as),
                is.character(label_as),
                label_as %in% c('pct', 'pct_diff'),
                msg = 'label_as must be either "pct" or "pct_diff".')
  })

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
#' @return n by m matrix of powered compositions.
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
#' @return A matrix of barycentric centroid coordinates of regions id=1,...,k^2.
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
#' @return Index, vertex id and barycentric vertex coordinates for each of the
#'         k^2 sub-triangles.
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
#' @return Vector of distances between coordinate p and all coordinates in C.
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

# Ternary Color Scale -----------------------------------------------------

#' RGB Mixture of Ternary Composition
#'
#' Return the ternary balance scheme colors for a matrix of ternary compositions.
#'
#' @param P n by 3 matrix of ternary compositions {p1, p2, p3}_i for
#'          i=1, ..., n.
#' @param breaks Number of breaks in the discrete color scale. An integer >1.
#'               Values above 99 imply no discretization.
#' @param h_ Primary hue of the first ternary element in angular degrees [0, 360].
#' @param c_ Maximum possible chroma of mixed colors [0, 200].
#' @param l_ Lightness of mixed colors [0, 100].
#' @param contrast Lightness contrast of the color scale [0, 1).
#' @param center Ternary coordinates of the grey-point.
#' @param spread Spread of the color scale around center > 0.
#'
#' @return An n row data frame giving, for each row of the input P, the input
#' proportions (p1, p2, p3), parameters of the color mixture (h, c, l) and the
#' hexsrgb string of the mixed colors.
#'
#' @examples
#' P <- prop.table(matrix(runif(9), ncol = 3), 1)
#' tricolore:::ColorMap(P, breaks = 5, h_ = 80, c_ = 170, l_ = 80,
#'                      contrast = 0.6, center = rep(1/3, 3), spread = 1)
#'
#' @importFrom grDevices hcl hsv
#'
#' @keywords internal
ColorMap <- function (P, breaks, h_, c_, l_, contrast, center, spread) {

  # generate primary colors starting with a hue value in [0, 360) and then
  # picking two equidistant points on the circumference of the color wheel.
  # input hue in degrees, all further calculations in radians.
  phi <- (h_*0.0174 + c(0, 2.09, 4.19)) %% 6.28

  # closing
  P <- Plgnd <- prop.table(P, margin = 1)
  # centering
  P <- Pertube(P, 1/center)
  # scaling
  P <- PowerScale(P, spread)

  # discretize composition
  if (breaks < 100) {
    # don't discretize if breaks > 99
    # to avoid expensive calculations which don't make
    # much of a difference in output
    P <- TernaryNearest(P, TernaryMeshCentroids(breaks)[,-1])
  }

  # calculate the chroma matrix C by scaling the row proportions
  # of the input matrix P by the maximum chroma parameter.
  C <- P*c_

  # the complex matrix Z represents each case (i) and group (j=1,2,3) specific
  # color in complex polar form with hue as angle and chroma as radius.
  Z <- matrix(complex(argument = phi, modulus = c(t(C))), ncol = 3, byrow = TRUE)

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
  hexsrgb <- hcl(h = M[,1], c = M[,2], l = M[,3],
                 alpha = 1, fixup = TRUE)

  # non-transformed compositions, hcl values of mixtures and hexsrgb code
  result <- data.frame(Plgnd, M[,1], M[,2], M[,3], hexsrgb,
                       row.names = NULL, check.rows = FALSE,
                       check.names = FALSE, stringsAsFactors = FALSE)
  colnames(result) <- c('p1', 'p2', 'p3', 'h', 'c', 'l', 'hexsrgb')
  return(result)
}

#' Ternary Balance Scheme Legend
#'
#' Plot a ternary balance scheme legend.
#'
#' @inheritParams ColorMap
#' @param label_as "pct" for percent-share labels or "pct_diff" for
#'   percent-point-difference from center labels.
#'
#' @examples
#' tricolore:::ColorKey(breaks = 5, h_ = 0, c_ = 140, l_ = 70,
#'                      contrast = 0.5, center = rep(1/3, 3),
#'                      spread = 1, label_as = "pct")
#'
#' @importFrom ggplot2 aes_string geom_polygon scale_color_identity
#'   scale_fill_identity element_text element_blank
#' @importFrom ggtern ggtern geom_mask theme_classic theme annotate
#'   scale_L_continuous scale_R_continuous scale_T_continuous
#'
#' @keywords internal
ColorKey <- function (breaks, h_, c_, l_, contrast, center, spread, label_as) {

  # don't allow more than 99^2 different colors/regions in the legend
  if (breaks > 99) { breaks = 100 }

  # partition the ternary legend into k^2 equilateral sub-triangles
  # calculate ternary vertex coordinates and fill color for each sub-triangle.
  C <- TernaryMeshCentroids(breaks)
  V <- TernaryMeshVertices(C)
  rgbs <- ColorMap(P = C[,-1], breaks = 100, h_, c_, l_,
                   contrast, center, spread)[['hexsrgb']]
  legend_surface <- data.frame(V, rgb = rep(rgbs, 3))

  # legend grid (percent share or percent point difference from center)
  pct_grid <- list(breaks = seq(0, 1, length.out = breaks+1),
                   labels = round(seq(0, 1, length.out = breaks+1)*100, 1))
  pct_diff_grid <- TernaryCenterGrid(center = center, spacing = 10)

  # plot legend
  legend <-
    # basic legend
    ggtern(legend_surface, aes_string(x = 'p1', y = 'p2', z = 'p3')) +
    geom_polygon(aes_string(group = 'id', fill = 'rgb', color = 'rgb'), lwd = 1) +
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
      # proportion labels
      if (label_as == 'pct' && breaks <= 10) {
        list(
          # dynamic axis labels, for breaks < 10 label
          # at the border of two regions
          scale_L_continuous(breaks = pct_grid[['breaks']],
                             labels = pct_grid[['labels']]),
          scale_T_continuous(breaks = pct_grid[['breaks']],
                             labels = pct_grid[['labels']]),
          scale_R_continuous(breaks = pct_grid[['breaks']],
                             labels = pct_grid[['labels']])
        )
      },
      if (label_as == 'pct_diff') {
        list(
          scale_L_continuous(breaks = pct_diff_grid[['breaks']][['p1']],
                             labels = pct_diff_grid[['labels']][['p1']]),
          scale_T_continuous(breaks = pct_diff_grid[['breaks']][['p2']],
                             labels = pct_diff_grid[['labels']][['p2']]),
          scale_R_continuous(breaks = pct_diff_grid[['breaks']][['p3']],
                             labels = pct_diff_grid[['labels']][['p3']])
        )
      }
    )

  return(legend)
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
#' @param breaks Number of per-axis breaks in the discrete color scale.
#'               An integer >1. Values above 99 imply no discretization.
#' @param hue Primary hue of the first ternary element (0 to 1).
#' @param chroma Maximum possible chroma of mixed colors (0 to 1).
#' @param lightness Lightness of mixed colors (0 to 1).
#' @param contrast Lightness contrast of the color scale (0 to 1).
#' @param center Ternary coordinates of the color scale center.
#'               (default = 1/3,1/3,1/3). NA puts center over the compositional
#'               mean of the data.
#' @param spread The spread of the color scale. Choose values > 1 to focus the
#'               color scale on the center.
#' @param legend Should a legend be returned along with the colors? (default=TRUE)
#' @param show_data Should the data be shown on the legend? (default=TRUE)
#' @param show_center Should the center be shown on the legend? (default=TRUE)
#' @param label_as "pct" for percent-share labels or "pct_diff" for
#'   percent-point-difference from center labels. (default="pct")
#' @param input_validation Should the function arguments be validated? (default=TRUE)
#'
#' @return
#' * legend=FALSE: A vector of rgbs hex-codes representing the ternary balance
#'                 scheme colors.
#' * legend=TRUE: A list with elements "hexsrgb" and "legend".
#'
#' @examples
#' P <- as.data.frame(prop.table(matrix(runif(3^6), ncol = 3), 1))
#' Tricolore(P, 'V1', 'V2', 'V3')
#'
#' @importFrom ggplot2 aes_string geom_point labs
#' @importFrom ggtern geom_Lline geom_Tline geom_Rline
#' @importFrom assertthat assert_that is.string
#'
#' @md
#'
#' @export
Tricolore <- function (df, p1, p2, p3,
                       breaks = 100, hue = 0, chroma = 0.8, lightness = 0.7,
                       contrast = 0.4, center = rep(1/3, 3), spread = 1,
                       legend = TRUE, show_data = TRUE, show_center = TRUE,
                       label_as = 'pct', input_validation = TRUE) {

  # validation of main input arguments
  if (input_validation) {
    ValidateMainArguments(df, p1, p2, p3)
    ValidateParameters(list(breaks = breaks, hue = hue, chroma = chroma,
                            lightness = lightness, contrast = contrast,
                            center = center, spread = spread, legend = legend,
                            show_data = show_data, show_center = show_center,
                            label_as = label_as))
    }

  # construct 3 column matrix of proportions
  P <- cbind(df[[p1]], df[[p2]], df[[p3]])
  # ensure data is closed
  P <- prop.table(P, 1)

  # use continuous colors for off-center color scales
  if (!identical(center, rep(1/3, 3))) {
    if (breaks < 100) {
      message('Discrete colors not supported for center != c(1/3, 1/3, 1/3). Reverting to continuous.')
    }
    breaks = 100
  }
  # center color-scale over data's centre if center==NA
  if ( is.na(center[1]) ) { center = Centre(P) }

  # derive the color mixture
  # the magic numbers rescale the [0,1] color-specification to the
  # cylindrical-coordinates format required by ColorMap()
  mixture <- ColorMap(P, breaks, hue*360, chroma*200, lightness*100,
                      contrast, center, spread)

  # if specified, return a legend along with the srgb color mixtures...
  if (legend) {
    lgnd <-
      ColorKey(breaks, hue*360, chroma*200, lightness*100,
               contrast, center, spread, label_as) +
      list(
        # labels take names from input variables
        labs(x = p1, y = p2, z = p3),
        if (show_center) {
          list(
            geom_Lline(Lintercept = center[1], color = 'black', alpha = 0.5),
            geom_Tline(Tintercept = center[2], color = 'black', alpha = 0.5),
            geom_Rline(Rintercept = center[3], color = 'black', alpha = 0.5)
          )
        },
        if (show_data) {
          geom_point(aes_string(x = 'p1', y = 'p2', z = 'p3'),
                     color = 'black', shape = 16, size = 0.5, alpha = 0.5,
                     data = mixture)
        }
      )

    result <- list(hexsrgb = mixture[['hexsrgb']], legend = lgnd)
  # ... else just return a vector of hexsrgb codes of the mixed colors
  } else {
    result <- mixture[['hexsrgb']]
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
