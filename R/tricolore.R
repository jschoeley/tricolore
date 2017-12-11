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
  # The geometric mean can't really deal with elements equal to 0.
  # This option recodes 0's as NA instead, thereby ignoring 0's
  # in the calculation of the mean.
  if (zero.rm) { x = x[x!=0] }
  return(exp(mean(log(x), na.rm = na.rm)))
}

#' Center Composition
#'
#' Center a compositional data set around its mean.
#'
#' @param P n by m matrix of compositions {p1, ..., pm}_i for
#'          i=1,...,n.
#'
#' @return n by m matrix of centered compositions.
#'
#' @examples
#' P <- prop.table(matrix(runif(300), 100), margin = 1)
#' tricolore:::Centre(P)
#'
#' @references Von Eynatten, H., Pawlowsky-Glahn, V., & Egozcue, J. J. (2002).
#' Understanding perturbation on the simplex: A simple method to better
#' visualize and interpret compositional data in ternary diagrams.
#' Mathematical Geology, 34(3), 249-257.
#'
#' @keywords internal
Centre <- function (P) {
  # calculate the geometric mean for each element of the composition
  g = apply(P, MARGIN = 2, FUN = GeometricMean)
  # the closed vector of geometric means is the mean (centroid)
  # of the compositional data set
  centre = g/sum(g)
  # perturbating the original composition by the inverse
  # centroid centers the composition around the centroid
  return(prop.table(t(t(P)*(1/centre)), margin = 1))
}

# Discrete Ternary Geometry -----------------------------------------------

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
#' @references S. H. Derakhshan and C. V. Deutsch (2009): A Color Scale for
#'   Ternary Mixtures.
#'
#' @examples
#' tricolore:::GetCentroids(1)
#' tricolore:::GetCentroids(2)
#' tricolore:::GetCentroids(3)
#'
#' @keywords internal
GetCentroids <- function (k) {
  # total number of centroids and centroid id
  K = k^2; id = 1:K

  # centroid coordinates as function of K and id
  g = floor(sqrt(K-id)); gsq = g^2
  c1 = (((-K + id + g*(g+2) + 1) %% 2) - 3*gsq - 3*id + 3*K + 1) / (6*k)
  c2 = -(((-K + gsq + id + 2*g + 1) %% 2) + 3*g - 3*k + 1) / (3*k)
  c3 = (((-K + gsq + id + 2*g + 1) %% 2) + 3*gsq + 6*g + 3*id - 3*K + 1) / (6*k)

  return(cbind(id = id, p1 = c1, p2 = c2, p3 = c3))
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
#' @references https://en.wikipedia.org/wiki/Barycentric_coordinate_system#Distance_between_points
#'
#' @examples
#' p <- c(0.5, 0.2, 0.3)
#' C <- prop.table(matrix(runif(3*10), ncol = 3), 1)
#' tricolore:::GetTernaryDistance(p, C)
GetTernaryDistance <- function(p, C) {
  Q = t(p-t(C))
  return(-Q[,2]*Q[,3]-Q[,3]*Q[,1]-Q[,1]*Q[,2])
}

# from nnet::which.is.max()
GetMaxIndex <- function (x) {
  y = seq_along(x)[x == max(x)]
  if (length(y) > 1L) sample(y, 1L) else y
}

#' For Ternary Coordinates P Return the Nearest Coordinate in Set C
#'
#' @param P,C n by 3 matrix of ternary coordinates {p1, p2, p3}_i for
#'            i=1,...,n. n may be different for P and C.
#'
#' @return A matrix of ternary coordinates in C.
#'
#' @examples
#' P <- prop.table(matrix(runif(9), ncol = 3), 1)
#' C <- tricolore:::GetCentroids(2)[,-1]
#' tricolore:::GetNearest(P, C)
GetNearest <- function (P, C, index = FALSE) {
  id = apply(P, 1, function (x) GetMaxIndex(-GetTernaryDistance(x, C)))
  return(C[id,])
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
#' C <- tricolore:::GetCentroids(k)
#' tricolore:::GetVertices(C)
#'
#' @references S. H. Derakhshan and C. V. Deutsch (2009): A Color Scale for
#'   Ternary Mixtures.
#'
#' @keywords internal
GetVertices <- function (C) {
  k = sqrt(nrow(C))
  j = k - floor(sqrt(k^2-C[,1]))
  i = C[,1] - (j-1)*(2*k-j+1)
  term1 = ((-1)^(i %% 2) * 2) / (3*k)
  term2 = ((-1)^(i %% 2)) / (3*k)

  v1 = cbind(C[,2] - term1, C[,3] + term2, C[,4] + term2)
  v2 = cbind(C[,2] + term2, C[,3] - term1, C[,4] + term2)
  v3 = cbind(C[,2] + term2, C[,3] + term2, C[,4] - term1)

  V = cbind(C[,1], rep(1:3, each = nrow(C)), rbind(v1, v2, v3))
  colnames(V) = c('id', 'vertex', 'p1', 'p2', 'p3')

  return(V)
}

# Ternary Color Mixture ---------------------------------------------------

#' RGB Mixture of Ternary Composition
#'
#' Return the ternary balance scheme colors for a matrix of ternary compositions.
#'
#' @param P n by 3 matrix of ternary compositions {p1, p2, p3}_i for
#'          i=1, ..., n.
#' @param k Number of breaks in the discrete color scale. An integer >0.
#'          Values above 99 imply no discretization.
#' @param h_ Primary hue of the first ternary element in angular degrees [0, 360].
#' @param c_ Maximum possible chroma of mixed colors [0, 200].
#' @param l_ Lightness of mixed colours [0, 100].
#' @param center Should the composition be centered? (default=TRUE)
#'
#' @return A vector of rgbs hex-codes representing the mixed colors.
#'
#' @examples
#' P <- prop.table(matrix(runif(9), ncol = 3), 1)
#' tricolore:::GetMixture(P, k = 5, h_ = 80, c_ = 170, l_ = 80, contrast = 0.6,
#'                        center = TRUE, color_space = 'hcl')
#'
#' @importFrom grDevices hcl hsv
#' @importFrom scales rescale
#'
#' @keywords internal
GetMixture <- function (P, k, h_, c_, l_, contrast, center, color_space) {

  # generate primary colours starting with a hue value in [0, 360) and then
  # picking two equidistant points on the circumference of the colour wheel.
  # input hue in degrees, all further calculations in radians.
  phi = (h_*0.0174 + c(0, 2.09, 4.19)) %% 6.28

  # optional centering of the composition
  # we keep a copy called "Plgnd" to plot the
  # centered data points in the legend, whereas
  # "P" may be discretized later on
  if (center) {
    P = Plgnd = Centre(prop.table(P, margin = 1))
  } else {
    P = Plgnd = prop.table(P, margin = 1)
  }

  # discretize composition
  if (k < 100) {
    P = GetNearest(P, GetCentroids(k)[,-1])
  }

  # calculate the chroma matrix C by scaling the row proportions
  # of the input matrix P by the maximum chroma parameter.
  C = P*c_

  # the complex matrix Z represents each case (i) and group (j=1,2,3) specific
  # color in complex polar form with hue as angle and chroma as radius.
  Z = matrix(complex(argument = phi, modulus = c(t(C))), ncol = 3, byrow = TRUE)

  # adding up the rows gives the CIE-Lab (cartesian) coordinates
  # of the convex color mixture in complex form.
  z = rowSums(Z)
  # convert the cartesian CIE-Lab coordinates to polar CIE-Luv coordinates
  # and add lightness level.
  M = cbind(h = (Arg(z)*57.3)%%360, c = Mod(z), l = l_)

  # boost lightness and chroma contrast of balanced to unbalanced mixtures
  cfactor = rescale(M[,2], from = c(0, c_), to = c(1-contrast, 1))
  M[,3] = cfactor*M[,3]
  M[,2] = cfactor*M[,2]

  # convert the complex representation of the color mixture to
  # hex-srgb representation via the hcl (CIE-Luv) color space
  # or the hsv (polar RGB) color space
  if (color_space == 'hcl') {
    # expects h = [0, 360], c = [0, 200], l = c[0, 100]
    hexsrgb = hcl(h = M[,1], c = M[,2], l = M[,3],
                  alpha = 1, fixup = TRUE)
  }
  if (color_space == 'hsv') {
    # expects h = [0, 1], c = s = [0, 1], l = v = c[0, 1]
    hexsrgb = hsv(h = M[,1]/360, s = M[,2]/200, v = M[,3]/100,
                  alpha = 1)
  }

  # (centered) compositions, hcl values of mixtures and hexsrgb code
  result = data.frame(Plgnd, M[,1], M[,2], M[,3], hexsrgb,
                      row.names = NULL, check.rows = FALSE,
                      check.names = FALSE, stringsAsFactors = FALSE)
  colnames(result) = c('p1', 'p2', 'p3', 'h', 'c', 'l', 'hexsrgb')
  return(result)
}

# User functions ----------------------------------------------------------

#' Ternary Balance Scheme Colors
#'
#' Return the ternary balance scheme colors for ternary compositions.
#'
#' @param df Data frame.
#' @param p1 Unquoted column name for variable in df giving first proportion
#'           of ternary composition.
#' @param p2 Unquoted column name for variable in df giving second proportion
#'           of ternary composition.
#' @param p3 Unquoted column name for variable in df giving third proportion
#'           of ternary composition.
#' @param hue Primary hue of the first ternary element [0, 1].
#' @param chroma Maximum possible chroma of mixed colors [0, 1].
#' @param lightness Lightness of mixed colours [0, 1].
#' @param center Should the composition be centered? (default=TRUE)
#' @param legend Should a legend be returned along with the colors? (default=FALSE)
#'
#' @return legend=FALSE: A vector of rgbs hex-codes representing the ternary
#'         balance scheme colors. legend=TRUE: A list with elements "hexsrgb"
#'         and "legend".
#'
#' @examples
#' P <- as.data.frame(prop.table(matrix(runif(3^6), ncol = 3), 1))
#' MixColor(P, V1, V2, V3, legend = TRUE)
#'
#' @importFrom rlang enquo quo_text
#' @importFrom ggplot2 aes_string geom_point labs
#'
#' @export
MixColor <- function (df, p1, p2, p3,
                      k = Inf, hue = 0.3, chroma = 0.8, lightness = 0.8,
                      contrast = 0.6, center = FALSE, legend = FALSE,
                      color_space = 'hcl') {

  # construct 3 column matrix of proportions
  p1 = enquo(p1); p2 = enquo(p2); p3 = enquo(p3)
  P = cbind(df[[quo_text(p1)]], df[[quo_text(p2)]], df[[quo_text(p3)]])

  # derive the color mixture
  # the magic numbers rescale the [0,1] color-specification to the
  # cylindrical-coordinates format required by GetMixture()
  mixture = GetMixture(P, k, hue*360, chroma*200, lightness*100, contrast,
                       center, color_space)

  # if specified, return a legend along with the srgb color mixtures...
  if (legend) {
    lgnd =
      PlotLegend(k, hue, chroma, lightness, contrast, center, color_space) +
      geom_point(aes_string(x = 'p1', y = 'p2', z = 'p3'),
                 data = mixture, shape = 1, size = 1,
                 color = 'black', alpha = 0.7) +
      labs(x = quo_text(p1), y = quo_text(p2), z = quo_text(p3))

    result = list(hexsrgb = mixture[['hexsrgb']], legend = lgnd)
  # ... else just return a vector of hexsrgb codes of the mixed colors
  } else {
    result = mixture[['hexsrgb']]
  }

  return(result)
}

#' Ternary Balance Scheme Legend
#'
#' Plot a ternary balance scheme legend.
#'
#' @param k Number of breaks on each axis.
#' @inheritParams MixColor
#'
#' @importFrom ggplot2 aes_string geom_polygon scale_color_identity
#'   scale_fill_identity element_text element_blank
#' @importFrom ggtern ggtern geom_mask theme_classic theme annotate
#'   scale_L_continuous scale_R_continuous scale_T_continuous
#'
#' @export
PlotLegend <- function (k, hue = 0.3, chroma = 0.8, lightness = 0.8,
                        contrast = 0.6, center = FALSE, color_space = 'hcl') {

  # don't allow more than 100^2 different colors in the legend
  if (k > 99) { k = 100 }

  # partition the ternary legend into k^2 equilateral sub-triangles
  # calculate barycentric vertex coordinates and fill color for each
  # sub-triangle.
  C = GetCentroids(k)
  V = GetVertices(C)
  rgbs = GetMixture(P = C[,-1], k = Inf, hue*360, chroma*200,
                    lightness*100, contrast, center, color_space)[['hexsrgb']]
  sub_triangles = data.frame(V, rgb = rep(rgbs, 3))

  # plot the legend
  breaks = seq(0, 1, length.out = k+1); labs = round(breaks*100, 1)
  legend <-
    # basic legend
    ggtern(sub_triangles, aes_string(x = 'p1', y = 'p2', z = 'p3')) +
    geom_polygon(aes_string(group = 'id', fill = 'rgb', color = 'rgb'),
                 lwd = 1) +
    geom_mask() +
    # rgb color input
    scale_color_identity(guide = FALSE) +
    scale_fill_identity(guide = FALSE) +
    # theme
    theme_classic() +
    theme(tern.axis.title.L = element_text(hjust = 0.2, vjust = 1, angle = -60),
          tern.axis.title.R = element_text(hjust = 0.8, vjust = 0.6, angle = 60)) +
    # axis labels
    list(
      if (k <= 10) {
        list(
          scale_L_continuous(breaks = breaks, labels = labs),
          scale_R_continuous(breaks = breaks, labels = labs),
          scale_T_continuous(breaks = breaks, labels = labs)
        )
      },
      if (center) {
        list(
          annotate(geom = 'segment', color = 'black', alpha = 0.7,
                   x = c(1,0,0), xend = c(0,0.5,0.5),
                   y = c(0,1,0), yend = c(0.5,0,0.5),
                   z = c(0,0,1), zend = c(0.5,0.5,0)),
          theme(axis.text = element_blank(),
                axis.ticks = element_blank())
        )
      }
    )

  return(legend)
}

# Shiny app ---------------------------------------------------------------

#' @export
DemoTricolore <- function() {
  app_dir <- system.file('shiny', package = 'tricolore')
  if (app_dir == '') {
    stop("Could not find example directory. Try re-installing 'tricolore'.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = 'normal')
}

# Data --------------------------------------------------------------------


