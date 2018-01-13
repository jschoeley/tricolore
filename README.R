#' ---
#' title: Tricolore. A flexible color scale for ternary compositions
#' author: Jonas Schöley
#' output:
#'   github_document
#' ---

#+echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE,
                      fig.width = 12, fig.height = 12)

#'## What is 'tricolore'?
#'
#' Tricolore is a flexible color scale for three-part (ternary) compositions
#' allowing you to color code any ternary composition and draw a corresponding
#' color key. Tricolore flexibly adapts to many different visualization
#' challenges via
#'
#'   - discrete and continuous colors
#'   - support for unbalanced compositional data or data with very narrow range
#'     via centering and scaling of the color scale
#'   - hue, chroma and lightness options
#'
#' ![](readme_files/teaser.png)
#'
#'## Install

#+eval=FALSE
devtools::install_github('jschoeley/tricolore')
library(tricolore); DemoTricolore()

#'## Getting Started
#'
#' The `Tricolore()` function takes a data-frame of three-part compositions,
#' color-codes them and returns a list with elements `hexsrgb` and `legend`. The
#' first list element is a vector of rgb codes for the color-coded compositions,
#' the latter element gives a plot of the color key.
#'
#' Here's a minimal example using simulated data.

#+message=FALSE
library(ggtern)
library(tricolore)

# simulate 243 ternary compositions
P <- as.data.frame(prop.table(matrix(runif(3^6), ncol = 3), 1))
# color-code each composition and return a corresponding color key
tric <- Tricolore(P, V1, V2, V3)
# the color-coded compositions
head(tric$hexsrgb)

tric$legend
#' *A ternary color key with the color-coded compositional data visible as points.*
#'
#' You can familiarize yourself with the various options of `tricolore` by
#' running `DemoTricolore()`.
#'
#' ## Case study: European labor force composition.
#'
#' We are interested in the regional distribution of labor force by sector in
#' the European Union. The `eu_sectors` data contains the share of workers in
#' the three sectors for the year 2016 by NUTS-2 region.

head(eu_sectors)

#' In order to prepare a map I've prepared a data frame with the outlines of
#' the European NUTS-2 regions and neighboring countries, `eushp_nuts2`

head(eushp_nuts2)

#' Using tricolore, I color-code each regions labor force composition and merge
#' the resulting vector of hexsrgb color codes with the map data.

# generate colors based on compositions in `eu_sectors`, default options
tricol <- Tricolore(eu_sectors, primary, secondary, tertiary)

# merge vector of colors with with map data
eu_sectors$srgb <- tricol$hexsrgb
map_data <- dplyr::left_join(eushp_nuts2, eu_sectors, by = c('id' = 'nuts2'))

#' I use `ggplot2` to plot a map of Europe with each NUTS-2 region shaded
#' according to its corresponding hexsrgb values (`scale_fill_identity()`). I
#' merge the color-key returned by `tricolore` with the map using the
#' `annotation_custom()` function.

europe_map +
  geom_polygon(aes(long, lat, group = group, fill = srgb),
               data = map_data) +
  scale_fill_identity() +
  annotation_custom(ggplotGrob(tricol$legend),
                    xmin = -7e5, xmax = 73e5, ymin = 42e5, ymax = 55e5)
#' *Labor force composition in EU regions. Default color scale options. Data by eurostat.*

#' Europe's labor force predominantly works in the tertiary sector, as seen by a
#' map colored in various shades of blue. Reddish and greenish hues in eastern
#' Europe indicate a higher share of primary and secondary labor force
#' respectively.
#'
#' I'm not happy with the basic colors used to represent primary, secondary, and
#' tertiary sectors. I think its much more natural to encode the primary
#' (agricultural) sector in green, the secondary (industrial) sector in blue and
#' the tertiary (services) sector in red. This is easily achieved by changing
#' *hue* parameter of the color scale.

tricol <- Tricolore(eu_sectors, primary, secondary, tertiary, hue = 0.33)

#' *In the examples to follow I omit the whole mapping code. It is identical for
#' all examples.*

#+echo=FALSE
eu_sectors$srgb <- tricol$hexsrgb
map_data <- dplyr::left_join(eushp_nuts2, eu_sectors, by = c('id' = 'nuts2'))
europe_map +
  geom_polygon(aes(long, lat, group = group, fill = srgb),
               data = map_data) +
  scale_fill_identity() +
  annotation_custom(ggplotGrob(tricol$legend),
                    xmin = -7e5, xmax = 73e5, ymin = 42e5, ymax = 55e5)

#' *Labor force composition in EU regions. Hue parameter set to 0.33. Data by eurostat.*
#'
#' Up until now I used continuous colors to show the regional labor force
#' composition. A discrete color scale introduces sharp contours which sometimes
#' pronounce interesting patterns in the data. The `k` parameter determines the
#' number of colors for the color scale. A value of 3 gives a discrete scale of
#' 3^2=9 colors. The discrete scale pronounces the east-west divide in
#' labor force composition.

tricol <- Tricolore(eu_sectors, primary, secondary, tertiary, hue = 0.33, k = 3)

#+echo=FALSE
eu_sectors$srgb <- tricol$hexsrgb
map_data <- dplyr::left_join(eushp_nuts2, eu_sectors, by = c('id' = 'nuts2'))
europe_map +
  geom_polygon(aes(long, lat, group = group, fill = srgb),
               data = map_data) +
  scale_fill_identity() +
  annotation_custom(ggplotGrob(tricol$legend),
                    xmin = -7e5, xmax = 73e5, ymin = 42e5, ymax = 55e5)
#' *Labor force composition in EU regions. Discrete scale with 9 colors. Data by
#' eurostat.*
#'
#' A technique I've adopted from compositional data analysis is *ternary
#' centering*. Centering shifts the center of the color scale (the greypoint) to
#' the center of the data and thereby shows deviations from the average
#' composition. It's the ternary equivalent to a divergent color-scale with the
#' average value at the midpoint.
#'
#' Centering the color scale over the labor-force composition of the average
#' European NUTS-2 region shows various patterns of deviations from the average.
#' Metropolitan regions (Hamburg, Stockholm, Paris, Madrid) have a higher than
#' average share of tertiary workers. Large parts of France are quite grey,
#' indicating a labor-force composition close to the average, while Eastern
#' Europe, the south of Spain and Italy have a higher than average share of
#' workers active in the primary sector.

tricol <- Tricolore(eu_sectors, primary, secondary, tertiary,
                    hue = 0.33, center = NA)

#+echo=FALSE
eu_sectors$srgb <- tricol$hexsrgb
map_data <- dplyr::left_join(eushp_nuts2, eu_sectors, by = c('id' = 'nuts2'))
europe_map +
  geom_polygon(aes(long, lat, group = group, fill = srgb),
               data = map_data) +
  scale_fill_identity() +
  annotation_custom(ggplotGrob(tricol$legend),
                    xmin = -7e5, xmax = 73e5, ymin = 42e5, ymax = 55e5)
