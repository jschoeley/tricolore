#'---
#' title: A flat and simplified map of Europe
#' author: Jonas Schöley
#' date: 2018-08-28
#'---

library(tidyverse)
library(sf)
library(rnaturalearth)

eura_sf <-
  # download geospatial data for European, Asian and African countries
  ne_countries(continent = c('europe', 'asia', 'africa'), returnclass = 'sf',
               scale = 50) %>%
  # project to crs 3035
  st_transform(crs = 3035) %>%
  # merge into single polygon
  st_union(by_feature = FALSE) %>%
  st_crop(xmin = 25e5, xmax = 75e5, ymin = 13.5e5, ymax = 54.5e5)

# draw a basemap of Europe
euro_basemap <-
  ggplot(eura_sf) +
  geom_sf(color = NA, fill = 'grey90') +
  coord_sf(expand = FALSE, datum = NA) +
  theme_void() +
  theme(panel.border = element_rect(fill = NA, color = 'grey90', linewidth = 1))

save(euro_basemap, file = './data-raw/euro_basemap.RData', compress = 'xz')
