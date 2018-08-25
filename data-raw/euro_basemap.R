#'---
#' title: A map of European neighbours
#' author: Ilya Kashnitsky, Jonas Schöley
#' date: 2018-08-25
#'---

library(tidyverse)

# European neighbour countries geodata
# prepared by Ilya Kashnitsky:
# https://ikashnitsky.github.io/2017/subplots-in-maps/
load('./data-raw/euro_region_geo.RData')

# draw a basemap of Europe
euro_basemap <-
  ggplot(euro_region_geo, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = NA, fill = 'grey90') +
  coord_equal(ylim = c(1350000, 5450000),
              xlim = c(2500000, 7500000),
              expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(fill = NA, color = 'grey90', size = 1))

save(euro_basemap, file = './data-raw/euro_basemap.RData', compress = 'xz')
