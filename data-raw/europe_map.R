#'---
#' title: Prepare Europe base-map
#' author: Ilya Kashinsky, Jonas Schöley
#' date: 2017-11-09
#'---

library(tidyverse)

# europe countries shapefile
# prepared by Ilya Kashnitsky:
# https://ikashnitsky.github.io/2017/subplots-in-maps/
load('./data-raw/eushp_cntrs.RData')

# draw a basemap of Europe
europe_map <-
  ggplot(eushp_cntrs, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = NA, fill = 'grey90') +
  coord_equal(ylim = c(1350000, 5450000),
              xlim = c(2500000, 6600000),
              expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(fill = NA, color = 'grey90', size = 1))

save(europe_map, file = './data-raw/europe_map.RData')
