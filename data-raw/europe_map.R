#'---
#' title: Prepare Europe base-map
#' author: Ilya Kashinsky, Jonas Schöley
#' date: 2017-11-09
#'---

library(tidyverse)

# EU nuts 2 shapefiles
load('../priv/data_preparation/eu_nuts2.RData')

# draw a basemap of Europe
europe_map <-
  ggplot(eu_nuts2$country_regions, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = NA, fill = 'grey90') +
  coord_equal(ylim = c(1350000, 5450000), xlim = c(2500000, 6600000)) +
  scale_x_continuous(expand = c(0 , 0)) +
  scale_y_continuous(expand = c(0 , 0)) +
  scale_fill_identity() +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(panel.border = element_rect(fill = NA, color = 'grey90', size = 1))

save(europe_map, file = '../priv/data_preparation/europe_map.RData')
