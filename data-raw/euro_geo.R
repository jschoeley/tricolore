#'---
#'title: Adapt the files Ilya sent me to work with tricolore
#'author: Jonas Schöley
#'date: 2018-02-13
#'---

library(tidyverse)

load('data-raw/tricolore-nuts2-fort.RData')

euro_geo_nuts2 <-
  fort_nuts2 %>%
  mutate(piece = as.integer(piece),
         id = as.character(id),
         group = as.character(group))

save(euro_geo_nuts2, file = 'data-raw/euro_geo_nuts2.RData', compress = 'xz')

euro_region_geo <-
  fort_neighbours %>%
  mutate(piece = as.integer(piece),
         id = as.character(id),
         group = as.character(group))

save(euro_region_geo, file = 'data-raw/euro_region_geo.RData', compress = 'xz')
