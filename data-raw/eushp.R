#'---
#' title: Prepare EU map shape files
#' author: Ilya Kashinsky, Jonas Schöley
#' date: 2017-11-08
#'---

library(tidyverse)
library(rgeos)

# load shape files
# shape file prepared by Ilya Kashnitsky:
# https://ikashnitsky.github.io/2017/subplots-in-maps/
load(url("https://ikashnitsky.github.io/doc/misc/map-subplots/spatial-27-261.RData"))

# fortify spatial objects

# EU NUTS-2 regions
eushp_nuts2 <- fortify(Sn2, region = 'id') %>% mutate_if(is.factor, as.character)
# EU + EU neighbour country regions
eushp_cntrs <- fortify(Sneighbors) %>% mutate_if(is.factor, as.character)

save(eushp_nuts2, file = './data-raw/eushp_nuts2.RData')
save(eushp_cntrs, file = './data-raw/eushp_cntrs.RData')
