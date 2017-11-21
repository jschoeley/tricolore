#'---
#' title: Prepare EU map shape files
#' author: Ilya Kashinsky, Jonas Schöley
#' date: 2017-11-08
#'---

library(tidyverse)
library(rgeos)

# load shape files
# shape file prepared by Ilya Kashnitsky (https://ikashnitsky.github.io/)
load(url("https://ikashnitsky.github.io/doc/misc/map-subplots/spatial-27-261.RData"))

# fortify spatial objects

# borders among EU member states
eushp_borders <- fortify(Sborders)
# EU NUTS-2 regions
eushp_nuts2 <- fortify(Sn2, region = 'id')
# EU + EU neighbor country regions
eushp_cntrs <- fortify(Sneighbors)

save(eushp_borders, file = './data-raw/eushp_borders.RData')
save(eushp_nuts2, file = './data-raw/eushp_nuts2.RData')
save(eushp_cntrs, file = './data-raw/eushp_cntrs.RData')
