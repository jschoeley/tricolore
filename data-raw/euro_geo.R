#'---
#' title: Prepare Europe map geodata
#' author: Ilya Kashnitsky, Jonas Schöley
#' date: 2018-01-18
#'---

# Init --------------------------------------------------------------------

# original code by ikashnitsky.github.io 2017-06-30,
# adapted by Jonas Schöley
library(tidyverse)
library(lubridate)
library(forcats)
library(rgdal)
library(rgeos)
library(maptools)
library(tidyverse)
library(rgeos)
library(ggplot2)

# Geodata European regions ------------------------------------------------

# geodata will be stored in a directory 'geodata'
ifelse(!dir.exists('./data-raw/geodata'),
       dir.create('./data-raw/geodata'),
       paste('Directory already exists'))

# Eurostat shapefiles for European regions
# http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units
f <- tempfile()
download.file(
  'http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2013_20M_SH.zip',
  destfile = f
)
unzip(f, exdir = './data-raw/geodata/.')
euro_geo <- readOGR('./data-raw/geodata/NUTS_2013_20M_SH/data/.', 'NUTS_RG_20M_2013')

# colnames to lower case
names(euro_geo@data) <- tolower(names(euro_geo@data))

# change coordinate system to LAEA Europe (EPSG:3035)
# check out https://epsg.io
epsg3035 <- '+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
euro_geo <- spTransform(euro_geo, CRS(epsg3035))

# subset NUTS-2 regions
euro_geo_nuts2 <- euro_geo[euro_geo$stat_levl_ == 2,]

# remove remote areas and convert to df
remote <- c(paste0('ES', c(63, 64, 70)),
            paste('FRA', 1:5, sep=''),
            'PT20', 'PT30')
euro_geo_nuts2 <-
  fortify(euro_geo, region = 'nuts_id') %>%
  filter(!str_sub(id, 1, 4) %in% remote,
         !str_sub(id, 1, 2) == 'AL') %>%
  mutate_if(is.factor, as.character)

# check
ggplot(euro_geo_nuts2) +
  geom_map(aes(map_id = id), map = euro_geo_nuts2) +
  expand_limits(x = euro_geo_nuts2$long, y = euro_geo_nuts2$lat) +
  theme_void()

save(euro_geo_nuts2, file = './data-raw/euro_geo_nuts2.RData', compress = 'xz')

# Geodata European countries and neighbours -------------------------------

# download shapefiles for EU neighbours
f <- tempfile()
download.file(
  'http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/CNTR_2010_20M_SH.zip',
  destfile = f
)
unzip(f, exdir = './data-raw/geodata/.')
world_geo <- readOGR('./data-raw/geodata/CNTR_2010_20M_SH/CNTR_2010_20M_SH/Data/.',
                    'CNTR_RG_20M_2010')

# colnames to lower case
names(world_geo@data) <- tolower(names(world_geo@data))

# select the Europe and neighbour countries
euro_region <- c('AT', 'BE', 'BG', 'CH', 'CZ', 'DE', 'DK',
                 'EE', 'EL', 'ES', 'FI', 'FR', 'HU', 'IE', 'IS', 'IT', 'LT',
                 'LV', 'NL', 'NO', 'PL', 'PT', 'SE', 'SI', 'SK', 'UK', 'IM',
                 'FO', 'GI', 'LU', 'LI', 'AD', 'MC', 'MT', 'VA', 'SM', 'HR',
                 'BA', 'ME', 'MK', 'AL', 'RS', 'RO', 'MD', 'UA', 'BY', 'RU',
                 'TR', 'CY', 'EG', 'LY', 'TN', 'DZ', 'MA', 'GG', 'JE', 'KZ',
                 'AM', 'GE', 'AZ', 'SY', 'IQ', 'IR', 'IL', 'JO', 'PS', 'SA',
                 'LB', 'MN', 'LY', 'EG')
euro_region_geo <- world_geo[world_geo$cntr_id %in% euro_region,]

# reproject the shapefile to a pretty projection for mapping Europe
euro_region_geo <- spTransform(euro_region_geo, CRS(epsg3035))

# rectangular cut to region of interest
rect <- readWKT(
  'POLYGON((20e5 10e5,
  80e5 10e5,
  80e5 60e5,
  20e5 60e5,
  20e5 10e5))')
euro_region_geo <- gIntersection(euro_region_geo, rect, byid = TRUE)

# convert to df
euro_region_geo <-
  fortify(euro_region_geo, region = 'nuts_id') %>%
  mutate_if(is.factor, as.character)

# check
ggplot(euro_region_geo) +
  geom_map(aes(map_id = id), fill = 'grey',
           map = euro_region_geo,
           data = euro_region_geo) +
  expand_limits(x = euro_region_geo$long, y = euro_region_geo$lat) +
  theme_void()

save(euro_region_geo, file = './data-raw/euro_region_geo.RData', compress = 'xz')
