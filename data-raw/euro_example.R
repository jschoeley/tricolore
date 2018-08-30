#'---
#' title: Geodata for European NUTS-2 regions with added variables
#' author: Jonas Schöley
#' date: 2018-08-28
#'---

# Init --------------------------------------------------------------------

library(tidyverse)
library(sf)
library(eurostat)

# European NUTS-2 geodata -------------------------------------------------

# download geodata on nuts-2 regions

euro_geo_nuts2 <-
  get_eurostat_geospatial(output_class = 'sf',
                          resolution = '60', nuts_level = 2, year = 2013) %>%
  # project to crs 3035
  st_transform(crs = 3035) %>%
  # crop to Europe
  st_crop(xmin = 25e5, xmax = 75e5, ymin = 13.5e5, ymax = 54.5e5) %>%
  # select nuts id, region name and geometry columns
  select(id, name = NUTS_NAME, geometry)

# Download data on European educational composition -----------------------

# download data on education composition by NUTS-2 level for Europe
educ <- get_eurostat('edat_lfse_04')

# select data for 2016 and calculate shares
euro_education <-
  educ %>%
  mutate(year = lubridate::year(time),
         id = as.character(geo)) %>%
  # year 2016, total population, nuts 2 levels
  filter(year == 2016,
         str_length(geo) == 4,
         isced11 %in% c('ED0-2', 'ED3_4', 'ED5-8'),
         sex == 'T') %>%
  mutate(values = values/100) %>%
  spread(isced11, values) %>%
  select(id, ed_0to2 = `ED0-2`, ed_3to4 = `ED3_4`, ed_5to8 = `ED5-8`)

# Download data on European labor-force composition -----------------------

# download data on labor-force composition by NUTS-2 level for Europe
lf <- get_eurostat("lfst_r_lfe2en2")

# select data for 2016, recode to ternary sectors and calculate shares
euro_sectors <-
  lf %>%
  # recode time as year
  mutate(year = as.integer(lubridate::year(time))) %>%
  # subset to total age, year 2016 and NUTS-2 regions
  filter(
    age == 'Y_GE15',
    str_length(geo) == 4,
    year == 2016
  ) %>%
  # recode into three sectors
  mutate(
    sector = recode(as.character(nace_r2),
                    `A` = 'primary',
                    `B-E` = 'secondary',
                    `F` = 'secondary'),
    sector = ifelse(!sector %in% c('primary', 'secondary', 'TOTAL'),
                    'tertiary',
                    sector)
  ) %>%
  group_by(year, geo, sector) %>%
  summarise(N = sum(values, na.rm = TRUE)) %>%
  ungroup() %>%
  # calculate shares on total
  spread(sector, N) %>%
  mutate_at(vars(primary, secondary, tertiary), .funs = funs(./TOTAL)) %>%
  # simplify
  mutate(id = as.character(geo)) %>%
  select(id, lf_pri = primary, lf_sec = secondary, lf_ter = tertiary)

# Join compositional data with geodata ------------------------------------

euro_example <-
  euro_geo_nuts2 %>%
  left_join(euro_education, 'id') %>%
  left_join(euro_sectors, 'id')

save(euro_example, file = './data-raw/euro_example.RData', compress = 'xz')

#
# foo <- tricolore::Tricolore(euro_example,
#                             p1 = 'primary', p2 = 'secondary', p3 = 'tertiary',
#                             center = NA, hue = 0.2)
#
# euro_example %>%
#   st_transform(crs = 4326) %>%
#   leaflet() %>%
#   addProviderTiles(providers$Esri.WorldTerrain) %>%
#   addPolygons(color = str_sub(foo$hexsrgb, 1, 7),
#               weight = 1, smoothFactor = 0.1,
#               fillColor = str_sub(foo$hexsrgb, 1, 7),
#               fillOpacity = 1,
#               popup =
#                 paste0(
#                   euro_example$name, '</br>',
#                   'Primary: ',
#                   formatC(euro_example$primary*100,
#                           digits = 1, format = 'f'), '%</br>',
#                   ' Secondary: ',
#                   formatC(euro_example$secondary*100,
#                           digits = 1, format = 'f'), '%</br>',
#                   ' Tertiary: ',
#                   formatC(euro_example$tertiary*100,
#                           digits = 1, format = 'f'), '%</br>'
#                 )
#   )
