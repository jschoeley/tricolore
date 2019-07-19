#'---
#' title: Geodata for European NUTS-2 regions with added variables
#' author: Jonas Schöley
#' date: 2019-07-19
#'---

# Init --------------------------------------------------------------------

library(tidyverse)
library(stringi)
library(sf)
library(rmapshaper)
library(eurostat)

# European NUTS-2 geodata -------------------------------------------------

# download geodata on nuts-2 regions
euro_geo_nuts2 <-
  get_eurostat_geospatial(output_class = 'sf',
                          resolution = '60', nuts_level = 2, year = 2016) %>%
  # exclude some regions which don't report
  # the statistics we're interested in
  filter(!(str_detect(geo, '^AL') | str_detect(geo, '^LI') | geo == 'FI20')) %>%
  # project to crs 3035
  st_transform(crs = 3035) %>%
  # pseudo-buffer regions to avoid self-intersection errors
  st_buffer(0) %>%
  # crop to Europe
  st_crop(xmin = 25e5, xmax = 75e5, ymin = 13.5e5, ymax = 54.5e5) %>%
  # simplify to save space
  ms_simplify(keep = 0.05, keep_shapes = TRUE) %>%
  # transliterate non-ASCII characters in region names
  # (so that CRAN-check stops complaining)
  mutate(
    name = stri_trans_general(NUTS_NAME, id = 'any-latin; latin-ascii')
  ) %>%
  # select nuts id, region name and geometry columns
  select(id, name, geometry)

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
  select(id, ed_0to2 = `ED0-2`, ed_3to4 = `ED3_4`, ed_5to8 = `ED5-8`) %>%
  drop_na()

# Download data on European labor-force composition -----------------------

# download data on labor-force composition by NUTS-2 level for Europe
lf <- get_eurostat("lfst_r_lfe2en2")

# select data for 2016, recode to ternary sectors and calculate shares
euro_sectors <-
  lf %>%
  # recode time as year and geo as character
  mutate(
    year = as.integer(lubridate::year(time)),
    geo = as.character(geo)
  ) %>%
  # subset to total age, year 2016 and NUTS-2 regions
  filter(
    age == 'Y_GE15',
    str_length(geo) == 4,
    year == 2016
  ) %>%
  # if a sector wasn't reported, assume no one worked there
  # (this is motivated by the "missing" agricultural workers in innner london)
  complete(nace_r2, geo, year, fill = list(values = 0)) %>%
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
  mutate_at(vars(primary, secondary, tertiary), .funs = ~ ./TOTAL) %>%
  # simplify
  select(id = geo, lf_pri = primary, lf_sec = secondary, lf_ter = tertiary) %>%
  drop_na()

# Join compositional data with geodata ------------------------------------

euro_example <-
  euro_geo_nuts2 %>%
  left_join(euro_education, 'id') %>%
  left_join(euro_sectors, 'id') %>%
  arrange(id)

save(
  euro_example,
  file = './data-raw/euro_example.RData',
  compress = 'xz',
  version = 2
)

# Test --------------------------------------------------------------------

# library(leaflet)
# foo <- tricolore::Tricolore(euro_example,
#                             p1 = 'lf_pri', p2 = 'lf_sec', p3 = 'lf_ter',
#                             center = NA, hue = 0.2)
# euro_example %>%
#   st_transform(crs = 4326) %>%
#   leaflet() %>%
#   addProviderTiles(providers$Esri.WorldTerrain) %>%
#   addPolygons(color = str_sub(foo$rgb, 1, 7),
#               weight = 1, smoothFactor = 0.1,
#               fillColor = str_sub(foo$rgb, 1, 7),
#               fillOpacity = 1,
#               popup =
#                 paste0(
#                   euro_example$id, euro_example$name, '</br>',
#                   'Primary: ',
#                   formatC(euro_example$lf_pri*100,
#                           digits = 1, format = 'f'), '%</br>',
#                   ' Secondary: ',
#                   formatC(euro_example$lf_sec*100,
#                           digits = 1, format = 'f'), '%</br>',
#                   ' Tertiary: ',
#                   formatC(euro_example$lf_ter*100,
#                           digits = 1, format = 'f'), '%</br>'
#                 )
#   )
# foo <- tricolore::Tricolore(euro_example,
#                             p1 = 'ed_0to2', p2 = 'ed_3to4', p3 = 'ed_5to8', hue = 0.2)
# euro_example %>%
#   st_transform(crs = 4326) %>%
#   leaflet() %>%
#   addProviderTiles(providers$Esri.WorldTerrain) %>%
#   addPolygons(color = str_sub(foo$rgb, 1, 7),
#               weight = 1, smoothFactor = 0.1,
#               fillColor = str_sub(foo$rgb, 1, 7),
#               fillOpacity = 1,
#               popup =
#                 paste0(
#                   euro_example$id, euro_example$name, '</br>',
#                   'Primary: ',
#                   formatC(euro_example$ed_0to2*100,
#                           digits = 1, format = 'f'), '%</br>',
#                   ' Secondary: ',
#                   formatC(euro_example$ed_3to4*100,
#                           digits = 1, format = 'f'), '%</br>',
#                   ' Tertiary: ',
#                   formatC(euro_example$ed_5to8*100,
#                           digits = 1, format = 'f'), '%</br>'
#                 )
#   )
