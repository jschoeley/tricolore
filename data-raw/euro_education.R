#'---
#' title: Prepare data on 2016 European education composition by NUTS-2 region
#' author: Jonas Schöley
#' date: 2018-02-19
#'---

# Init --------------------------------------------------------------------

library(tidyverse)
library(eurostat)

# Download data on European educational composition -----------------------

dat <- eurostat::get_eurostat('edat_lfse_04')

euro_education <-
  dat %>%
  mutate(year = lubridate::year(time),
         id = as.character(geo)) %>%
  # year 2016, total population, nuts 2 levels
  filter(year == 2016,
         str_length(geo) == 4,
         isced11 %in% c('ED0-2', 'ED3_4', 'ED5-8'),
         sex == 'T') %>%
  mutate(values = values/100) %>%
  spread(isced11, values) %>%
  select(id, ed0_2 = `ED0-2`, ed3_4 = `ED3_4`, ed5_8 = `ED5-8`)

save(euro_education, file = './data-raw/euro_education.RData', compress = 'xz')
