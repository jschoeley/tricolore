#'---
#' title: Prepare data on 2016 European labor force composition by NUTS-2 region
#' author: Jonas Schöley
#' date: 2018-01-18
#'---

# Init --------------------------------------------------------------------

library(tidyverse)
library(eurostat)

# Download data on European labor-force composition -----------------------

# download data on labor-force composition by NUTS-2 level for Europe
lf <- get_eurostat("lfst_r_lfe2en2")

euro_sectors <-
  lf %>%
  # recode time as year
  mutate(year = as.integer(lubridate::year(time))) %>%
  # subset to total age and NUTS-2 regions
  filter(
    age == 'Y_GE15',
    str_length(geo) == 4
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
  mutate(nuts2 = as.character(geo)) %>%
  select(year, nuts2, primary, secondary, tertiary)

save(euro_sectors, file = './data-raw/euro_sectors.RData')
