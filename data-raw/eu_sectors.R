#'---
#' title: Prepare data on 2016 EU labor force composition by NUTS-2 region
#' author: Jonas Schöley
#' date: 2017-11-09
#'---

# Init --------------------------------------------------------------------

library(tidyverse)

# Input -------------------------------------------------------------------

# labor market composition by EU nuts 2
dat <- read_csv('./data-raw/lfst_r_lfe2en2_1_Data.csv', na = ':')

# Transform ---------------------------------------------------------------

eu_sectors <-
  dat %>%
  # only 2016
  filter(TIME == 2016) %>%
  # compute labor force share by sector
  select(TIME, GEO, NACE_R2, Value) %>%
  mutate(
    sector = recode(NACE_R2,
                    `A` = 'primary',
                    `B-E` = 'secondary',
                    `F` = 'secondary'),
    sector = ifelse(!sector %in% c('primary', 'secondary', 'TOTAL'),
                    'tertiary',
                    sector)
  ) %>%
  group_by(TIME, GEO, sector) %>%
  summarise(N = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(sector, N) %>%
  mutate_at(vars(primary, secondary, tertiary), .funs = funs(./TOTAL)) %>%
  # simplify
  select(-TOTAL, -TIME) %>%
  rename(nuts2 = GEO)

save(eu_sectors, file = './data-raw/eu_sectors.RData')
