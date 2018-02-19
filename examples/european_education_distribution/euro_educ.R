#' ---
#' title: European inequalities in educational attainment
#' author: Jonas Schöley
#' date:
#' output:
#'   github_document
#' ---

# Init --------------------------------------------------------------------

library(tidyverse)
library(eurostat)
library(tricolore) # devtools::install_github('jschoeley/tricolore')
library(ggtern)
library(extrafont)

gghole <- function (fort) {
  poly <- fort[fort$geo %in% fort[fort$hole, ]$geo, ]
  hole <- fort[!fort$geo %in% fort[fort$hole, ]$geo, ]
  out <- list(poly, hole)
  names(out) <- c("poly", "hole")
  return(out)
}

# Load geo data -----------------------------------------------------------

load('./examples/european_education_distribution/euro_geo_nuts2.RData')
load('./examples/european_education_distribution/euro_basemap.RData')

# Prepare education data --------------------------------------------------

dat <- eurostat::get_eurostat('edat_lfse_04')

euro_educ <-
  dat %>%
  mutate(year = lubridate::year(time)) %>%
  filter(year == 2016,
         str_length(geo) == 4,
         isced11 %in% c('ED0-2', 'ED3_4', 'ED5-8'),
         sex == 'T') %>%
  spread(isced11, values) %>%
  select(geo, ed0_2 = `ED0-2`, ed3_4 = `ED3_4`, ed5_8 = `ED5-8`)

# Perform color-coding ----------------------------------------------------

tric <- Tricolore(euro_educ, p1 = 'ed0_2', p2 = 'ed3_4', p3 = 'ed5_8',
                  breaks = 4,
                  h = 0.2, lightness = 0.9, chroma = 1,
                  contrast = 0.7, show_center = FALSE)
euro_educ$srgb <- tric$hexsrgb

# Plot map ----------------------------------------------------------------

euro_educ_map <- gghole(left_join(euro_educ, euro_geo_nuts2,
                                  by = c('geo' = 'id')))
p_eudc_cont <-
  euro_basemap +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$poly) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$hole) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      tric$legend +
        theme(
          text = element_text(family = 'Roboto Condensed'),
          plot.background = element_rect(fill = NA, color = NA),
          tern.axis.title = element_text(size = 10)) +
        labs(L = 'Lower secondary\nor less', T = 'Upper secondary', R = 'Tertiary')
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(title = 'European inequalities in educational attainment',
       subtitle = 'Regional distribution of education levels for people aged 25-64 in 2016.',
       caption = 'Data by eurostat (edat_lfse_04) | github.com/jschoeley/tricolore | Jonas Schöley (@jschoeley)')

ggsave(p_eudc_cont,
       filename = 'europe_educ.svg', device = svglite::svglite,
       path = './examples/european_education_distribution',
       width = 7, height = 7)
