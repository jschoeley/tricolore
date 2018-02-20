## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  tidy = FALSE,
  comment = "#>",
  fig.width = 6, fig.height = 6
)

## ------------------------------------------------------------------------
library(tricolore)
euro_education

## ------------------------------------------------------------------------
# color-code the data set and generate a color-key
tric <- Tricolore(euro_education, p1 = 'ed0_2', p2 = 'ed3_4', p3 = 'ed5_8',
                  breaks = 4)

## ------------------------------------------------------------------------
# add the vector of colors to the `euro_education` data
euro_education$rgb <- tric$hexsrgb
euro_education

## ------------------------------------------------------------------------
# merge the geodata with the color-coded compositional data
euro_educ_map <- dplyr::left_join(euro_education, euro_geo_nuts2, by = 'id')

## ------------------------------------------------------------------------
library(ggplot2)

plot_educ <-
  # using data `euro_educ_map`...
  ggplot(euro_educ_map) +
  # ...draw a polygon for each `group` along `long` and `lat`...
  geom_polygon(aes(x = long, y = lat, group = group, fill = rgb)) +
  # ...and color each region according to the color code in the variable `rgb`
  scale_fill_identity()

plot_educ 

## ------------------------------------------------------------------------
library(ggtern)

plot_educ +
  annotation_custom(
    ggplotGrob(tric$legend),
    xmin = 55e5, xmax = Inf, ymin = 35e5, ymax = Inf
  )

## ------------------------------------------------------------------------
plot_educ <-
  plot_educ +
  annotation_custom(
    ggplotGrob(tric$legend +
                 theme(plot.background = element_rect(fill = NA, color = NA)) +
                 labs(L = '0-2', T = '3-4', R = '5-8')),
    xmin = 55e5, xmax = Inf, ymin = 35e5, ymax = Inf
  )
plot_educ

## ------------------------------------------------------------------------
plot_educ +
  theme_void() +
  labs(title = 'European inequalities in educational attainment',
       subtitle = 'Regional distribution of ISCED education levels for people aged 25-64 in 2016.',
       caption = 'Data by eurostat (edat_lfse_04).')

