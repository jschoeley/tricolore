## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  tidy = FALSE,
  comment = "#>",
  fig.width = 6, fig.height = 6
)

## ------------------------------------------------------------------------
library(tricolore)
library(dplyr)

as_tibble(euro_example)

## ------------------------------------------------------------------------
# color-code the data set and generate a color-key
tric <- Tricolore(euro_example, p1 = 'ed_0to2', p2 = 'ed_3to4', p3 = 'ed_5to8')

## ------------------------------------------------------------------------
# add the vector of colors to the `euro_example` data
euro_example$rgb <- tric$rgb

## ------------------------------------------------------------------------
library(ggplot2)

plot_educ <-
  # using sf dataframe `euro_example`...
  ggplot(euro_example) +
  # ...draw a polygon for each region...
  geom_sf(aes(fill = rgb, geometry = geometry), size = 0.1) +
  # ...and color each region according to the color code in the variable `rgb`
  scale_fill_identity()

plot_educ 

## ------------------------------------------------------------------------
library(ggtern)
plot_educ +
  annotation_custom(
    ggplotGrob(tric$key),
    xmin = 55e5, xmax = 75e5, ymin = 8e5, ymax = 80e5
  )

## ------------------------------------------------------------------------
plot_educ <-
  plot_educ +
  annotation_custom(
    ggplotGrob(tric$key +
                 theme(plot.background = element_rect(fill = NA, color = NA)) +
                 labs(L = '0-2', T = '3-4', R = '5-8')),
    xmin = 55e5, xmax = 75e5, ymin = 8e5, ymax = 80e5
  )
plot_educ

## ------------------------------------------------------------------------
plot_educ +
  theme_void() +
  coord_sf(datum = NA) +
  labs(title = 'European inequalities in educational attainment',
       subtitle = 'Regional distribution of ISCED education levels for people aged 25-64 in 2016.',
       caption = 'Data by eurostat (edat_lfse_04).')

## ------------------------------------------------------------------------
# color-code the data set and generate a color-key
tric <- Tricolore(euro_example, p1 = 'ed_0to2', p2 = 'ed_3to4', p3 = 'ed_5to8',
                  breaks = Inf)

# add the vector of colors to the `euro_example` data
euro_example$rgb <- tric$rgb

## ------------------------------------------------------------------------
library(sf)
library(leaflet)

euro_example %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addPolygons(smoothFactor = 0.1, weight = 0,
              fillColor = euro_example$rgb,
              fillOpacity = 1)

## ------------------------------------------------------------------------
euro_example %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldTerrain) %>%
  addPolygons(smoothFactor = 0.1, weight = 0,
              fillColor = euro_example$rgb,
              fillOpacity = 1,
              popup =
                paste0(
                  '<b>', euro_example$name, '</b></br>',
                  'Primary: ',
                  formatC(euro_example$ed_0to2*100,
                          digits = 1, format = 'f'), '%</br>',
                  'Secondary: ',
                  formatC(euro_example$ed_3to4*100,
                          digits = 1, format = 'f'), '%</br>',
                  'Tertiary: ',
                  formatC(euro_example$ed_5to8*100,
                          digits = 1, format = 'f'), '%</br>'
                )
  )

## ------------------------------------------------------------------------
makePlotURI <- function(expr, width, height, ...) {
  pngFile <- shiny::plotPNG(function() { expr }, width = width, height = height, ...)
  on.exit(unlink(pngFile))

  base64 <- httpuv::rawToBase64(readBin(pngFile, raw(1), file.size(pngFile)))
  paste0("data:image/png;base64,", base64)
}

legend_symbol <- makePlotURI({
  print(tric$key +
          theme(plot.background = element_rect(fill = NA, color = NA)) +
          labs(L = '0-2', T = '3-4', R = '5-8'))
}, 200, 200, bg = "transparent")

df <- data.frame(
  lng = 30,
  lat = 70,
  plot = legend_symbol,
  stringsAsFactors = FALSE
)

euro_example %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldTerrain) %>%
  addPolygons(smoothFactor = 0.1, weight = 0,
              fillColor = euro_example$rgb,
              fillOpacity = 1,
              popup =
                paste0(
                  '<b>', euro_example$name, '</b></br>',
                  'Primary: ',
                  formatC(euro_example$ed_0to2*100,
                          digits = 1, format = 'f'), '%</br>',
                  'Secondary: ',
                  formatC(euro_example$ed_3to4*100,
                          digits = 1, format = 'f'), '%</br>',
                  'Tertiary: ',
                  formatC(euro_example$ed_5to8*100,
                          digits = 1, format = 'f'), '%</br>'
                )
  ) %>%
  addMarkers(data = df, icon = ~icons(plot))

