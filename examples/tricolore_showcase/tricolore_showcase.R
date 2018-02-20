#' ---
#' title: Showcase various tricolore features
#' author: Jonas Schöley
#' date:
#' output:
#'   github_document
#' ---

# Init --------------------------------------------------------------------

library(tricolore) # devtools::install_github('jschoeley/tricolore')
library(ggtern)

gghole <- function (fort) {
  poly <- fort[fort$id %in% fort[fort$hole, ]$id, ]
  hole <- fort[!fort$id %in% fort[fort$hole, ]$id, ]
  out <- list(poly, hole)
  names(out) <- c('poly', 'hole')
  return(out)
}

# Load geo data -----------------------------------------------------------

# high-res files
load('./examples/tricolore_showcase/euro_geo_nuts2.RData')
load('./examples/tricolore_showcase/euro_basemap.RData')

# Color-code ternary compositions -----------------------------------------

tric <- Tricolore(euro_education, p1 = 'ed0_2', p2 = 'ed3_4', p3 = 'ed5_8')
euro_education$srgb <- tric$hexsrgb

euro_educ_map <- gghole(dplyr::left_join(euro_education, euro_geo_nuts2, by = 'id'))
p1 <-
  euro_basemap +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$poly) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$hole) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      tric$legend +
        theme(plot.background = element_rect(fill = NA, color = NA),
              tern.axis.title = element_text(size = 10),
              plot.caption = element_text(size = 8, hjust = 0)) +
        labs(L = '0 to 2', T = '3 to 4', R = '5 to 8',
             caption = 'Percent share of regional population\nby education level')
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(subtitle = 'Regional distribution of ISCED education levels for people aged 25-64 in 2016',
       caption = 'Data by eurostat (edat_lfse_04).')

# ggsave(p_eudc_cont,
#        filename = 'europe_educ.svg', device = svglite::svglite,
#        path = './examples/european_education_distribution',
#        width = 7, height = 7)

# Support for discrete color scales I -------------------------------------

tric <- Tricolore(euro_education, p1 = 'ed0_2', p2 = 'ed3_4', p3 = 'ed5_8',
                  breaks = 4, show_center = FALSE)
euro_education$srgb <- tric$hexsrgb

euro_educ_map <- gghole(dplyr::left_join(euro_education, euro_geo_nuts2, by = 'id'))
p2 <-
  euro_basemap +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$poly) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$hole) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      tric$legend +
        theme(plot.background = element_rect(fill = NA, color = NA),
              tern.axis.title = element_text(size = 10),
              plot.caption = element_text(size = 8, hjust = 0)) +
        labs(L = '0 to 2', T = '3 to 4', R = '5 to 8',
             caption = 'Percent share of regional population\nby education level')
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(subtitle = 'Regional distribution of ISCED education levels for people aged 25-64 in 2016',
       caption = 'Data by eurostat (edat_lfse_04).')

# Support for discrete color scales II ------------------------------------

tric <- Tricolore(euro_education, p1 = 'ed0_2', p2 = 'ed3_4', p3 = 'ed5_8',
                  breaks = 2, show_center = FALSE)
euro_education$srgb <- tric$hexsrgb

euro_educ_map <- gghole(dplyr::left_join(euro_education, euro_geo_nuts2, by = 'id'))
p3 <-
  euro_basemap +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$poly) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$hole) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      tric$legend +
        theme(plot.background = element_rect(fill = NA, color = NA),
              tern.axis.title = element_text(size = 10),
              plot.caption = element_text(size = 8, hjust = 0)) +
        labs(L = '0 to 2', T = '3 to 4', R = '5 to 8',
             caption = 'Percent share of regional population\nby education level')
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(subtitle = 'Regional distribution of ISCED education levels for people aged 25-64 in 2016',
       caption = 'Data by eurostat (edat_lfse_04).')

# Flexible parametrization I ----------------------------------------------

tric <- Tricolore(euro_education, p1 = 'ed0_2', p2 = 'ed3_4', p3 = 'ed5_8',
                  hue = 0.55)
euro_education$srgb <- tric$hexsrgb

euro_educ_map <- gghole(dplyr::left_join(euro_education, euro_geo_nuts2, by = 'id'))
p4 <-
  euro_basemap +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$poly) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$hole) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      tric$legend +
        theme(plot.background = element_rect(fill = NA, color = NA),
              tern.axis.title = element_text(size = 10),
              plot.caption = element_text(size = 8, hjust = 0)) +
        labs(L = '0 to 2', T = '3 to 4', R = '5 to 8',
             caption = 'Percent share of regional population\nby education level')
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(subtitle = 'Regional distribution of ISCED education levels for people aged 25-64 in 2016',
       caption = 'Data by eurostat (edat_lfse_04).')

# Flexible parametrization II ---------------------------------------------

tric <- Tricolore(euro_education, p1 = 'ed0_2', p2 = 'ed3_4', p3 = 'ed5_8',
                  hue = 0.55, contrast = 1, chroma = 1, lightness = 0.7)
euro_education$srgb <- tric$hexsrgb

euro_educ_map <- gghole(dplyr::left_join(euro_education, euro_geo_nuts2, by = 'id'))
p5 <-
  euro_basemap +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$poly) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$hole) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      tric$legend +
        theme(plot.background = element_rect(fill = NA, color = NA),
              tern.axis.title = element_text(size = 10),
              plot.caption = element_text(size = 8, hjust = 0)) +
        labs(L = '0 to 2', T = '3 to 4', R = '5 to 8',
             caption = 'Percent share of regional population\nby education level')
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(subtitle = 'Regional distribution of ISCED education levels for people aged 25-64 in 2016',
       caption = 'Data by eurostat (edat_lfse_04).')

# Flexible parametrization III --------------------------------------------

tric <- Tricolore(euro_education, p1 = 'ed0_2', p2 = 'ed3_4', p3 = 'ed5_8',
                  hue = 0.55, contrast = 0, chroma = 0.6, lightness = 0.7)
euro_education$srgb <- tric$hexsrgb

euro_educ_map <- gghole(dplyr::left_join(euro_education, euro_geo_nuts2, by = 'id'))
p6 <-
  euro_basemap +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$poly) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_educ_map$hole) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      tric$legend +
        theme(plot.background = element_rect(fill = NA, color = NA),
              tern.axis.title = element_text(size = 10),
              plot.caption = element_text(size = 8, hjust = 0)) +
        labs(L = '0 to 2', T = '3 to 4', R = '5 to 8',
             caption = 'Percent share of regional population\nby education level')
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(subtitle = 'Regional distribution of ISCED education levels for people aged 25-64 in 2016',
       caption = 'Data by eurostat (edat_lfse_04).')

# Centering I -------------------------------------------------------------

tric <- Tricolore(euro_sectors, p1 = 'primary', p2 = 'secondary', p3 = 'tertiary')
euro_sectors$srgb <- tric$hexsrgb

euro_sectors_map <- gghole(dplyr::left_join(euro_sectors, euro_geo_nuts2, by = 'id'))

p7 <-
  euro_basemap +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_sectors_map$poly) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_sectors_map$hole) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      tric$legend +
        theme(plot.background = element_rect(fill = NA, color = NA),
              tern.axis.title = element_text(size = 10),
              plot.caption = element_text(size = 8, hjust = 0)) +
        labs(L = '0 to 2', T = '3 to 4', R = '5 to 8',
             caption = 'Percent share of regional labor-force\nby sector')
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(subtitle = 'Regional distribution of labor-force composition in 2016',
       caption = 'Data by eurostat (edat_lfse_04).')

# Centering II ------------------------------------------------------------

tric <- Tricolore(euro_sectors, p1 = 'primary', p2 = 'secondary', p3 = 'tertiary',
                  center = NA)
euro_sectors$srgb <- tric$hexsrgb

euro_sectors_map <- gghole(dplyr::left_join(euro_sectors, euro_geo_nuts2, by = 'id'))

p8 <-
  euro_basemap +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_sectors_map$poly) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = srgb),
               data = euro_sectors_map$hole) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      tric$legend +
        theme(plot.background = element_rect(fill = NA, color = NA),
              tern.axis.title = element_text(size = 10),
              plot.caption = element_text(size = 8, hjust = 0)) +
        labs(L = '0 to 2', T = '3 to 4', R = '5 to 8',
             caption = 'Percent share of regional labor-force\nby sector. Colors show\ndeviations from average region.')
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(subtitle = 'Regional distribution of labor-force composition in 2016',
       caption = 'Data by eurostat (edat_lfse_04).')
