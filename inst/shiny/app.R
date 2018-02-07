library(shiny)
library(dplyr)
library(ggtern)
library(tricolore)

# Functions ---------------------------------------------------------------

gghole <- function (fort) {
  poly <- fort[fort$id %in% fort[fort$hole, ]$id, ]
  hole <- fort[!fort$id %in% fort[fort$hole, ]$id, ]
  out <- list(poly, hole)
  names(out) <- c("poly", "hole")
  return(out)
}

# UI ----------------------------------------------------------------------

ui <- fluidPage(

  titlePanel(title = 'Tricolore: A flexible color scale for ternary compositions'),

  sidebarLayout(

    # INPUT
    sidebarPanel(width = 3,
                 sliderInput(inputId = 'hue', label = 'Hue', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.3),
                 sliderInput(inputId = 'chroma', label = 'Chroma', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.9),
                 sliderInput(inputId = 'lightness', label = 'Lightness', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.8),
                 sliderInput(inputId = 'contrast', label = 'Contrast', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.6),
                 sliderInput(inputId = 'spread', label = 'Spread',
                             min = 0.5, max = 2, step = 0.1, value = 1, ticks = FALSE),
                 sliderInput(inputId = 'breaks', label = 'Discretization', ticks = FALSE,
                             min = 2, max = 20, step = 1, value = 5),
                 radioButtons(inputId = 'center', label = 'Mean centering',
                              choices = list(No = 'No', Yes = 'Yes'),
                              selected = 'No'),
                 radioButtons(inputId = 'show_data', label = 'Show data',
                              choices = list(No = FALSE, Yes = TRUE),
                              selected = 'FALSE')
    ),

    # OUTPUT
    mainPanel(plotOutput(outputId = 'example'))
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {

  euro_sectors <- filter(euro_sectors, year == 2016)

  output$example <- renderPlot(width = 800, height = 700, {


    # mix color, generate legend
    mixed <- Tricolore(euro_sectors,
                       p1 = 'primary', p2 = 'secondary', p3 = 'tertiary',
                       breaks = input$breaks,
                       hue = input$hue, chroma = input$chroma,
                       lightness = input$lightness, contrast = input$contrast,
                       center = switch(input$center, No = rep(1/3,3), Yes = NA),
                       spread = input$spread, show_data = input$show_data,
                       show_center = FALSE, legend = TRUE)

    # customize legend
    lgnd <- mixed[['legend']] +
      labs(x = 'Primary', y = 'Secondary', z = 'Tertiary',
           caption = 'Labor force composition in European regions 2016') +
      theme(plot.background = element_rect(fill = NA, color = NA))

    # merge data and map
    euro_sectors$rgb <- mixed[['hexsrgb']]
    euro_nuts2_sectors <- gghole(right_join(euro_geo_nuts2,
                                            euro_sectors,
                                            c('id' = 'nuts2')))

    # generate map
    euro_map <-
      euro_basemap +
      geom_polygon(aes(fill = rgb), color = NA,
                   data = euro_nuts2_sectors$poly) +
      geom_polygon(aes(fill = rgb), color = NA,
                   data = euro_nuts2_sectors$hole) +
      annotation_custom(ggplotGrob(lgnd),
                        xmin = 55e5, xmax = Inf,
                        ymin = 38e5, ymax = Inf) +
      scale_fill_identity() +
      labs(caption = 'Data by eurostat. Jonas Schöley | github.com/jschoeley/tricolore | twitter: @jschoeley')

    print(euro_map)
  })

}

shinyApp(ui, server)
