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

  titlePanel(title = 'Tricolore: A balanced color scale for ternary compositions.'),

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
                 sliderInput(inputId = 'k', label = 'Discretization', ticks = FALSE,
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

  output$example <- renderPlot(width = 700, height = 700, {

    # mix color, generate legend
    mixed <- Tricolore(eu_sectors,
                       p1 = 'primary', p2 = 'secondary', p3 = 'tertiary',
                       k = input$k,
                       hue = input$hue, chroma = input$chroma,
                       lightness = input$lightness, contrast = input$contrast,
                       center = switch(input$center, No = rep(1/3,3), Yes = NA),
                       spread = input$spread, show_data = input$show_data,
                       show_center = FALSE, legend = TRUE)

    # customize legend
    lgnd <- mixed[['legend']] +
      labs(x = 'Pri', y = 'Sec', z = 'Ter') +
      theme(plot.background = element_rect(color = 'grey90'))

    # merge data and map
    eu_sectors$rgb <- mixed[['hexsrgb']]
    eu_nuts2_sectors <- gghole(dplyr::right_join(eushp_nuts2,
                                                 eu_sectors,
                                                 c('id' = 'nuts2')))

    # generate map
    eumap <- europe_map +
      geom_polygon(aes(fill = rgb), color = NA,
                   data = eu_nuts2_sectors$poly) +
      geom_polygon(aes(fill = rgb), color = NA,
                   data = eu_nuts2_sectors$hole) +
      annotation_custom(ggplotGrob(lgnd),
                        xmin = -8.1e5, xmax = 73e5,
                        ymin = 42e5, ymax = 55e5) +
      scale_fill_identity() +
      labs(caption = 'Labor force composition in EU regions. Data by eurostat. Jonas Schöley | github.com/jschoeley/tricolore | twitter: @jschoeley')

    print(eumap)
  })

}

shinyApp(ui, server)

# ggtitle(label = 'Labor force composition by sector in EU NUTS-2 regions.',
#         subtitle = 'Data: eurostat. Source: jschoeley.github.io')
