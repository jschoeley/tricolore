library(shiny)
library(dplyr)
library(ggtern)
library(tricolore)

# UI ----------------------------------------------------------------------

ui <- fluidPage(

  titlePanel(title = 'Tricolore: A flexible color scale for ternary compositions'),

  sidebarLayout(

    # INPUT
    sidebarPanel(width = 3,
                 sliderInput(inputId = 'hue', label = 'Hue', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.2),
                 sliderInput(inputId = 'chroma', label = 'Chroma', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.7),
                 sliderInput(inputId = 'lightness', label = 'Lightness', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.8),
                 sliderInput(inputId = 'contrast', label = 'Contrast', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.4),
                 sliderInput(inputId = 'spread', label = 'Spread',
                             min = 0.5, max = 2, step = 0.1, value = 1, ticks = FALSE),
                 sliderInput(inputId = 'breaks', label = 'Discretization', ticks = FALSE,
                             min = 2, max = 20, step = 1, value = 4),
                 checkboxInput(inputId = 'center', label = 'Mean centering',
                               value = FALSE),
                 checkboxInput(inputId = 'show_center', label = 'Show center',
                               value = FALSE),
                 checkboxInput(inputId = 'show_data', label = 'Show data',
                               value = TRUE),
                 checkboxInput(inputId = 'crop', label = 'Crop legend',
                               value = FALSE),
                 radioButtons(inputId = 'label_as', label = 'Label as',
                              choices = list('percent-share' = 'pct',
                                             'pct-pt. difference' = 'pct_diff'),
                              selected = 'pct')
    ),

    # OUTPUT
    mainPanel(verbatimTextOutput(outputId = 'call'), plotOutput(outputId = 'example'))
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {

  output$call <- renderText({
    paste0(
      "Tricolore(euro_sectors, p1 = 'lf_pri', p2 = 'lf_sec', p3 = 'lf_ter'",
      ', breaks = ', input$breaks,
      ', hue = ', input$hue,
      ', chroma = ', input$chroma,
      ', lightness = ', input$lightness,
      ', contrast = ', input$contrast,
      ', center = ', ifelse(input$center, 'NA', 'rep(1/3,3)'),
      ', spread = ', input$spread,
      ', show_data = ', input$show_data,
      ', show_center = ', input$show_center,
      ', label_as = "', input$label_as, '"',
      ', crop = ', input$crop,
      ', legend = TRUE)'
    )
  })

  output$example <- renderPlot(res = 120, width = 1000, height = 800, {

    # mix color, generate legend
    mixed <- Tricolore(euro_example,
                       p1 = 'lf_pri', p2 = 'lf_sec', p3 = 'lf_ter',
                       breaks = input$breaks,
                       hue = input$hue, chroma = input$chroma,
                       lightness = input$lightness, contrast = input$contrast,
                       center = if (input$center) NA else rep(1/3,3),
                       spread = input$spread,
                       show_data = input$show_data,
                       show_center = input$show_center,
                       label_as = input$label_as,
                       crop = input$crop,
                       legend = TRUE)

    # customize legend
    lgnd <- mixed[['legend']] +
      labs(x = 'Primary', y = 'Secondary', z = 'Tertiary',
           caption = paste0('Labor force composition in European regions 2016\n',
                            ifelse(input$center,
                                   'Colors show deviation from average composition',
                                   'Colors show deviations from balanced composition'))) +
      theme(plot.background = element_rect(fill = 'grey95', color = 'grey50'))

    # merge data and map
    euro_example$rgb <- mixed[['hexsrgb']]

    # generate map
    euro_map <-
      euro_basemap +
      geom_sf(aes(fill = rgb), color = NA,
              data = euro_example) +
      annotation_custom(ggplotGrob(lgnd),
                        xmin = 54e5, xmax = 74e5,
                        ymin = 8e5, ymax = 80e5) +
      scale_fill_identity() +
      coord_sf(expand = FALSE, datum = NA) +
      labs(caption = 'Data by eurostat. Jonas Schöley | github.com/jschoeley/tricolore | twitter: @jschoeley')

    print(euro_map)
  })

}

shinyApp(ui, server)
