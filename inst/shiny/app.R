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
                 radioButtons(inputId = 'show_center', label = 'Show center',
                              choices = list(No = 'No', Yes = 'Yes'),
                              selected = 'No'),
                 radioButtons(inputId = 'show_data', label = 'Show data',
                              choices = list(No = 'No', Yes = 'Yes'),
                              selected = 'Yes'),
                 radioButtons(inputId = 'label_as', label = 'Label as',
                              choices = list('percent-share' = 'pct',
                                             'percent-point-difference\nfrom center point' = 'pct_diff'),
                              selected = 'pct'),
                 radioButtons(inputId = 'crop', label = 'Crop legend',
                              choices = list(No = 'No', Yes = 'Yes'),
                              selected = 'No')
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
      ', center = ', switch(input$center, No = 'rep(1/3,3)', Yes = 'NA'),
      ', spread = ', input$spread,
      ', show_data = ', switch(input$show_data, No = FALSE, Yes = TRUE),
      ', show_center = ', switch(input$show_center, No = FALSE, Yes = TRUE),
      ', label_as = "', input$label_as, '"',
      ', crop = ', switch(input$show_center, No = FALSE, Yes = TRUE),
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
                       center = switch(input$center, No = rep(1/3,3), Yes = NA),
                       spread = input$spread,
                       show_data = switch(input$show_data, No = FALSE, Yes = TRUE),
                       show_center = switch(input$show_center, No = FALSE, Yes = TRUE),
                       label_as = input$label_as,
                       crop = switch(input$crop, No = FALSE, Yes = TRUE),
                       legend = TRUE)

    # customize legend
    lgnd <- mixed[['legend']] +
      labs(x = 'Primary', y = 'Secondary', z = 'Tertiary',
           caption = paste0('Labor force composition in European regions 2016\n',
                            switch(input$center, No = 'Colors show deviations from balanced composition',
                                   input$center, Yes = 'Colors show deviation from average composition'))) +
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
