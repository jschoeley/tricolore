library(shiny)
library(sf)
library(ggtern)
library(tricolore)

# UI ----------------------------------------------------------------------

ui <- fluidPage(

  titlePanel(title = 'Tricolore: A flexible color scale for ternary compositions'),

  sidebarLayout(

    # INPUT
    sidebarPanel(width = 3,
                 radioButtons(inputId = 'data', label = 'Data', inline = TRUE,
                              choices = list('Labour force' = 'lf',
                                             'Education' = 'educ'),
                              selected = 'educ'),
                 radioButtons(inputId = 'type', label = 'Type', inline = TRUE,
                              choices = list('Default' = 'tricolore',
                                             'Sextant' = 'sextant'),
                              selected = 'tricolore'),
                 conditionalPanel(
                   condition = 'input.type == "tricolore"',
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
                 checkboxInput(inputId = 'discrete', label = 'Discrete', value = FALSE),
                 conditionalPanel(
                   condition = 'input.discrete',
                   sliderInput(inputId = 'breaks', label = 'Breaks', ticks = FALSE,
                               min = 2, max = 20, step = 1, value = 4)
                 )),
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
                              selected = 'pct'),
                 verbatimTextOutput(outputId = 'call')
    ),

    # OUTPUT
    mainPanel(plotOutput(outputId = 'example'))
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {

  output$call <- renderText({
    paste0(
      if (input$type == 'tricolore') 'Tricolore(',
      if (input$type == 'sextant') 'TricoloreSextant(',
      "euro_example, ",
      if (input$data == 'educ') "p1 = 'ed_0to2', p2 = 'ed_3to4', p3 = 'ed_5to8'",
      if (input$data == 'lf') "p1 = 'lf_pri', p2 = 'lf_sec', p3 = 'lf_ter'",
      ', center = ', ifelse(input$center, 'NA', 'rep(1/3,3)'),
      if (input$type == 'tricolore') {
        paste0(
          ', breaks = ', ifelse(input$discrete, input$breaks, 'Inf'),
          ', hue = ', input$hue,
          ', chroma = ', input$chroma,
          ', lightness = ', input$lightness,
          ', contrast = ', input$contrast,
          ', spread = ', input$spread
        )
      },
      ', legend = TRUE',
      ', show_data = ', input$show_data,
      ', show_center = ', input$show_center,
      ', label_as = "', input$label_as, '"',
      ', crop = ', input$crop, ')'
    )
  })

  output$example <- renderPlot(res = 120, width = 1000, height = 800, {

    if (input$data == 'educ') {
      p1 = 'ed_0to2'; p2 = 'ed_3to4'; p3 = 'ed_5to8'
      title = 'Compos. of education lvls in European regions 2016\n'
    }
    if (input$data == 'lf') {
      p1 = 'lf_pri'; p2 = 'lf_sec'; p3 = 'lf_ter'
      title = 'Labor force composition in European regions 2016\n'
    }

    if (input$type == 'tricolore') {

      # mix color, generate legend
      mixed <- Tricolore(euro_example,
                         p1 = p1, p2 = p2, p3 = p3,
                         center = if (input$center) NA else rep(1/3,3),
                         breaks = ifelse(input$discrete, input$breaks, Inf),
                         hue = input$hue, chroma = input$chroma,
                         lightness = input$lightness,
                         contrast = input$contrast,
                         spread = input$spread,
                         show_data = input$show_data,
                         show_center = input$show_center,
                         label_as = input$label_as,
                         crop = input$crop,
                         legend = TRUE)

    }

    if (input$type == 'sextant') {

      # mix color, generate legend
      mixed <- TricoloreSextant(euro_example,
                                p1 = p1, p2 = p2, p3 = p3,
                                center = if (input$center) NA else rep(1/3,3),
                                show_data = input$show_data,
                                show_center = input$show_center,
                                label_as = input$label_as,
                                crop = input$crop,
                                legend = TRUE)

    }

    # customize legend
    lgnd <- mixed[['key']] +
      labs(x = 'Primary', y = 'Secondary', z = 'Tertiary',
           caption = paste0(title,
                            ifelse(input$center,
                                   'Colors show deviation from average composition',
                                   'Colors show deviations from balanced composition'))) +
      theme(plot.background = element_rect(fill = 'grey95', color = 'grey50'))

    # merge data and map
    euro_example$rgb <- mixed[['rgb']]

    # generate map
    euro_map <-
      euro_basemap +
      geom_sf(aes(fill = rgb, geometry = geometry), color = NA,
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
