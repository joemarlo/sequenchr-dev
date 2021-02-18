UI <- fluidPage(
    # download roboto font
  HTML('<link rel="preconnect" href="https://fonts.gstatic.com">'),
  HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
  HTML('<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Libre+Barcode+128+Text&display=swap">'),

  # load custom CSS file
  includeCSS(file.path("www", "custom_css.css")),
  # includeCSS(file.path("www", "d3_css.css")),
  
  # set top left title
  titlePanel(
    title = "",
    windowTitle = "sequenchr"
  ),
  
  # main UI
  sidebarLayout(
    sidebarPanel(
      width = 3,
      HTML("<h2 id='logo' style='font-family: \"Libre Barcode 128 Text\", cursive; font-weight:500; color: #8c8c8c; font-size: 5rem;'>sequenchr</h2>"),
      br(),
      h4("Data summary"),
      htmlOutput(outputId = 'summary_table'),
      checkboxInput(inputId = 'plotting_check_cluster',
                    label = 'Cluster the data?'),
      conditionalPanel(
        condition = 'input.plotting_check_cluster == true',
        selectInput(inputId = 'clustering_select_distance_method',
                    label = 'Distance method',
                    choices = 'TRATE',
                    selected = 'TRATE'),
        selectInput(inputId = 'clustering_select_clustering_method',
                    label = 'Clustering method',
                    choices = c('ward.D2', 'complete', 'average'),
                    selected = 'ward.D2'),
        actionButton(inputId = 'clustering_button_cluster',
                     label = 'Cluster the data'),
        uiOutput(outputId = 'clustering_UI')
      ),
      br(), br(),
      HTML('<details><summary>Additional settings</summary>'),
      # br(),
      # sliderInput(inputId = 'slider_sample_n',
      #             label = 'Randomly sample the data. Select n',
      #             min = 100,
      #             max = 1000, #nrow(sequence_data),
      #             step = 100,
      #             value = 1000), #nrow(sequence_data)),
      # actionButton(inputId = 'button_sample',
      #              label = 'Sample'),
      br(),
      sliderInput(inputId = 'clustering_slider_facet_ncol',
                  label = 'Number of columns for plot facets',
                  min = 1,
                  max = 20,
                  value = 1,
                  ticks = FALSE),
      sliderInput(inputId = 'clustering_slider_dendrogram_depth',
                  label = 'Depth to plot dendrogram. Deeper = slower peformance',
                  min = 0,
                  max = 500,
                  step = 5,
                  value = 50,
                  ticks = FALSE),
      sliderInput(inputId = 'clustering_slider_silhouette_range',
                  label = 'n clusters to calculate silhouette width at',
                  min = 2,
                  max = 30,
                  step = 1,
                  value = c(2, 10),
                  ticks = FALSE),
      br(),
      h4("Citations"),
      p("....lorem ipsum..."),
      br(),
      uiOutput(outputId = 'clustering_button_UI'),
      HTML('</details><br>')
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = 'plotting_tabs',
        type = 'tabs',
        tabPanel(
          title = 'Sequence index plot',
          br(),
          plotOutput(outputId = 'plotting_plot_sequence',
                     height = 750)
        ),
        tabPanel(
          title = 'Top 10',
          br(),
          plotOutput(outputId = 'plotting_plot_common',
                     height = 750)
        ),
        tabPanel(
          title = 'State distribution',
          br(),
          plotOutput(outputId = 'plotting_plot_state',
                     height = 750)
        ),
        tabPanel(
          title = 'Modal activities',
          br(),
          plotOutput(outputId = 'plotting_plot_modal',
                     height = 750)
        ),
        tabPanel(
          title = "Transition matrix",
          h3("Transition between activities, regardless of period"),
          h5("The outside arc (node) represents the frequency of the activity"),
          h5("The connection between the nodes is the transition rates between the two activities"),
          br(), br(),
          chorddiag::chorddiagOutput(
            outputId = 'explore_plot_chord',
            height = 700
            )
        ),
        tabPanel(
          title = 'Legend',
          br(),
          plotOutput(outputId = 'plotting_plot_legend',
                     height = 400,
                     width = 250)
        )
      )
    )
  )
)
