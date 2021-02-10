UI_explore <- tabPanel(
  title = "Explore",
  tabsetPanel(
    id = 'explore_tabs',
    type = 'tabs',
    tabPanel(
      title = "Transition matrix",
      h3("Transition between activities, regardless of period"),
      h5("The outside arc (node) represents the frequency of the activity"),
      h5("The connection between the nodes is the transition rates between the two activities"),
      br(), br(),
      chorddiagOutput(outputId = 'explore_plot_chord',
                      height = 700)
      )
  )
  # )
  # sidebarLayout(
  #   sidebarPanel(
  #     width = 0,
  #     # sliderInput(inputId = "bar_max", 
  #     #             label = "Number of edges",
  #     #             min = 2, max = 48, value = 10, step = 1)
  #   ),
  #   mainPanel(
  #     width = 12,
  #     # d3Output(outputId = "explore_d3_chord",
  #     #          height = 600)
  #     h3("Transition matrix of activities"),
  #     chorddiagOutput(outputId = 'explore_plot_chord',
  #                     height = 600)
  #   )
  # )
)