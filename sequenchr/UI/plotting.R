UI_plotting <- tabPanel(
  title = "Descriptive",
  sidebarLayout(
    sidebarPanel(
      width = 4,
      htmlOutput(outputId = 'summary_table')
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        id = 'plotting_tabs',
        type = 'tabs',
        tabPanel(
          title = 'Sequence plot',
          br(),
          plotOutput(outputId = 'plotting_plot_sequence',
                     height = 600)
          ),
        tabPanel(
          title = 'Top 10 most common',
          br(),
          plotOutput(outputId = 'plotting_plot_common',
                     height = 600)
        ),
        tabPanel(
          title = 'State distribution',
          br(),
          plotOutput(outputId = 'plotting_plot_state',
                     height = 600)
        ),
        tabPanel(
          title = 'Modal activities',
          br(),
          plotOutput(outputId = 'plotting_plot_modal',
                     height = 600)
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