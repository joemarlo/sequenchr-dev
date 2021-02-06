UI_clustering <- tabPanel(
  title = "Clustering",
  sidebarLayout(
    sidebarPanel(
      width = 6,
      NULL
    ),
    mainPanel(
      width = 6,
      tabsetPanel(
        id = 'clustering_tabs',
        type = 'tabs'
        # tabPanel(
        #   title = 'Sequence plot',
        #   plotOutput('plotting_plot_sequence')
        # )
      )
    )
  )
)