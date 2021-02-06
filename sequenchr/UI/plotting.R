UI_plotting <- tabPanel(
  title = "Plotting",
  sidebarLayout(
    sidebarPanel(
      width = 6,
      NULL
    ),
    mainPanel(
      width = 6,
      tabsetPanel(
        id = 'plotting_tabs',
        type = 'tabs',
        tabPanel(
          title = 'Sequence plot',
          plotOutput('plotting_plot_sequence')
          )
        )
    )
  )
)