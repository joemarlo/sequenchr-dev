UI_explore <- tabPanel(
  title = "Explore",
  sidebarLayout(
    sidebarPanel(
      width = 4,
      sliderInput("bar_max", label = "Max:",
                  min = 0.1, max = 1.0, value = 0.2, step = 0.1)
    ),
    mainPanel(
      width = 8,
      d3Output("d3")
    )
  )
)