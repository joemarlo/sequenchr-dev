UI_welcome <- tabPanel(
  title = "Welcome",
  sidebarLayout(
    sidebarPanel(
      width = 6,
      includeMarkdown("markdowns/welcome.md")),
    mainPanel(
      width = 6,
      br(),
      tableOutput('summary_table')
      )
    )
  )