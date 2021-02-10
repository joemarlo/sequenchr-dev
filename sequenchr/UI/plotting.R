UI_plotting <- tabPanel(
  title = "Descriptive",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Data summary"),
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
                    choices = 'ward.D2',
                    selected = 'ward.D2'),
        actionButton(inputId = 'clustering_button_cluster',
                     label = 'Cluster the data'),
        br(),br(),
        sliderInput(inputId = 'clustering_slider_n_clusters',
                    label = 'Number of clusters',
                    min = 2,
                    max = 10,
                    step = 1,
                    value = 1),
        br(),
        actionButton(inputId = 'clustering_button_silhouette',
                     label = 'Calculate silhouette width'),
        br(), br(),
        actionButton(inputId = 'clustering_button_download',
                     label = 'Download cluster assignments')
      )
    ),
    mainPanel(
      width = 9,
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