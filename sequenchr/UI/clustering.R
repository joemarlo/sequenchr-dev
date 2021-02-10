UI_clustering <- tabPanel(
  title = "Clustering",
  sidebarLayout(
    sidebarPanel(
      width = 4,
      actionButton(inputId = 'clustering_button_cluster',
                   label = 'Cluster the data'),
      br(),br(),
      sliderInput(inputId = 'clustering_slider_n_clusters',
                  label = 'Number of clusters',
                  min = 1,
                  max = 30,
                  step = 1,
                  value = 1),
      br(),
      actionButton(inputId = 'clustering_button_silhouette',
                   label = 'Calculate silhouette width')
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        id = 'clustering_tabs',
        type = 'tabs',
        tabPanel(
          title = 'Dendrogram',
          br(),
          plotOutput(outputId = 'clustering_plot_dendrogram',
                     height = 500)
        ),
        tabPanel(
          title = 'Silhouette plot',
          br(),
          plotOutput(outputId = 'clustering_plot_silhouette',
                     height = 500)
        )
      )
    )
  )
)