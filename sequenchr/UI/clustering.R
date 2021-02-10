UI_clustering <- tabPanel(
  title = "Clustering",
  sidebarLayout(
    sidebarPanel(
      width = 4,
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
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        id = 'clustering_tabs',
        type = 'tabs',
        # tabPanel(
        #   title = 'Dendrogram',
        #   br(),
        #   plotOutput(outputId = 'clustering_plot_dendrogram',
        #              height = 500)
        # ),
        # tabPanel(
        #   title = 'Silhouette plot',
        #   br(),
        #   plotOutput(outputId = 'clustering_plot_silhouette',
        #              height = 500)
        # ),
        tabPanel(
          title = 'Sequence plot by cluster',
          br(),
          plotOutput(outputId = 'clustering_plot_sequence',
                     height = 500)
        )
      )
    )
  )
)