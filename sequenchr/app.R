library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(viridis)
# library(TraMineR)
# library(dendextend)
# library(chorddiag)
theme_set(theme_minimal(base_size = 16))

# this shouldn't be necessary but is currently required
setwd("sequenchr")

# load UI
# map(list.files('UI'), function(file) source(file.path("UI", file))) # TODO: is there a do this w/o global vars?
source('main_ui.R', local = TRUE)

# set default object if one doesn't exist
# TODO: change this to stop if input doesn't exist
appDir <- getwd()
# sequence_data_default <- atus_seq #mtcars
sequence_data <- getShinyOption("sequence_data") #sequence_data_default)
covariates_data <- getShinyOption("covariates_data") 


# functions ---------------------------------------------------------------

cluster_stats <- function(dist_matrix, cluster_model, k_min, k_max){
  all_stats <- lapply(k_min:k_max, function(k){
    c_stats <- fpc::cluster.stats(
      d = dist_matrix,
      clustering = stats::cutree(cluster_model, k = k),
      silhouette = TRUE
    )
    return(dplyr::tibble(k = k, ch = c_stats$ch, silhouette = c_stats$avg.silwidth))
  })
  
  all_stats <- dplyr::bind_rows(all_stats)
  scale_01 <- function(x) (x - min(x)) / diff(range(x))
  all_stats$ch_norm <- scale_01(all_stats$ch)
  all_stats$silhouette_norm <- scale_01(all_stats$silhouette)
  
  return(all_stats)
}


# app ---------------------------------------------------------------------

shinyApp(
    ui = UI,
    server = function(input, output, session){
        
        # set and reset working directory
        oldwd <- setwd(appDir)
        on.exit(setwd(oldwd))
        on.exit(setwd(".."))
        
        # initialize store
        store <- reactiveValues()
        
        # establish color mapping
        color_mapping <- viridis::viridis_pal()(length(alphabet(sequence_data)))
        names(color_mapping) <- alphabet(sequence_data)
        store$color_mapping <- color_mapping
        
        # tidy the data
        store$tidy_data <- sequence_data %>%
            as_tibble() %>% 
            setNames(1:ncol(sequence_data)) %>% 
            mutate(sequenchr_seq_id = row_number()) %>%
            pivot_longer(cols = setdiff(colnames(.), "sequenchr_seq_id")) %>% 
            mutate(period = as.numeric(name)) %>% 
            dplyr::select(-name)
        if (!is.null(covariates_data)){
          store$tidy_cov_data <- covariates_data %>% 
            as_tibble() %>% 
            mutate(sequenchr_seq_id = row_number()) %>% 
            pivot_longer(cols = -sequenchr_seq_id)
        } else store$tidy_cov_data <- NULL
        
        # summary table
        output$summary_table <- renderText({
            data.frame(
                n_sequences = nrow(sequence_data),
                n_unique_sequences = nrow(distinct(as.data.frame(sequence_data))),
                n_periods = ncol(sequence_data)
            ) %>% 
                t() %>% 
                `rownames<-`(c('n sequences', 'n unique sequences', 'n periods')) %>% 
                knitr::kable(digits = 2, format = 'html') %>% 
                kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"))
        })
        

        # plotting ----------------------------------------------------------------

        # render the sequence index plot
        output$plotting_plot_sequence <- renderPlot({
          
          if (isFALSE(input$plotting_check_cluster)){
            p <- plot_sequence_index(seq_def_tidy = store$tidy_data,
                                     color_mapping = store$color_mapping)
          } else {
            p <- plot_sequence_index(
              seq_def_tidy = store$tidy_data,
              color_mapping = store$color_mapping,
              cluster_assignments = cluster_assignments(),
              n_col_facets = input$clustering_slider_facet_ncol
            )
          }
            
          return(p)
        })
        
        # render the top 10 most common sequences
        # TODO: should show frequency of sequence somehow
        output$plotting_plot_common <- renderPlot({

            if (isFALSE(input$plotting_check_cluster)){
            
                # plot without clustering
                p <- store$tidy_data %>% 
                    group_by(sequenchr_seq_id) %>% 
                    summarize(seq_collapsed = paste0(value, collapse = 'SE3P'),
                              .groups = 'drop') %>% 
                    count(seq_collapsed) %>% 
                    arrange(desc(n)) %>%
                    slice_head(n = 10) %>% 
                    separate(seq_collapsed, into = paste0('p', 1:ncol(sequence_data)), sep = "SE3P") %>% 
                    mutate(sequenchr_seq_id = row_number()) %>%
                    pivot_longer(cols = setdiff(colnames(.), c('n', "sequenchr_seq_id"))) %>% 
                    mutate(name = as.numeric(gsub('p', '', name))) %>% 
                    rename(period = name) %>% 
                    ggplot(aes(x = period, y = sequenchr_seq_id, fill = value)) +
                    geom_tile() +
                    scale_fill_manual(values = color_mapping) +
                    scale_y_discrete(labels = NULL, breaks = NULL) +
                    labs(title = "Top 10 most common sequences",
                         x = 'Period',
                         y = 'Sequence (ranked by count)',
                         fill = NULL)
            } else {
                
                # plot with clustering
                p <- store$tidy_data %>%
                    left_join(data.frame(cluster = factor(sub("  \\|.*", "", cluster_assignments()),
                                                          levels = paste0('Cluster ', 1:length(cluster_assignments()))), 
                                         sequenchr_seq_id = 1:length(cluster_assignments())),
                              by = "sequenchr_seq_id") %>% 
                    group_by(sequenchr_seq_id, cluster) %>% 
                    summarize(seq_collapsed = paste0(value, collapse = 'SE3P'),
                              .groups = 'drop') %>% 
                    count(cluster, seq_collapsed) %>%
                    group_by(cluster) %>% 
                    arrange(desc(n)) %>%
                    slice_head(n = 10) %>% 
                    ungroup() %>% 
                    separate(seq_collapsed, into = paste0('p', 1:ncol(sequence_data)), sep = "SE3P") %>% 
                    mutate(id = row_number()) %>%
                    pivot_longer(cols = setdiff(colnames(.), c('n', "id", 'cluster'))) %>% 
                    mutate(name = as.numeric(gsub('p', '', name))) %>% 
                    rename(period = name) %>%
                    ggplot(aes(x = period, y = id, fill = value)) +
                    geom_tile() +
                    scale_fill_manual(values = color_mapping) +
                    scale_y_discrete(labels = NULL, breaks = NULL) +
                    facet_wrap(~cluster, scales = 'free_y', ncol = input$clustering_slider_facet_ncol) +
                    labs(title = "Top 10 most common sequences by cluster",
                         x = 'Period',
                         y = 'Sequence (ranked by count)',
                         fill = NULL)

            }
            
            return(p)
        }) 
        
        # plot of just the legend colors
        output$plotting_plot_legend <- renderPlot({
          
          # plot it
          p <- plot_legend(color_mapping = store$color_mapping)
          
          return(p)
        })
        
        # state distribution plot
        output$plotting_plot_state <- renderPlot({
            
          if (isFALSE(input$plotting_check_cluster)){
            p <- plot_state(seq_def_tidy = store$tidy_data,
                            color_mapping = store$color_mapping)
          } else {
            p <- plot_state(
              seq_def_tidy = store$tidy_data,
              color_mapping = store$color_mapping,
              cluster_assignments = cluster_assignments(),
              n_col_facets = input$clustering_slider_facet_ncol
            )
          }
          
          return(p)
        })
        
        # modal plot
        output$plotting_plot_modal <- renderPlot({
            
          if (isFALSE(input$plotting_check_cluster)){
            p <- plot_modal(seq_def_tidy = store$tidy_data,
                            color_mapping = store$color_mapping)
          } else {
            p <- plot_modal(
              seq_def_tidy = store$tidy_data,
              color_mapping = store$color_mapping,
              cluster_assignments = cluster_assignments(),
              n_col_facets = input$clustering_slider_facet_ncol
            )
          }
          
          return(p)
        })
        
        # covariates plot
        output$plotting_plot_covariates <- renderPlot({
          
          validate(need(is(store$tidy_cov_data, 'data.frame'),
                        'Covariates data not provided'))
          
          if (isFALSE(input$plotting_check_cluster)){
            
            # plot without clustering
            p <- store$tidy_cov_data %>% 
              ggplot(aes(x = value, group = name)) + 
              geom_density()
            
          } else {
            # plot with cluster
            p <- dplyr::tibble(cluster = cluster_assignments(),
                               sequenchr_seq_id = 1:length(cluster_assignments())) %>%
              right_join(store$tidy_cov_data, by = 'sequenchr_seq_id') %>%
              ggplot(aes(x = value, group = name)) +
              geom_density() +
              facet_wrap( ~ cluster,
                          scales = 'free_y',
                          ncol = input$clustering_slider_facet_ncol)
          }
          
          return(p)
        })
        

    # clustering --------------------------------------------------------------

        # cluster the data
        observeEvent(input$clustering_button_cluster, {
          
            # compute optimal matching distances
            store$dist_matrix <- TraMineR::seqdist(
                seqdata = sequence_data,
                method = "OM",
                indel = 1,
                sm = "TRATE"
            )
            # dist_om_DHD <- seqdist(atus_seq, method = "DHD")
            
            # cluster the data
            store$cluster <- fastcluster::hclust(
                d = as.dist(store$dist_matrix), 
                method = input$clustering_select_clustering_method
                )
            
            # remove and add dendrogram tab
            removeTab(inputId = 'plotting_tabs',
                      target = 'Dendrogram')
            insertTab(
                inputId = 'plotting_tabs',
                target = 'Legend',
                position = 'after',
                select = TRUE,
                tab = tabPanel(
                    title = 'Dendrogram',
                    br(),
                    plotOutput(outputId = 'clustering_plot_dendrogram',
                               height = 600)
                )
            )
            
            # render the clustering UI
            output$clustering_UI <- renderUI({
                tagList(
                    br(),
                    sliderInput(inputId = 'clustering_slider_n_clusters',
                                label = 'Number of clusters',
                                min = 2,
                                max = 20,
                                step = 1,
                                value = 1,
                                ticks = FALSE),
                    br(),
                    actionButton(inputId = 'clustering_button_separation',
                                 label = 'Calculate separation metrics')
                )
            })
            
            # add the download button
            output$clustering_button_UI <- renderUI({
                downloadButton(outputId = 'clustering_button_download',
                               label = 'Download cluster assignments')
            })
        })
        
        # returns the current cluster assignments
        cluster_assignments <- reactive({
            
            # stop here if clustering hasn't been run yet
            validate(need(is(store$cluster, 'hclust'),
                          'Cluster the data first'))
            
            # get the cluster assignments
            hcl_k <- input$clustering_slider_n_clusters
            cluster_assignments <- cutree(store$cluster, k = hcl_k)
            
            # reorder clusters to match dendrogram left to right
            cluster_to_dend_mapping <- dplyr::tibble(cluster = cluster_assignments[store$cluster$order]) %>% 
                nest(-cluster) %>% 
                mutate(cluster_dend = row_number()) %>% 
                unnest(data) %>% 
                distinct()
            cluster_sorted <- dplyr::tibble(cluster = cluster_assignments) %>% 
                left_join(cluster_to_dend_mapping, by = 'cluster') %>% 
                pull(cluster_dend)
            
            # add label
            cluster_ns <- table(cluster_sorted)
            cluster_names <- factor(
                cluster_sorted,
                labels = paste("Cluster", 1:hcl_k, " | n = ", cluster_ns)
            )
            
            return(cluster_names)
        })

        # plot the dendrogram
        output$clustering_plot_dendrogram <- renderPlot({
            
            # stop here if clustering hasn't been run yet
            validate(need(is(store$cluster, 'hclust'),
                          'Cluster the data first'))
            
            # retrieve the current cluster model and k cluster value
            cluster <- store$cluster
            k <- input$clustering_slider_n_clusters
            h <- input$clustering_slider_dendrogram_depth
            
            # plot it
            p <- plot_dendrogram(cluster, k, h)
            
            return(p)
        })
        
        # compute and plot silhouette width
        observeEvent(input$clustering_button_separation, {
            # get optimal cluster sizes by calculating silhouette width
            # store$s_width <- NbClust::NbClust(
            #     data = NULL,
            #     diss = as.dist(store$dist_matrix),
            #     distance = NULL,
            #     method = 'ward.D2',
            #     max.nc = input$clustering_slider_separation_range[2],
            #     min.nc = input$clustering_slider_separation_range[1],
            #     index = 'silhouette'
            # )
            
            store$separation_metrics <- cluster_stats(
                dist_matrix = as.dist(store$dist_matrix),
                cluster_model = store$cluster,
                k_min = input$clustering_slider_separation_range[1],
                k_max = input$clustering_slider_separation_range[2]
            )
            
            # update slider with best k value
            # updateSelectInput(session = session,
            #                   inputId = 'clustering_slider_n_clusters',
            #                   selected = store$s_width$Best.nc[['Number_clusters']])
            
            # remove and add silhouette plot tab
            removeTab(inputId = 'plotting_tabs',
                      target = 'Separation plot')
            insertTab(
                inputId = 'plotting_tabs',
                target = 'Dendrogram',
                position = 'after',
                select = TRUE,
                tab = tabPanel(
                    title = 'Separation plot',
                    br(),
                    plotOutput(outputId = 'clustering_plot_separation',
                               height = 600)
                )
            )
        })
        
        # plot the silhouette width
        output$clustering_plot_separation <- renderPlot({
            
            # stop here if silhouette width hasn't been run yet
            validate(need(is(store$separation_metrics, 'data.frame'),
                          'Calculate the silhouette width first'))
            
            # enframe
            # widths <- as.data.frame(store$s_width$All.index)
            # widths$name <- rownames(widths)
            # 
            # # plot it
            # p <- widths %>% 
            #     rename(value = `store$s_width$All.index`) %>% 
            #     mutate(name = as.numeric(name)) %>% 
            #     ggplot(aes(x = name, y = value)) +
            #     geom_line(color = 'grey30') +
            #     geom_area(alpha = 0.4) +
            #     geom_point(color = 'grey30') +
            #     labs(title = "Silhouette width",
            #          subtitle = 'Greater width is better',
            #          x = 'n clusters',
            #          y = 'Silhouette width') 
            
            p <- store$separation_metrics %>% 
              rename(`CH index` = ch_norm,
                     `Silhouette width` = silhouette_norm) %>% 
              pivot_longer(cols = c("CH index", "Silhouette width")) %>% 
              ggplot(aes(x = k, y = value, group = name, color = name)) +
              geom_line(size = 1.2) +
              labs(title = "Cluster seperation measured by Calinski-Harabasz index and silhouette width",
                   subtitle = 'Optimal clusters: minimum CH, maximum silhouette width',
                   x = 'n clusters',
                   y = 'Normalized index',
                   color = NULL) +
              theme(legend.position = 'bottom')
            
            return(p)
        })
        
        # download the clusters
        output$clustering_button_download <- downloadHandler(
            
            # use plot title as file name but only retain alpha-numeric characters
            filename <- function() {
                time <- gsub("-|:| ", "", Sys.time())
                paste0(time, '_cluster_assignments.csv')
            }, 
            
            # dataframe of clusters to download
            content <- function(file) {
                cluster_assignments() %>% 
                    as.data.frame() %>% 
                    mutate(row = row_number(),
                           cluster = sub("  \\|.*", "", `.`)) %>% 
                    select(-`.`) %>% 
                write.csv(., file, row.names = FALSE)
            }
        )
        
        # update the max number of rows to plot based on the number of clusters
        observeEvent(input$clustering_slider_n_clusters, {
            updateSliderInput(session = session,
                              inputId = 'clustering_slider_facet_ncol',
                              max = input$clustering_slider_n_clusters)
        })

        

    # chord plot --------------------------------------------------------------

        # # render the d3 plot
        # output$explore_d3_chord <- renderD3({
        #     # r2d3(data = runif(5, 0, input$bar_max),
        #     #      script = file.path('d3_plots', 'chord.js')
        #     r2d3(data = matrix(round(runif(input$bar_max, 1, 10000)), 
        #                        ncol = 4, nrow = 4), 
        #          script = file.path('d3_plots', 'chord.js'))
        # })
        
        # transition matrix
        transition_matrix <- reactive({
            
            # filter the data to the periods specfiied by the input slider
            # add NA filler rows after each group before calculating transition matrix
            # this prevents end of day looping back to beginning of day for next group
            freq_data <- store$tidy_data %>% 
                filter(period >= input$plotting_slider_chord[1],
                       period <= input$plotting_slider_chord[2]) %>% 
                mutate(value = as.character(value)) %>% 
                group_by(sequenchr_seq_id) %>% 
                group_split() %>% 
                lapply(X = ., FUN = function(df){
                    df %>% add_row(sequenchr_seq_id = NA, value = NA, period = NA)
                }) %>% 
                bind_rows()
            
            # calculate transition matrix
            n <- nrow(freq_data)  
            TRATE_mat <- table(data.frame(previous = freq_data$value[1:(n-1)],
                                          current = freq_data$value[2:n]))
            TRATE_mat <- TRATE_mat / sum(TRATE_mat)
            
            # ensure matrix contains all the states (b/c above filters may remove some)
            unique_states <- unique(store$tidy_data$value) %>% as.vector()
            TRATE_filled <- crossing(previous = unique_states, current = unique_states) %>% 
                left_join(as_tibble(TRATE_mat),
                          by = c('previous', 'current')) %>% 
                replace_na(list(n = 0)) %>% 
                pivot_wider(names_from = previous, values_from = n)
            TRATE_filled_mat <- as.matrix(TRATE_filled[, -1])
            rownames(TRATE_filled_mat) <- TRATE_filled[[1]]
            
            return(list(TRATE_filled, TRATE_filled_mat))
        })
        
        # render the chord plot
        output$explore_plot_chord <- chorddiag::renderChorddiag({
            
            # get the transition matrix
            trans_mat <- transition_matrix()
            freq_data <- trans_mat[[1]]
            TRATE_mat <- trans_mat[[2]]
            
            # create the color vector
            states_included <- intersect(names(store$color_mapping), rownames(TRATE_mat)) #unique(freq_data$value))
            colors_chord <- as.vector(store$color_mapping[states_included])
            
            # plot the chord diagram
            p <- chorddiag::chorddiag(
                data = TRATE_mat,
                groupColors = colors_chord,
                groupnamePadding = 20,
                groupnameFontsize = 12,
                precision = 4
            )
            
            return(p)
        })
        
        # render the transition plot
        output$explore_plot_matrix <- renderPlot({
            
            # get the transition matrix
            TRATE_mat <- transition_matrix()[[1]]
            
            # plot it
            p <- plot_transition_matrix(TRATE_mat)
                
            return(p)
        })
        
        # on load, update the slider with the correct number of periods based on the data
        updateSliderInput(session = session,
                          inputId = 'plotting_slider_chord',
                          max = ncol(sequence_data),
                          value = c(1, ncol(sequence_data)))
        
    }
)
