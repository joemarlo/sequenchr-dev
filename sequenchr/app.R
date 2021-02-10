require(tidyverse)
# require(DT)
# require(shinyBS)
require(shiny)
# library(shinythemes)
library(shinyWidgets) # for slider skin
library(arm) # currently just for lalonde dataset
library(viridis) # for color blind sensitive colors
library(TraMineR)
library(dendextend) # for dendroram
# library(r2d3) # for explore
library(chorddiag) # for chord plot
theme_set(theme_minimal())

# this shouldn't be necessary but is currently required
setwd("sequenchr")

# load UI
map(list.files('UI'), function(file) source(file.path("UI", file))) # TODO: is there a do this w/o global vars?
source('main_ui.R', local = TRUE)

# set default object if one doesn't exist
# TODO: change this to stop if input doesn't exist
appDir <- getwd()
sequence_data_default <- atus_seq #mtcars
sequence_data <- getShinyOption("sequence_data", sequence_data_default)


shinyApp(
    ui = main_UI,
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
        
        # summary table
        output$summary_table <- renderText({
            data.frame(
                'n sequences' = nrow(sequence_data),
                'n unique sequences' = nrow(distinct(as.data.frame(sequence_data)))
            ) %>% 
                t() %>% 
                knitr::kable(digits = 2, format = 'html') %>% 
                kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"))
        })
        

        # plotting ----------------------------------------------------------------

        # render the sequence plot
        output$plotting_plot_sequence <- renderPlot({
            
            if (isFALSE(input$plotting_check_cluster)){
                
                # plot the regular sequences without clustering
                p <- store$tidy_data %>% 
                    group_by(sequenchr_seq_id) %>% 
                    mutate(entropy = DescTools::Entropy(table(value))) %>%
                    ungroup() %>% 
                    ggplot(aes(x = period, y = reorder(sequenchr_seq_id, entropy), fill = value)) +
                    geom_tile() +
                    scale_fill_manual(values = color_mapping) +
                    scale_y_discrete(labels = NULL) +
                    labs(title = "All sequences sorted by entropy",
                         x = 'Period',
                         y = 'Sequence',
                         fill = NULL)
            } else {
                # stop here if clustering hasn't been run yet
                validate(need(is(store$cluster, 'hclust'),
                              'Cluster the data first'))
                
                # get the cluster assignments and clean up
                hcl_k <- input$clustering_slider_n_clusters
                cluster_assignments <- cutree(store$cluster, k = hcl_k)
                cluster_ns <- table(cluster_assignments)
                cluster_assignments <- factor(
                    cluster_assignments, 
                    labels = paste("Cluster", 1:hcl_k, " | n = ", cluster_ns)
                )
                
                # plot the sequences with clusters
                p <- tibble(cluster = cluster_assignments,
                            sequenchr_seq_id = 1:length(cluster_assignments)) %>% 
                    right_join(store$tidy_data, by = 'sequenchr_seq_id') %>% 
                    group_by(sequenchr_seq_id) %>% 
                    mutate(entropy = DescTools::Entropy(table(value))) %>%
                    ungroup() %>% 
                    ggplot(aes(x = period, y = reorder(sequenchr_seq_id, entropy), fill = value)) +
                    geom_tile() +
                    scale_fill_manual(values = color_mapping) +
                    scale_y_discrete(labels = NULL) +
                    facet_wrap(~cluster, scales = 'free_y') +
                    labs(title = "All sequences by cluster sorted by entropy",
                         x = 'Period',
                         y = 'Sequence',
                         fill = NULL)
            }
            
            return(p)
        })
        
        # render the top 10 most commmon sequences
        # should show frequency of sequence somehow
        output$plotting_plot_common <- renderPlot({
            store$tidy_data %>% 
                group_by(sequenchr_seq_id) %>% 
                summarize(seq_collapsed = paste0(value, collapse = 'SE3P'),
                          .groups = 'drop') %>% 
                count(seq_collapsed) %>% 
                arrange(desc(n)) %>%
                slice_head(n = 10) %>% 
                separate(seq_collapsed, into = paste0('p', 1:48), sep = "SE3P") %>% 
                mutate(sequenchr_seq_id = row_number()) %>%
                pivot_longer(cols = setdiff(colnames(.), c('n', "sequenchr_seq_id"))) %>% 
                mutate(name = as.numeric(stringr::str_remove(name, 'p'))) %>% 
                rename(period = name) %>% 
                ggplot(aes(x = period, y = sequenchr_seq_id, fill = value)) +
                geom_tile() +
                scale_fill_manual(values = color_mapping) +
                scale_y_continuous(breaks = 1:10) +
                labs(title = "Top 10 most common sequences",
                     x = 'Period',
                     y = 'Sequence (ranked)',
                     fill = NULL)
        })  
        
        # plot of just the legend colors
        output$plotting_plot_legend <- renderPlot({
            as_tibble(names(store$color_mapping)) %>%
                mutate(index = row_number()) %>% 
                ggplot(aes(x=1, y = reorder(value, -index), fill = value)) + 
                geom_tile(color = 'white', size = 3) + 
                scale_fill_manual(values = color_mapping) +
                scale_x_continuous(labels = NULL) + 
                labs(x = NULL, y = NULL) + 
                theme(legend.position = 'none')
        })
        
        # state distribution plot
        output$plotting_plot_state <- renderPlot({
            store$tidy_data %>% 
                ggplot(aes(x = period, fill = value)) +
                geom_bar(width = 1) +
                scale_fill_manual(values = color_mapping) +
                labs(title = "State distributions",
                     x = 'Period',
                     y = 'Frequency',
                     fill = NULL)
        })
        
        # modal plot
        output$plotting_plot_modal <- renderPlot({
            store$tidy_data %>% 
                count(value, period) %>% 
                group_by(period) %>% 
                filter(n == max(n)) %>% 
                ggplot(aes(x = period, y = n, fill = value)) +
                geom_col() +
                scale_fill_manual(values = color_mapping) +
                labs(title = "Modal activity per period",
                     x = "Period",
                     y = 'Frequency',
                     fill = NULL)  
        })
        

    # clustering --------------------------------------------------------------

        # cluster the data
        observeEvent(input$clustering_button_cluster, {
          
            # compute optimal matching distances
            store$dist_matrix <- seqdist(atus_seq, method = "OM", indel = 1, sm = "TRATE")
            # dist_om_DHD <- seqdist(atus_seq, method = "DHD")
            
            # cluster the data
            store$cluster <- fastcluster::hclust(as.dist(store$dist_matrix), method = "ward.D2")
            
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
                               height = 500)
                )
            )
            
            # add cluster assignments to the tidy data
            # hcl_k <- input$clustering_slider_n_clusters
            # cluster_assignments <- cutree(store$cluster, k = hcl_k)
            # cluster_ns <- table(cluster_assignments)
            # cluster_assignments <- factor(
            #     cluster_assignments, 
            #     labels = paste("Cluster", 1:hcl_k, " | n = ", cluster_ns)
            # )
            # 
            # # plot the sequences with clusters
            # store$tidy_data <- tibble(cluster = cluster_assignments,
            #             sequenchr_seq_id = 1:length(cluster_assignments)) %>% 
            #     right_join(store$tidy_data, by = 'sequenchr_seq_id') 
            
        })
        
        # plot the dendrogram
        output$clustering_plot_dendrogram <- renderPlot({
            
            # stop here if clustering hasn't been run yet
            validate(need(is(store$cluster, 'hclust'),
                          'Cluster the data first'))
            
            
            hcl_ward <- store$cluster
            hcl_k <- input$clustering_slider_n_clusters
            
            # build base dendrogram
            dend <- as.dendrogram(hcl_ward) %>% set("branches_k_color", k = hcl_k) %>% set("labels_colors")
            
            # cut off bottom of dendogram for computation performance
            # TODO: need a reactive way to do this
            dend <- cut(dend, h = 50)$upper
            ggd1 <- as.ggdend(dend)
            
            # set dashed line for non-cluster segments
            ggd1$segments$linetype <- 'solid'
            ggd1$segments$linetype[which(is.na(ggd1$segments$col))] <- 'dashed'
            
            # set connecting lines to grey
            ggd1$segments$col[is.na(ggd1$segments$col)] <- 'grey50'
            
            # plot the dendrograms
            ggd1$segments %>% 
                ggplot() + 
                geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                             color = ggd1$segments$col, linetype = ggd1$segments$linetype,
                             lwd = 0.9, alpha = 0.7) +
                scale_x_continuous(labels = NULL) +
                scale_y_continuous(labels = scales::comma_format()) +
                labs(title = "Dendrogram of edit distance with Ward (D2) linkage",
                     subtitle = 'Helpful subtitle goes here',
                     x = NULL,
                     y = NULL) +
                theme(axis.ticks = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      legend.position = 'none')
        })
        
        # compute and plot silhouette width
        observeEvent(input$clustering_button_silhouette, {
            # get optimal cluster sizes by calculating silhouette width
            store$s_width <- NbClust::NbClust(
                data = NULL,
                diss = as.dist(store$dist_matrix),
                distance = NULL,
                method = 'ward.D2',
                max.nc = 10,
                min.nc = 2,
                index = 'silhouette'
            )
            
            # update slider with best k value
            updateSelectInput(session = session,
                              inputId = 'clustering_slider_n_clusters',
                              selected = store$s_width$Best.nc[['Number_clusters']])
            
            # remove and add silhouette plot tab
            removeTab(inputId = 'plotting_tabs',
                      target = 'Silhouette plot')
            insertTab(
                inputId = 'plotting_tabs',
                target = 'Dendrogram',
                position = 'after',
                select = TRUE,
                tab = tabPanel(
                    title = 'Silhouette plot',
                    br(),
                    plotOutput(outputId = 'clustering_plot_silhouette',
                               height = 500)
                )
            )
        })
        
        # plot the silhouette width
        output$clustering_plot_silhouette <- renderPlot({
            
            # stop here if silhouette width hasn't been run yet
            validate(need(is(store$s_width, 'list'),
                          'Calculate the silhouette width first'))
            
            store$s_width$All.index %>% 
                enframe() %>% 
                mutate(name = as.numeric(name)) %>% 
                ggplot(aes(x = name, y = value)) +
                geom_line(color = 'grey30') +
                geom_area(alpha = 0.4) +
                geom_point(color = 'grey30') +
                scale_x_continuous(breaks = 2:10) +
                labs(title = "Silhouette width",
                     subtitle = 'Greater width is better',
                     x = 'n clusters',
                     y = 'Silhouette width') 
        })
        

    # explore -----------------------------------------------------------------

        # # render the d3 plot
        # output$explore_d3_chord <- renderD3({
        #     # r2d3(data = runif(5, 0, input$bar_max),
        #     #      script = file.path('d3_plots', 'chord.js')
        #     r2d3(data = matrix(round(runif(input$bar_max, 1, 10000)), 
        #                        ncol = 4, nrow = 4), 
        #          script = file.path('d3_plots', 'chord.js'))
        # })
        
        # render the chord plot
        output$explore_plot_chord <- renderChorddiag({
            
            # add NA filler rows after each group before calculating transition matrix
            # this prevents end of day looping back to beginning of day for next group
            freq_data <- store$tidy_data %>% 
                mutate(value = as.character(value)) %>% 
                group_by(sequenchr_seq_id) %>% 
                group_split() %>% 
                map_dfr(.f = function(df){
                    df %>% add_row(sequenchr_seq_id = NA, value = NA, period = NA)
                })
            
            # calculate transition matrix
            n <- nrow(freq_data)  
            TRATE_mat <- table(tibble(previous = freq_data$value[1:(n-1)],
                                      current = freq_data$value[2:n]))
            TRATE_mat <- TRATE_mat / sum(TRATE_mat)
            
            # plot the chord diagram
            p <- chorddiag(data = TRATE_mat, 
                           groupColors = as.vector(color_mapping), 
                           groupnamePadding = 20,
                           groupnameFontsize = 12)
            
            return(p)
        })
        
    }
)