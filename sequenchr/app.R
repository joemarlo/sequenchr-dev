require(tidyverse)
require(DT)
require(shinyBS)
require(shiny)
library(shinythemes)
library(shinyWidgets) # for slider skin
library(arm) # currently just for lalonde dataset
library(viridis) # for color blind sensitive colors
theme_set(theme_minimal())

# this shouldn't be necessary but is currently required
setwd("sequenchr")

# load UI
map(list.files('UI'), function(file) source(file.path("UI", file))) # TODO: is there a do this w/o global vars?
source('main_ui.R', local = TRUE)

# set default object if one doesn't exist
# TODO: change this to stop if input doesn't exist
appDir <- getwd()
sequence_data_default <- mtcars
sequence_data <- getShinyOption("sequence_data", sequence_data_default)

shinyApp(
    ui = main_UI,
    server = function(input, output, session){
        
        # set and reset working directory
        oldwd <- setwd(appDir)
        # on.exit(setwd(oldwd))
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
            mutate(period = as.numeric(name))
        
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
            store$tidy_data %>% 
                ggplot(aes(x = period, y = sequenchr_seq_id, fill = value)) +
                geom_tile() +
                scale_fill_manual(values = color_mapping) +
                labs(title = "All sequences sorted",
                     x = 'Period',
                     y = 'Sequence',
                     fill = NULL)
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
    }
)