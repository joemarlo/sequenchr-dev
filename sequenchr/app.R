require(tidyverse)
require(DT)
require(shinyBS)
require(shiny)
library(shinythemes)
library(shinyWidgets) # for slider skin
library(arm) # currently just for lalonde dataset
library(viridis) # for color blind sensitive colors
theme_set(theme_minimal())

# this shouldn't be neccessary but is currently required
setwd("sequenchr")

# load UI
map(list.files('UI'), function(file) source(file.path("UI", file)))
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
    
        output$summary_table <- renderTable({
            data.frame(
                n_sequences = nrow(sequence_data),
                unique_sequences = nrow(distinct(as.data.frame(sequence_data)))
            )
        })
        

# plotting ----------------------------------------------------------------

        # render the sequence plot
        output$plotting_plot_sequence <- renderPlot({
            sequence_data %>%
                as_tibble() %>% 
                setNames(1:ncol(sequence_data)) %>% 
                mutate(sequenchr_seq_id = row_number()) %>%
                pivot_longer(cols = setdiff(colnames(.), "sequenchr_seq_id")) %>%
                mutate(period = as.numeric(name)) %>% 
                ggplot(aes(x=period, y=sequenchr_seq_id, fill=value)) +
                geom_tile() +
                labs(title = "lorem ipsum",
                     x = 'Period',
                     y = 'Sequence')
        })        
    }
)