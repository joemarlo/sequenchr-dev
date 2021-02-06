library(shiny)

launch_sequenchr <- function(sequence_data){
  #https://stackoverflow.com/questions/49470474/saving-r-shiny-app-as-a-function-with-arguments-passed-to-the-shiny-app
  shinyOptions(sequence_data = sequence_data)
  # source(system.file("sequenchr/app.R", local = TRUE, chdir = TRUE))$value
  source(file.path("sequenchr", "app.R"))$value
}
