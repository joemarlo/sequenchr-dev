main_UI <- fluidPage(
    # download roboto font
    HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
    
    # set default slider skin
    chooseSliderSkin(skin = "Flat", color = "#221146"),
    
    # initiate shinyjs
    # useShinyjs(),
    
    # load custom CSS file
    includeCSS(file.path("www", "custom_css.css")),
    includeCSS(file.path("www", "d3_css.css")),
    
    # set top left title
    # titlePanel(
    #   title = h2("sequenchr"),
    #   windowTitle = "sequenchr"
    # ),
    
    # set main navigation
    # TODO: redesign UI so more like shinystan
    navbarPage(
      id = "nav",
      title = 'sequenchr',
      UI_plotting,
      UI_clustering,
      UI_explore
    )
  )
