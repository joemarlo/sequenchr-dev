main_UI <- fluidPage(
    # download roboto font
    HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
    
    # set default slider skin
    chooseSliderSkin(skin = "Flat", color = "#221146"),
    
    # initiate shinyjs
    # useShinyjs(),
    
    # load custom CSS file
    includeCSS("www/custom_css.css"),
    
    # set top left title
    titlePanel(
        title = h2("sequenchr"),
        windowTitle = "sequenchr"
    ),
    
    # set main navigation
    navbarPage(
      id = "nav",
      title = 'sequenchr',
      UI_welcome,
      UI_plotting
    )
  )
