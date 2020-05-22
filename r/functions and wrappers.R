

mbie_header =
  ## Adds a basic MBIE header, that will only display
  ##  if the page is not embedded as an iframe.
  ## Requires: www/mbie-logo.png
  function() div(id = "mbie-header",
                 div(class = "mbie-topbar"),
                 div(class = "mbie-brand",
                     tags$a(class = "mbie-brand",
                            title = "DEFRA logo",
                            tags$img(src = "DEFRA_logo.png",
                                     alt = "DEFRA"))
                 )
  )



tabTitle =
  ## Creates an appropriately styled title
  function(x)
    h3(class = "tabTitle", x)
tabDesc =
  ## Creates an appropriately styled description
  ## If NA, returns NULL
  function(x)
    if(!is.na(x)) tags$p(class = "tabDesc", x) else NULL
tabPwT =
  ## tabPanel with Title (using tabTitle)
  ## Also searches for a match in `tabdesc`
  ##  (a named vector of descriptions found in "ui_doctabs.R")
  ## If one is found, adds the description below the title
  function(title, ...){
    tabPanel(title,
             div(class = "tabTitlePanel",
                 tabTitle(title),
                 tabDesc(tabdesc[title][[1]]),
                 div(class = "tabTitlePanel-end")
             ),
             ...
    )
  }


dashboardPage =
  ## Modified navbarPage from shiny
  ## Cuts bloat and enables use of tags$head with `thead`
  function(title, ..., id = "dashboard", thead = NULL, header = NULL, footer = NULL, windowTitle = title){
    pageTitle = title
    navbarClass = "navbar navbar-default"
    tabs = list(...)
    tabset = shiny:::buildTabset(tabs, "nav navbar-nav", NULL, id)
    containerDiv = div(class = "container", div(class = "navbar-header", 
                                                span(class = "navbar-brand", pageTitle)), tabset$navList)
    contentDiv = div(class = "container-fluid")
    if(!is.null(header))
      contentDiv = tagAppendChild(contentDiv, div(class = "row", header))
    contentDiv = tagAppendChild(contentDiv, tabset$content)
    if(!is.null(footer)) 
      contentDiv = tagAppendChild(contentDiv, div(class = "row", footer))
    bootstrapPage(title = windowTitle, thead,
                  tags$nav(class = navbarClass, role = "navigation", containerDiv),
                  contentDiv)
  }


loading_icon = 
  
  function (output){
    
    shinycssloaders::withSpinner(output,color="#808000", size=3)
    
  }
