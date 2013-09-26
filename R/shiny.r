json <- function(x) {
  path <- unique(paths(x))
  if (length(path) == 1 && !is.na(path)) {
    align(x)
  } else {
    reduce_depth(x, 2)
  }
}  


shine <- function(x, ...) {
  require(shiny)
  
  addResourcePath("lineprof", system.file("www", package = "lineprof"))
  ui <- pageWithSidebar(
    headerPanel("Line profiling"),
    sidebarPanel(
      textInput("nav", "Navigate to:"),
      radioButtons("type", "Type", c("loc", "calls")),
      actionButton("zoom", "Zoom")
    ),
    mainPanel(
      tags$head(tags$script(src = 'lineprof/format-table.js')),
      htmlOutput("profile")
    )
  )
  
  server <- function(input, output, session) {
    observe({
      # Take reactive dependency on zoom
      input$zoom
      
      nav <- isolate(input$nav)
      type <- isolate(input$type)
      
      if (nav != "") {
        if (type == "loc") {
          x <<- focus(x, ref = nav)
        } else {
          x <<- focus(x, f = eval(parse(text = nav)))
        }        
      }
      
      session$sendCustomMessage(type = 'formatTable', json(x))
    })
  }
  
  runApp(list(ui = ui, server = server))
}