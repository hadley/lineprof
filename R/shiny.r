shine <- function(x, ...) {
  require(shiny)
  
  
  ui <- bootstrapPage(
    addResourcePath("lineprof", system.file("www", package = "lineprof")),
    sidebarPanel(
      textInput("nav", "Navigate to:"),
      radioButtons("type", "Type", c("loc", "calls")),
      actionButton("zoom", "Zoom")
    ),
    tags$head(tags$script(src = 'lineprof/format-table.js')),
    htmlOutput("profile")
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
      
      session$sendCustomMessage(type = 'formatTable', html(x))
    })
  }
  
  runApp(list(ui = ui, server = server))
}