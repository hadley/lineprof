json <- function(x) {
  path <- unique(paths(x))
  if (length(path) == 1 && !is.na(path)) {
    align(x)
  } else {
    format(reduce_depth(x, 2))
  }
}  


shine <- function(x, ...) {
  require(shiny)
  require(shinySlickgrid)
   
  stack <- new_stack(x)
  
  navigate <- function(ref) {
    message("Navigating to ", ref)
    if (grepl('"', ref, fixed = TRUE)) {
      zoomed <- focus(x, f = eval(parse(text = ref)))
    } else {
      zoomed <- focus(x, ref = ref)
    }
    
    stack$push(zoomed)
  }
  
  server <- function(input, output, session) {
    update_table <- function() {
      msg <- json(stack$top())
      str(msg)
      session$sendCustomMessage(type = 'formatTable', msg)
    }

    update_table()

    observe({
      if (is.null(input$navigate)) return()

      navigate(input$navigate)
      update_table()
    })
    
    observe({
      if (input$back == 0) return()
      
      message("Backing up")
      stack$pop()
      update_table()
    })
  }
  
  addResourcePath("lineprof", system.file("www", package = "lineprof"))
  ui <- bootstrapPage(
    tags$div(class = "span12", style = "padding: 10px 0px;", 
      tags$h1("Line profiling", actionButton("back", "Back"))
    ),
    mainPanel(
      slickgridOutput("profile"),
      tags$head(
        tags$script(src = 'lineprof/format-table.js'),
        tags$link(href = "lineprof/table.css", rel = "stylesheet", 
          type = "text/css")
      )
    )
  )
  
  runApp(list(ui = ui, server = server))
}


# x <- new_stack()
# x$push(1)
# x$push(2)
# x$top()
# x$pop()
# x$top()
# x$top()
new_stack <- function(init = NULL) {
  
  if (is.null(init)) {
    stack <- list()
  } else {
    stack <- list(init)
  }
  
  pop <- function(x) {
    if (length(stack) == 1) return()
    old <- top()
    stack <<- stack[-length(stack)]
    old
  }
  push <- function(x) {
    stack <<- c(stack, list(x))
  }
  top <- function() {
    stack[[length(stack)]]
  }
  
  list(pop = pop, push = push, top = top)
}