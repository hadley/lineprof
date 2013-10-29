#' Generate shiny GUI for line profiling output
#'
#' The shiny GUI generates two types of view depending on whether or not the
#' src refs for the function are available. If src refs are available, it aligns
#' profiling information with the original source code so that you can
#' easily see time and memory behaviour in the context of the original code.
#' If they are not available, it does the best it can do, just displaying
#' the sequence of calls that it captured.
#'
#' @section Display:
#'
#' The shiny app creates a table with six columns:
#'
#' \itemize{
#'   \item the line number (or call number)
#'   \item the source code, or if not available, the name of the function
#'   \item t, the amount of time spent on that line (in seconds)
#'   \item r, the amount of memory released by that call (in megabytes)
#'   \item a, the amount of memory allocated by that call (in megabytes)
#'   \item d, the number of duplications performed by that call
#' }
#'
#' The time and memory summaries are displayed as inline bar charts. This gives
#' you a qualitative impression of how expensive each line of code is - to get
#' the precise details, hover over the bar.
#'
#' @section Navigation:
#'
#' Calls/lines with a non-trivial amount of computation will be linked so that
#' you can see the breakdown of time/memory within that line/call. This will
#' work well for your own code (since you will have all the srcrefs), but
#' less well for other packages and base R code since you'll only be able to
#' see the sequence of the calls.
#'
#' To go back to the previous call, click on the back button.
#'
#' @param x a line profiling dataset
#' @export
#' @examples
#' source(find_ex("read-delim.r"))
#' wine <- find_ex("wine.csv")
#'
#' x <- lineprof(read_delim(wine, sep = ","), torture = TRUE)
#' \dontrun{
#' shine(x)
#' }
shine <- function(x) {
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
    zoomed <- auto_focus(zoomed)

    stack$push(zoomed)
  }

  server <- function(input, output, session) {
    update_table <- function() {
      msg <- json(stack$top())
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

  runApp(list(ui = ui, server = server), 
    launch.browser = getOption("viewer", utils::browseURL))
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

json <- function(x) {
  path <- unique(paths(x))
  if (length(path) == 1 && !is.na(path)) {
    align(x)
  } else {
    format(reduce_depth(x, 2))
  }
}
