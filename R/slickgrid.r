slickgridOutput <- function(outputId, width = 600, height = 600) {
  shiny::addResourcePath(
    prefix = "slickgrid",
    directoryPath = system.file("slickgrid", package="lineprof")
  )

  shiny::tagList(
    shiny::singleton(shiny::tags$head(
      shiny::tags$script(src = "slickgrid/lib/jquery.event.drag-2.2.js"),
      shiny::tags$link(rel = "stylesheet", type = "text/css",
                href = "slickgrid/slick.grid.css"),
      shiny::tags$script(src = "slickgrid/slick.core.js"),
      shiny::tags$script(src = "slickgrid/slick.grid.js"),
      shiny::tags$script(src = "slickgrid/shiny-slickgrid.js")
    )),
    shiny::div(id = outputId, class = "slickgrid shiny-slickgrid-output",
      style = paste0("width:", width, "px; height:", height, "px;"))
  )
}
