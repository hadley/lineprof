slickgridOutput <- function(outputId, width = 600, height = 600) {
  addResourcePath(
    prefix = "slickgrid",
    directoryPath = system.file("slickgrid", package="lineprof"))

  tagList(
    singleton(tags$head(
      tags$script(src = "slickgrid/lib/jquery.event.drag-2.2.js"),
      tags$link(rel = "stylesheet", type = "text/css",
                href = "slickgrid/slick.grid.css"),
      tags$script(src = "slickgrid/slick.core.js"),
      tags$script(src = "slickgrid/slick.grid.js"),
      tags$script(src = "slickgrid/shiny-slickgrid.js")
    )),
    div(id = outputId, class = "slickgrid shiny-slickgrid-output",
      style = paste0("width:", width, "px; height:", height, "px;"))
  )
}
