library(shiny)
library(bslib)
library(gt)

ui <- page_sidebar(
  title = "Simulator",
  sidebar = sidebar("sidebar"),
  gt_output(outputId = "fastest_splits_tbl")
)

server <- function(input, output, session) {
  output$fastest_splits_tbl <- gt::render_gt()
}

shinyApp(ui, server)
