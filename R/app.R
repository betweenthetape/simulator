library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Simulator",
  sidebar = sidebar("sidebar"),
  "Main area"
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
