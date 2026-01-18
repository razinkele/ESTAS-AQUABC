# Root-level Shiny wrapper so Shiny Server serves the app at /AQUABC/
# This embeds the Python Shiny backend running on port 8001 in an iframe.

library(shiny)

ui <- fluidPage(
  tags$h2("AQUABC Front End"),
  tags$div(style = "height: 90vh;",
    tags$iframe(id = "appframe", src = "", style = "width:100%; height:100%; border:0;")
  ),
  tags$script(HTML("document.getElementById('appframe').src='//' + window.location.hostname + ':8001/';"))
)

server <- function(input, output, session) {
  # wrapper, nothing to do server-side
}

shinyApp(ui, server)
