library(shiny)

ui <- fluidPage(
  uiOutput("link")
)
server <- function(input, output) {
  newlink <- a("data-viz.it.wisc.edu/pandemic", 
               href="https://data-viz.it.wisc.edu/pandemic/")
  
  output$link = renderUI({
    tagList("Yandell pandemic app moved to", newlink)
    })
}

shinyApp(ui, server)

