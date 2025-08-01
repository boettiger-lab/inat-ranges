library(shinybusy)
library(shiny)
library(bslib)
library(colourpicker)

library(mapgl)
library(sf)
library(dplyr)
library(duckdbfs)
library(overture)


source("utils.R")
source("inat-ranges.R")


ui <- page_sidebar(
  shinybusy::add_busy_spinner(),

  title = "iNaturalist Species Ranges",
  sidebar = sidebar(
    textInput("location", "Location", "California"),
    varSelectInput("rank", NULL, taxa, selected = "scientificName"),
    textInput("taxon", NULL, "Canis lupus"),
    actionButton("button", "Go")
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  )
)

server <- function(input, output, session) {
  output$map <- renderMaplibre({
    if (file.exists(cache)) {
      meta <- jsonlite::read_json(cache)
      print(meta$url)
    } else {
      aoi <- get_division(input$location)
      meta <- richness(inat, aoi, rank = input$rank, taxon = input$taxon)
    }
    m <- richness_map(meta)
    m
  })

  observeEvent(input$button, {
    print(input$location)
    aoi <- get_division(input$location)
    meta <- richness(inat, aoi, rank = input$rank, taxon = input$taxon)

    jsonlite::write_json(meta, cache, auto_unbox = TRUE)
    message(paste("rendering", meta$url))

    session$reload()
  })
}

shinyApp(ui, server)
