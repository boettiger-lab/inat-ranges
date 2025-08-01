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

duckdb_config(threads = 24) # I/O limited so overclock threads

ui <- page_sidebar(
  shinybusy::add_busy_spinner(),

  title = "iNaturalist Species Ranges",
  sidebar = sidebar(
    textInput("location", "Location", "United States"),
    varSelectInput("rank", NULL, taxa, selected = "class"),
    textInput("taxon", NULL, "Aves"),
    actionButton("button", "Go")
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  )
)

server <- function(input, output, session) {
  output$map <- renderMaplibre({
    aoi <- get_division(input$location)
    url <- richness(inat, aoi, rank = input$rank, taxon = input$taxon)
    m <- richness_map(url, aoi)
    m
  })

  observeEvent(input$button, {
    print(input$location)
    aoi <- get_division(input$location)
    richness(inat, aoi, rank = input$rank, taxon = input$taxon)

    session$reload()
  })
}

shinyApp(ui, server)
