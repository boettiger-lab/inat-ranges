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
  # Create reactive expressions that only trigger when button is clicked
  aoi_reactive <- eventReactive(input$button, {
    get_division(input$location)
  })

  url_reactive <- eventReactive(input$button, {
    aoi <- aoi_reactive()
    richness(inat, aoi, rank = input$rank, taxon = input$taxon)
  })

  output$map <- renderMaplibre({
    # This will only render when button is clicked
    url <- url_reactive()
    aoi <- aoi_reactive()

    print(url)
    m <- richness_map(url, aoi)
    m
  })
}

shinyApp(ui, server)
