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
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  )
)

server <- function(input, output, session) {
  # output$map <- renderMaplibre({overture:::map(gdf)})

  observeEvent(input$taxon, {
    aoi <- get_division(input$location)

    message("Computing richness...")

    meta <- richness(inat, aoi, input$rank, input$taxon)
    jsonlite::write_json(meta, cache, auto_unbox = TRUE)

    message(paste("rendering", meta$url))

    # session$reload()
  }) |>
    debounce(millis = 600)

  output$map <- renderMaplibre({
    if (file.exists(cache)) {
      meta <- jsonlite::read_json(cache)
      print(meta$url)
    } else {
      meta <- list(
        X = -110,
        Y = 37,
        zoom = 4,
        url = paste0(
          "https://",
          public_endpoint,
          "/public-data/inat-tmp-ranges.h3j"
        )
      )
    }
    m <- richness_map(meta)
    m
  })
}

shinyApp(ui, server)
