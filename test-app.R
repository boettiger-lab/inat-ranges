library(shiny)
library(mapgl)

source("utils.R")
source("inat-ranges.R")

load_h3()
load_spatial()

duckdbfs::duckdb_secrets()
inat <- open_dataset("s3://public-inat/hex")

# dummy case
aoi <- spData::us_states

taxa <- open_dataset("https://minio.carlboettiger.info/public-inat/taxonomy/taxa.csv", format = "csv", recursive = FALSE)


# publish richness at the aoi (bbox or poly)
m = 
  maplibre(center = c(-110.5, 37), zoom = 3) |> 
  add_draw_control()


ui <- fluidPage(
    maplibreOutput("map"),
    actionButton("get_features", "Get Drawn Features"),
    verbatimTextOutput("feature_output")
)

server <- function(input, output, session) {
    output$map <- renderMaplibre({
        m
    })

    output$feature_output <- renderPrint({
            print(input$map_bbox)
    })


    observeEvent(input$map_bbox, {

        bbox =sf::st_bbox(unlist(input$map_bbox), crs = 4326)
        print(bbox)


    })

    observeEvent(input$get_features, {
        drawn_features <- get_drawn_features(mapboxgl_proxy("map"))
        
        bbox = sf::st_bbox(unlist(input$map_bbox), crs = 4326)
        zoom = input$map_zoom

        output$map <- renderMaplibre({
            richness(inat, drawn_features)
            richness_map(m)
     })
    })
}

shinyApp(ui, server)