library(shiny)
library(bslib)

source("utils.R")
source("inat-ranges.R")


# intialize data
load_h3()
load_spatial()
duckdbfs::duckdb_secrets()
inat <- open_dataset("s3://public-inat/hex")
taxa <- open_dataset("https://minio.carlboettiger.info/public-inat/taxonomy/taxa.parquet",
                     recursive = FALSE) |> rename(taxon_id = id)

# intialize map
m <- maplibre(center = c(-110, 37), zoom = 3) |>  add_draw_control()

# User interface
ui <- page_sidebar(
    title = "iNaturalist Rangemaps",

    sidebar = sidebar(
        card(
        input_switch("filter", "filter taxa", value = TRUE),
        varSelectInput("rank", NULL, taxa, selected = "class"),
        textInput("taxon", NULL, "Aves")
        ),
        actionButton("get_features", "Map richness")
    ),
    card(
    maplibreOutput("map"),
    )
)

# Server
server <- function(input, output, session) {
# output$map <- renderMaplibre(m)
# observeEvent(input$map_bbox, { }) # We can react to any zoom/pan on the map

    output$map <- renderMaplibre({
        richness_map(m) # |> fly_to(input$map_center, input$map_zoom)
     })

    observeEvent(input$get_features, {
        # Use the bbox as the focal area unless user has selected a focal area
        drawn_features <- get_drawn_features(mapboxgl_proxy("map"))

        if(nrow(drawn_features) > 0) {
            aoi <- drawn_features
        } else if (!is.null(input$map_bbox)){
            aoi <- sf::st_bbox(unlist(input$map_bbox), crs = 4326)
        } else {
            aoi <- spData::us_states
        }

        if (input$filter) {
            inat <- taxa |> 
            filter(.data[[input$rank]] == input$taxon) |> 
            select(taxon_id) |>
            inner_join(inat, by = "taxon_id")
        }

        richness(inat, aoi)

        #session$reload()

     #   maplibre_proxy("map") |> 
     #     set_view(input$map_center, input$map_zoom) |>
     #     set_h3j_source("h3j_layer", "https://minio.carlboettiger.info/public-data/inat-tmp-ranges.h3j") 
         
          #  set_filter("h3j_layer", filter = list(">=", "n", 300))

          m = maplibre(center = input$map_center, zoom = input$map_zoom)
    })

    output$map <- renderMaplibre({
        richness_map(m) # |> fly_to(input$map_center, input$map_zoom)
     })

}

shinyApp(ui, server)