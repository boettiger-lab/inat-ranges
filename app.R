library(shiny)
library(bslib)
library(shinybusy)

source("utils.R")
source("inat-ranges.R")
endpoint <- Sys.getenv("AWS_S3_ENDPOINT")

# intialize data
load_h3()
load_spatial()
duckdbfs::duckdb_secrets()
inat <- open_dataset("s3://public-inat/hex")
taxa <- open_dataset(glue("https://{endpoint}/public-inat/taxonomy/taxa.parquet"),
                     recursive = FALSE) |> rename(taxon_id = id)
cache <- tempfile(fileext = ".json")



###### User interface ######
ui <- page_sidebar(
  title = "iNaturalist Rangemaps",
  markdown("Visualize species richness from [iNaturalist Range map datasets](https://www.inaturalist.org/pages/range_maps).
  Pan & zoom the map over the desired area and hit 'map richness', or draw the desired area with the draw tool.
  Filter by specific taxonomic ranks or view all 100,293 mapped species. 
  Note that larger areas will be slower to compute. (Area selections that overlap the antimerdian may create visual artefacts).
  "),
  shinybusy::add_busy_spinner(),
  sidebar = sidebar(
    card(
    markdown("Filter by taxonomic group or toggle off to see all species."),
    input_switch("filter", "filter taxa:", value = TRUE),
    varSelectInput("rank", NULL, taxa, selected = "class"),
    textInput("taxon", NULL, "Aves")
    ),
    actionButton("get_features", "Map richness")
  ),
  card(
    maplibreOutput("map"),
  )
)

###### Server ######
server <- function(input, output, session) {
# observeEvent(input$map_bbox, { }) # We can react to any zoom/pan on the map

  output$map <- renderMaplibre({

    # Hacky -- we sidecar the metadata here
    if(file.exists(cache)) {
      meta <- jsonlite::read_json(cache)
      print(meta$url)
    } else {
      meta <- list(X = -110,
                   Y = 37,
                   zoom = 4,
                   url = paste0("https://",
                                endpoint,
                                "/public-data/inat-tmp-ranges.h3j"))
    }
   
    m <- richness_map(meta)
    m

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

    rank <- taxon <- NULL
    if (input$filter) {
      rank <- input$rank
      taxon <- input$taxon
    }

    message("Computing richness...")
    meta <- richness(inat, aoi, rank, taxon, zoom = input$map_zoom)
    jsonlite::write_json(meta, cache, auto_unbox = TRUE)
    message(paste("rendering", meta$url))
    session$reload()



  })



}

shinyApp(ui, server)