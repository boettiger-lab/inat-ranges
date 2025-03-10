library(dplyr)
library(duckdbfs)
library(mapgl)
library(glue)


# Also requires get_h3_aoi() from utils.R

richness <- function(inat, aoi, rank = NULL, taxon = NULL, zoom = 3) {

  endpoint <- Sys.getenv("AWS_S3_ENDPOINT")
  hash <- digest::digest(list(aoi, rank, taxon))
  s3 <- paste0("s3://public-data/cache/inat/", hash, ".h3j")

  # check if hash exists

  # filter
  if (!is.null(rank) && !is.null(taxon)) {
    taxa <- open_dataset(glue("https://{endpoint}/public-inat/taxonomy/taxa.parquet"),
           recursive = FALSE)
    inat <- taxa |> 
      rename(taxon_id = id) |>
      filter(.data[[rank]] == taxon) |> 
      select(taxon_id) |>
      inner_join(inat, by = "taxon_id")
  }

  h3_aoi  <- get_h3_aoi(aoi, precision = 4) |> select(h3id)

  clock <- bench::bench_time({
  inat |> 
    rename(h3id = h4) |>
    inner_join(h3_aoi, by = "h3id") |>
    distinct(taxon_id, h3id) |> 
    group_by(h3id) |>
    summarise(n = n()) |>
    mutate(height = n / max(n)) |> 
    duckdbfs::to_h3j(s3)
  #  write_dataset("s3://public-data/inat-tmp-ranges.parquet")
  })


  center <- c(st_coordinates(st_centroid(st_as_sfc(st_bbox(aoi)))))
  url <- gsub("s3://", glue("https://{endpoint}/"), s3)

  meta <- list(X = center[1], 
               Y = center[2],
               zoom = zoom, 
               url = url, 
               time = clock[[2]])
  return(meta)
}

richness_map <- function(meta) {

  m <- 
    maplibre(center = c(meta$X, meta$Y), zoom = meta$zoom) |>
    add_draw_control() |>
    add_h3j_source("h3j_source",
                  url = meta$url
    ) |>
    add_fill_extrusion_layer(
      id = "h3j_layer",
      source = "h3j_source",
      tooltip = "n",
      fill_extrusion_color = viridis_pal("height"),
      fill_extrusion_height = list(
        "interpolate",
        list("linear"),
        list("zoom"),
        0,
        0, 1,
        list("*", 100000, list("get", "height"))
      ),
      fill_extrusion_opacity = 0.7
    )

  return(m)
}
