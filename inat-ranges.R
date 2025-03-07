library(dplyr)
library(duckdbfs)
library(mapgl)

# Also requires get_h3_aoi() from utils.R

richness <- function(inat, aoi) {

  h3_aoi  <- get_h3_aoi(aoi, precision = 4) |> select(h3id)

  clock <- bench::bench_time({
  inat |> 
    rename(h3id = h4) |>
    inner_join(h3_aoi, by = "h3id") |>
    distinct(taxon_id, h3id) |> 
    group_by(h3id) |>
    summarise(n = n()) |>
    mutate(height = n / max(n)) |> 
    duckdbfs::to_h3j("s3://public-data/inat-tmp-ranges.h3j")
  #  write_dataset("s3://public-data/inat-tmp-ranges.parquet")
  })

  return(clock)
}

richness_map <- function(
  m = maplibre(),
  url = "https://minio.carlboettiger.info/public-data/inat-tmp-ranges.h3j",
  aoi = NULL
  ) {
  
  m <- m |>
    add_h3j_source("h3j_source",
                  url = url
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
        0,
        1,
        list("*", 100000, list("get", "height"))
      ),
      fill_extrusion_opacity = 0.7
    )
  if(!is.null(aoi)) {
    # center <- st_coordinates(st_centroid(st_as_sfc(st_bbox(aoi))))
   # m <- m |> set_view(center = center, zoom = 3)
  }

  return(m)
}
