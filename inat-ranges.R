library(dplyr)
library(duckdbfs)
library(mapgl)
library(glue)

public_endpoint <- Sys.getenv(
  "AWS_PUBLIC_ENDPOINT",
  Sys.getenv("AWS_S3_ENDPOINT")
)
# intialize data

duckdbfs::duckdb_secrets()
inat <- open_dataset("s3://public-inat/hex")

common <- open_dataset(
  "s3://public-inat/taxonomy/vernacular/VernacularNames-english.csv",
  format = 'csv',
  recursive = FALSE
) |>
  select(id, vernacularName)
taxa <- open_dataset(
  glue("s3://public-inat/taxonomy/taxa.parquet"),
  recursive = FALSE
) |>
  select(
    "id",
    "scientificName",
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "genus",
    "specificEpithet",
    "infraspecificEpithet"
  )

taxa <- common |> inner_join(taxa)

get_hash <- function(aoi, rank, taxon) {
  digest::digest(list(aoi, rank, taxon))
}

# Also requires get_h3_aoi() from utils.R
richness <- function(inat, aoi, rank = NULL, taxon = NULL, zoom = 3) {
  public_endpoint <- Sys.getenv(
    "AWS_PUBLIC_ENDPOINT",
    Sys.getenv("AWS_S3_ENDPOINT")
  )
  hash <- get_hash(aoi, rank, taxon)
  s3 <- paste0("s3://public-data/cache/inat/", hash, ".h3j")
  url <- gsub("s3://", glue("https://{public_endpoint}/"), s3)

  # check if hash exists
  cache_hit <- tryCatch(
    {
      duckdbfs::open_dataset(s3)
      TRUE
    },
    error = function(e) FALSE,
    finally = FALSE
  )

  if (cache_hit) {
    return(url)
  }

  if (rank == "" || rank == "NULL") {
    rank <- NULL
  }
  if (taxon == "" || taxon == "NULL") {
    taxon <- NULL
  }

  # Subset by taxon, if requested
  if (!is.null(rank) && !is.null(taxon)) {
    inat <- taxa |>
      rename(taxon_id = id) |>
      filter(.data[[rank]] == taxon) |>
      select(taxon_id) |>
      inner_join(inat, by = "taxon_id")
  }

  inat <- inat |> rename(h3id = h4)

  # Subset by area, if requested
  if (nrow(aoi) > 0) {
    inat <-
      get_h3_aoi(aoi, precision = 4) |>
      select(h3id) |>
      inner_join(inat, by = "h3id")
  }

  # Aggregate to h4 hex
  inat |>
    distinct(taxon_id, h3id) |>
    group_by(h3id) |>
    summarise(n = n()) |>
    mutate(height = n / max(n)) |>
    duckdbfs::to_h3j(s3)

  return(url)
}

richness_map <- function(url, gdf) {
  bounds <- as.vector(sf::st_bbox(gdf))
  m <-
    maplibre() |>
    #    add_draw_control() |>
    add_geocoder_control() |>
    add_h3j_source("h3j_source", url = url) |>
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
    ) |>
    fit_bounds(bounds, animate = TRUE)

  return(m)
}
