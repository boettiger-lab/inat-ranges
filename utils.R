
library(dplyr)
library(duckdbfs)
library(sf)

as_dataset.sf <- function(sf, ...) {
  # cludgy way to get polygon into duckdb as spatial data
  tmp <- tempfile(fileext = ".fgb")
  sf |> sf::st_transform(4326) |> sf::write_sf(tmp, append = FALSE, quiet = TRUE)
  aoi <- duckdbfs::open_dataset(tmp, ...)

  aoi
}

# promote bbox to polygon
as_poly <- function(aoi) {

  crs <- st_crs(aoi)
  if (crs$input != "EPSG:4326" ) {
    aoi <- aoi |> st_transform(4326)

  } 
  if (inherits(aoi, "bbox")) {
    aoi <- aoi |> 
      st_as_sfc() |> 
      st_as_sf() |> 
      rename(geom = x)
  }
aoi
}



get_h3_aoi <- function(aoi, zoom = 0L, precision = 6L, upper = FALSE) {
  duckdbfs::load_h3()

  zoom <- as.integer(zoom)
  # consider auto-retry at higher precision if subset is empty.
  precision <- as.integer(precision)
  res <- paste0("h", precision)

  if(inherits(aoi, "sf") || inherits(aoi, "bbox")) {
    aoi <- as_poly(aoi)
    aoi <- as_dataset.sf(aoi)
  }

  # multipolygon dump may not be needed for draw tools.
  h3_aoi <- aoi |>
    dplyr::mutate(poly = array_extract(unnest(st_dump(geom)),"geom"),
          h3id = h3_polygon_wkt_to_cells(poly,{precision}),
          h3id = unnest(h3id)
          ) |>
    dplyr::mutate(h0 = h3_h3_to_string( h3_cell_to_parent(h3id, {zoom})),
           h3id = h3_h3_to_string (h3id) )

  if(upper) {
    h3_aoi <- h3_aoi |> 
    dplyr::mutate(h0 = toupper(h0), h3id = toupper(h3id))
  }

  h3_aoi |>
    dplyr::select(h0, h3id) |>
    duckdbfs::as_view("h3_aoi")
}

hex_res <- function(x) {
    x |>
    utils::head(1) |>
    dplyr::mutate(res = h3_get_resolution(h3id)) |>
    dplyr::pull(res)
}

hex_join <- function(x,y) {
  res_x <- hex_res(x)
  res_y <- hex_res(y)

  if (res_x > res_y) {
    y <- y |> 
      dplyr::mutate(h3id = unnest(h3_cell_to_children(h3id, {res_x})),
                    h3id = toupper(h3id))
  }
    if (res_x < res_y) {
    y <- y |> 
      dplyr::mutate(h3id = h3_cell_to_parent(h3id, {res_x}))
  }

  dplyr::inner_join(x, y)
}


antimeridian_hexes <- function(zoom = 4, con = duckdbfs::cached_connection()) {
  duckdbfs::load_h3(con)
  duckdbfs::load_spatial(con)
  DBI::dbExecute(con, 
  "
  CREATE OR REPLACE TABLE antimeridian AS (
    SELECT ST_GeomFromText(
    'POLYGON ((170 85, 190 85, 190 -85, 170 -85, 170 85))'
    ) AS geometry
  )
  ")
  zoom <- as.integer(zoom)
  
  am <- 
    dplyr::tbl(con, "antimeridian") |>
    dplyr::mutate(h3id = unnest(
      h3_polygon_wkt_to_cells_string(geometry, {zoom})
      )) |>
    dplyr::select(h3id)

am
}


viridis_pal <- 
  function(column = "height",
           n = 61, 
           min_v = 0, 
           max_v = 1) {
  pal <- viridisLite::viridis(n)
  fill_color = mapgl::step_expr(
      column = column,
      base = pal[1],
      stops = pal[2:n],
      values = seq(min_v, max_v, length.out = n-1),
      na_color = "white"
    )
  
  fill_color
}
