


# type <- "region" # "county", "locality"
# gdf <- overture::get_division("United States", type = type, as_sf = FALSE)


duckdbfs::duckdb_secrets(key = "", secret = "",
                         endpoint = "s3.amazonaws.com",
                         bucket = "overturemaps-us-west-2")

options("overture-bucket" = "overturemaps-us-west-2",
        "overture-release" = "2025-07-23.0")

us <- overture::overture("divisions", "division_area") |>
 dplyr::filter(country == "US") |>
 dplyr::mutate(name = struct_extract(names, "primary")) |>
 dplyr::distinct(id, name, subtype, region) |>
 collect()
 
states <- us |> filter(subtype == "region") |> pull(name)


duckdbfs::duckdb_secrets()
sci <- duckdbfs::open_dataset("s3://public-inat/taxonomy/taxa.parquet", recursive = FALSE)
common <- duckdbfs::open_dataset("s3://public-inat/taxonomy/vernacular/VernacularNames-english.csv", format = "csv", recursive = FALSE) |> select(id, vernacularName)
taxa <- left_join(sci, common, by = "id")
