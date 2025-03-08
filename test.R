

## Illustrate/test core app functionality without shiny
library(dplyr)
library(duckdbfs)
library(ggplot2)
library(mapgl)
library(glue)
load_h3()
load_spatial()

source("utils.R")
source("inat-ranges.R")


duckdbfs::duckdb_secrets()
inat <- open_dataset("s3://public-inat/hex")

aoi <- spData::us_states
aoi <- spData::world

# publish richness at the aoi (bbox or poly)
url <- richness(inat, aoi)
m <- maplibre() |> add_draw_control()
m <- richness_map(url = url)
m
library(htmlwidgets)
htmlwidgets::saveWidget(m, "total-richness.html")


# publish richness at the aoi (bbox or poly)
url <- richness(inat, aoi, rank = "class", taxon = "Aves")
m <- richness_map(url = url)
htmlwidgets::saveWidget(m, "aves-richness.html")

# publish richness at the aoi (bbox or poly)
richness(inat, aoi, rank = "class", taxon = "Mammalia")
m <- richness_map()
htmlwidgets::saveWidget(m, "mammals-richness.html")


## UGH can't deal with antimeridian
# dropme <- antimeridian_hexes(3)
# dropme <- antimeridian_hexes(4) |> rename(h4 = h3id)
# inat |> anti_join(dropme) |> write_dataset("s3://public-inat/ranges.parquet")
# inat <- open_dataset("s3://public-inat/ranges.parquet", recursive = FALSE)

# mutate(h3 = h3_cell_to_parent(h4, 3L))



m <- maplibre(center = c(-110.5, 34.8), zoom = 4) |> add_draw_control() 
richness_map(m, "https://minio.carlboettiger.info/public-data/inat-tmp-ranges.h3j")











library(htmlwidgets)
htmlwidgets::saveWidget(m, "example.html")












amphib = open_dataset("s3://public-inat/polygon/Amphibia.parquet", recursive = FALSE)

gdf <- amphib |> 
filter(name == "Ambystoma californiense") |>
 to_sf(crs=4326)

maplibre(center = c(-122.5, 37.8), zoom = 4) |>
      add_source(id = "gdf", gdf) |>
      add_layer("gdf-layer",
                type = "fill",
                source = "gdf",
                paint = list(
                  "fill-color" =  "darkgreen",
                  "fill-opacity" = .9
                )
        )
      




# Access SVI
#svi = open_dataset("https://minio.carlboettiger.info/public-social-vulnerability/2022/SVI2022_US_tract.parquet")
#tracts = open_dataset("https://minio.carlboettiger.info/public-social-vulnerability/2022-tracts-h3-z5.parquet") # Access CalEnviroScreen
# ces = open_dataset("https://minio.carlboettiger.info/public-calenviroscreen/ces_2021.parquet", format="parquet")

# Filter GBIF to our area-of-interest (h-index) and species of interest

ca <- tracts |>
  filter(STATE == "California") |>
  mutate(h4 = h3_cell_to_parent(h5, 4L)) |>
  mutate(h4 = tolower(as.character(h4)))

out <- ca |>
 inner_join(inat, by = "h4") |>
 count(STATE, COUNTY, FIPS, h5)


#  mutate(height = n / max(n)) |>

