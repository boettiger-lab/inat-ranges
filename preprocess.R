library(dplyr)
library(duckdbfs)
library(ggplot2)
library(mapgl)
library(glue)
load_h3()
load_spatial()

duckdbfs::duckdb_secrets()
inat = open_dataset("s3://public-inat/rangemaps")

# Access SVI
#svi = open_dataset("https://minio.carlboettiger.info/public-social-vulnerability/2022/SVI2022_US_tract.parquet")
tracts = open_dataset("https://minio.carlboettiger.info/public-social-vulnerability/2022-tracts-h3-z8.parquet") # Access CalEnviroScreen
# ces = open_dataset("https://minio.carlboettiger.info/public-calenviroscreen/ces_2021.parquet", format="parquet")

# Filter GBIF to our area-of-interest (h-index) and species of interest

ca = tracts |>
  filter(STATE == "California") |>
  mutate(h3 = h3_cell_to_parent(h8, 3L)) |>
  mutate(h3 = tolower(as.character(h3)))

ca |> inner_join(inat, by = "h3") |> count(STATE, COUNTY, FIPS, h8)


bird_counts = sf_birds |>
  count(FIPS, geom) |>
  mutate(richness = n / {total})

ces_poverty = ces |> select("Poverty", "FIPS")
combined <- svi |>
  select("RPL_THEMES", "FIPS") |> filter(RPL_THEMES > 0) |>
  inner_join(bird_counts, "FIPS")  |>
  inner_join(ces_poverty, "FIPS") |>
  mutate(svi_bin = cut(RPL_THEMES, breaks = c(0, .25, .50, .75, 1), 
                           labels = c("Q1", "Q2", "Q3", "Q4"))) |>
  mutate(poverty_bin = cut(Poverty, breaks = c(0, 25, 50, 75, 100), 
                           labels = c("0-25", "25-50", "50-75", "75-100")))
 
  
