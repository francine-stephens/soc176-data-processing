#-------------------------------------------------------------------------------
# Multi-family housing
#
# AUTHOR: Francine Stephens
# DATE CREATED: 5/28/21
# LAST UPDATED: 5/29/21
#-------------------------------------------------------------------------------


# SET-UP------------------------------------------------------------------------
## LIBRARIES
packages <- c(
  "readr",
  "tidyverse",
  "sf",
  "ggplot2",
  "tigris",
  "censusapi", 
  "tidycensus", 
  "leaflet",
  "lubridate",
  "rsegregation",
  "scales"
)
lapply(packages, library, character.only = T)


## PATHS
setwd("~/Stanford/SOC176/soc176-data-processing/storymap")
wd <- getwd()
shp_repo <- "C:/Users/Franc/Documents/Shapefile_Repository/"
tracts10_path <- "2010USA_CensusGeog_Shp/nhgis0005_shapefile_tl2010_us_tract_2010/"
blockgrps_path <- "2010USA_CensusGeog_Shp/us_blck_grp_2010/"


## IMPORT DATA------------------------------------------------------------------
hh_data <- read_csv(paste0(wd, "/hh_size_cbg_acs19.csv"))

blck_grps <- st_read(paste0(shp_repo, 
                            blockgrps_path,
                            "US_blck_grp_2010.shp"),
                     quiet = F)


## CLEAN DATA-------------------------------------------------------------------
hh_data_prep <- hh_data %>%
  mutate(Geo_FIPS = as.character(Geo_FIPS),
         Geo_FIPS = str_pad(Geo_FIPS, width = 12, side = "left", pad = "0")
  ) %>%
  mutate(across(starts_with("hu_"), ~(.x/occ_hu)*100)
         )
  
hh_data_shp <- blck_grps %>% 
  right_join(., hh_data_prep, by = c("GEOID10" = "Geo_FIPS")
             )



## View shapefile
pal_reds <- colorNumeric(
  palette = "Reds",
  domain = hh_data_shp$avg_hh_size
)

pal_blues <- colorQuantile(
  palette = "Blues",
  domain = hh_data_shp$hu_size7up
)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = hh_data_shp %>% 
                st_transform(., crs = 4326),
              fillColor = ~pal_blues(hu_size7up),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              label = ~(hu_size7up)
  )
