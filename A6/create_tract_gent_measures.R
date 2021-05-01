#-------------------------------------------------------------------------------
# PREPARE CENSUS TRACT GENTRIFICATION DATA
#
# AUTHOR: Francine Stephens
# DATE CREATED: 4/30/21
# LAST UPDATED: 4/30/21
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
setwd("~/Stanford/SOC176/soc176-data-processing/A6")
wd <- getwd()
shp_repo <- "C:/Users/Franc/Documents/Shapefile_Repository/"
tracts10_path <- "2010USA_CensusGeog_Shp/nhgis0005_shapefile_tl2010_us_tract_2010/"
metros10_path <- "2010USA_CensusGeog_Shp/tl_2010_us_cbsa10/"
student_path <- "C:/Users/Franc/Documents/Stanford/SOC176/"
##INSERT DATA PATHS HERE ##



# APIs
census_api_key("99ccb52a629609683f17f804ca875115e3f0804c",  overwrite = T)
Sys.setenv(CENSUS_KEY="99ccb52a629609683f17f804ca875115e3f0804c")


## IMPORT DATA------------------------------------------------------------------
student_hoods <- read_csv(paste0(student_path,
                                 "student_neighborhoods_list.csv")
)

census_tracts <- st_read(paste0(shp_repo, 
                                tracts10_path,
                                "US_tract_2010.shp"),
                         quiet = F)
metros <- st_read(paste0(shp_repo, 
                         metros10_path,
                         "tl_2010_us_cbsa10.shp"),
                  quiet = F)

state_codes <- c(state.abb, "DC")
us_tracts <- map_df(state_codes, ~tracts(state = .x, cb = FALSE))
us_places <- map_df(state_codes, ~places(state = .x, cb = TRUE))

# 2020 DEMOGS


## 2010 DEMOGS 
  # census tenure
  # ACS all other measures


## 2000 DEMOGS


## 1990 DEMOGS 


