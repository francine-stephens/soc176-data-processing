#-------------------------------------------------------------------------------
# Multi-family housing
#
# AUTHOR: Francine Stephens
# DATE CREATED: 5/28/21
# LAST UPDATED: 5/28/21
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
