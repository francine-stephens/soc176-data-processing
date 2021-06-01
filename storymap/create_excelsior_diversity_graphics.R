#-------------------------------------------------------------------------------
# CREATE 2020 RACIAL/ETHNIC, NATIVITY, AND LINGUISTIC GRAPHICS 
#
# AUTHOR: Francine Stephens
# DATE CREATED: 6/1/21
# LAST UPDATED: 6/1/21
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


## IMPORT DATA------------------------------------------------------------------
race_data <- read_csv(paste0(wd,
                             "/san_francisco_2020_tracts_race.csv")
                      )

citizen_lang_data <- read_csv(paste0(wd,
                                     "/san_francisco_2020_tracts_citizen_language.csv")
                              )


excelsior_tracts <- c(25500, 
                      26001,
                      26002, 
                      26003,
                      26004, 
                      26301,
                      26100)


## CLEAN DATA-------------------------------------------------------------------

# Race Excelsior v. Race in SF





## BUILD & EXPORT GRAPHICS------------------------------------------------------

# Stacked bargraph of Race Excelsior v. Race in SF





