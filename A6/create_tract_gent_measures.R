#-------------------------------------------------------------------------------
# PREPARE CENSUS TRACT GENTRIFICATION DATA
#
# AUTHOR: Francine Stephens
# DATE CREATED: 4/30/21
# LAST UPDATED: 5/1/21
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
LTDB_geog_ids_path <- "C:/Users/Franc/Documents/Stanford/SOC176/soc176-data-processing/A5/LTDB_Std_All_fullcount/"
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

LTDB00 <- read_csv(paste0(LTDB_geog_ids_path, 
                          "LTDB_Std_2000_fullcount.csv")
            )

# 2020 DEMOGS


## 2010 DEMOGS 
  # census tenure
  # ACS all other measures


## 2000 DEMOGS


## 1990 DEMOGS 



## PREP GEOGRAPHIC IDENTIFIERS--------------------------------------------------

# Get neighborhood tracts for entire class
student_nhoods <- student_hoods %>% 
  filter(!is.na(neighborhood_name)) %>%
  mutate(GEOID = str_pad(CTID, width = 11, side = "left", pad = "0")) %>%
  select(-CTID, -6:-8) %>%
  rename(student = "student_name",
         nhood = "neighborhood_name",
         city = "city_name",
         state = "state_abbrev")

full_class_tracts <- us_tracts %>%
  right_join(., student_nhoods, by = "GEOID") %>%
  arrange(GEOID, student)


# Get all geographic identifiers for a CT
all_geog_identifiers_per_tract <- LTDB00 %>%
  select(TRTID10,
         state,
         county,
         placefp10,
         cbsa10,
         metdiv10,
         ccflag10
         ) %>%
  mutate(GEOID = str_pad(TRTID10, width = 11, side = "left", pad = "0"))
  
class_all_geoids <- full_class_tracts %>%
  left_join(.,  all_geog_identifiers_per_tract, by = "GEOID") %>%
  rename(state_abb = "state.x") %>%
  select(-state.y)



## CLEAN DEMOGRAPHIC VARIABLES--------------------------------------------------

# % OWNER OCCUPIED HOUSING UNITS

