#-------------------------------------------------------------------------------
# IDENTIFY CENSUS TRACTS FOR CITIES HISTORICAL APPROACH 
#
# AUTHOR: Francine Stephens
# DATE CREATED: 4/20/21
# LAST UPDATED: 4/23/21
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
  "lubridate"
)
lapply(packages, library, character.only = T)

## PATHS
setwd("~/Stanford/SOC176/soc176-data-processing/A5")
wd <- getwd()
shp_repo <- "C:/Users/Franc/Documents/Shapefile_Repository/"
tracts10_path <- "2010USA_CensusGeog_Shp/nhgis0005_shapefile_tl2010_us_tract_2010/"
student_path <- "C:/Users/Franc/Documents/Stanford/SOC176/"
ltdb_path <- "/LTDB_Std_All_fullcount/"

# APIs
census_api_key("99ccb52a629609683f17f804ca875115e3f0804c",  overwrite = T)
Sys.setenv(CENSUS_KEY="99ccb52a629609683f17f804ca875115e3f0804c")


## Import Data
student_hoods <- read_csv(paste0(student_path,
                                 "student_neighborhoods_list.csv")
)

census_tracts <- st_read(paste0(shp_repo, 
                                tracts10_path,
                                "US_tract_2010.shp"),
                         quiet = F)

state_codes <- c(state.abb, "DC")
us_tracts <- map_df(state_codes, ~tracts(state = .x, cb = FALSE))
us_places <- map_df(state_codes, ~places(state = .x, cb = TRUE))

    ## LTDB
tracts10_demogs <- read_csv(paste0(
  wd,
  ltdb_path, 
  "LTDB_Std_2010_fullcount.csv")
)
tracts00_demogs <- read_csv(paste0(
  wd,
  ltdb_path, 
  "LTDB_Std_2000_fullcount.csv")
)
tracts90_demogs <- read_csv(paste0(
  wd,
  ltdb_path, 
  "LTDB_Std_1990_fullcount.csv")
)
tracts80_demogs <- read_csv(paste0(
  wd,
  ltdb_path, 
  "LTDB_Std_1980_fullcount.csv")
)
tracts70_demogs <- read_csv(paste0(
  wd,
  ltdb_path, 
  "LTDB_Std_1970_fullcount.csv")
)
tracts70_supplement <- read_csv(paste0(
  wd,
  ltdb_path,
  "Census_1970_race_supp.csv")
)


  ## ACS 15-19
tracts20_demogs <- read_csv(paste0(
  wd,
  ltdb_path,
  "ACS19.csv")
)

# IDENTIFY NEIGHBORHOOD TRACTS & CITIES----------------------------------------- 

## Create full class set of neighborhoods and tracts
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

## Retrieve 2010 place codes for the neighborhood tracts
placefp10_codes <- tracts00_demogs %>%
  mutate(GEOID = as.character(TRTID10),
         GEOID = str_pad(GEOID, width = 11, side = "left", pad = "0")
         ) %>% 
  relocate(GEOID,
           TRTID10) %>%
  filter(GEOID %in% full_class_tracts$GEOID) %>% 
  select(GEOID, 
         TRTID10,
         placefp10) %>%
  mutate(statefp = str_sub(GEOID, end = 2),
         state_place = str_c(statefp, placefp10, sep = "")
         )

class_places <- left_join(full_class_tracts, placefp10_codes, by = "GEOID")


## Extract all tracts that belong to the class neighborhood places
    ##Yakima was not emplaced in 1970, Yakima not tracted, so fewer tracts in '70.
get_tracts_inplaces <- function(x) { 
  x %>%       
    mutate(TRTID10 = str_pad(TRTID10, width = 11, side = "left", pad = "0"), 
           statefp = str_sub(TRTID10, end = 2),
           state_place = str_c(statefp, placefp10, sep = "")
           ) %>% 
    relocate(TRTID10,
             statefp,
             state_place) %>%
    filter(state_place %in% placefp10_codes$state_place)
}

## 1970
tracts70_supp_modified <- tracts70_supplement %>%
  mutate(GEOID10 = as.character(Geo_FIPS), 
         GEOID10 = str_pad(GEOID10, width = 11, side = "left", pad = "0")
) %>%
  select(OTHER, SPANISH, GEOID10)

tracts70_demogs_places <- tracts70_demogs %>%
  get_tracts_inplaces(.) %>%
  select(TRTID10:ASIAN70) %>%
  select(TRTID10:tract, POP70:ASIAN70) %>%
  mutate(year = "01/31/1970",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("70")),
            .funs = funs(sub("70", "", .))) %>%
  rename(GEOID10 = "TRTID10",
         NHBLK = "BLACK") %>% 
  left_join(., tracts70_supp_modified, by = "GEOID10") %>%
  mutate(NHWHITE = (WHITE - SPANISH)
         ) %>%
  rename(HISPANIC = "SPANISH",
         NHBLACK = "NHBLK") %>%  
  mutate(
         NATIVE = 0,
         NONWHITE = (NHBLACK +  ASIAN + HISPANIC),
         NATIVE = na_if(NATIVE, 0)
  ) %>% 
  relocate(year,
           GEOID10,
           state,
           statefp,
           county,
           state_place,
           tract,
           POP,
           NHWHITE,
           NHBLACK,
           NATIVE, 
           ASIAN, 
           HISPANIC) %>%
  select(-WHITE:-OTHER)


## 1980
tracts80_demogs_places <- tracts80_demogs %>%
  get_tracts_inplaces(.) %>%
  select(TRTID10:tract, POP80:HISP80) %>%
  mutate(year = "01/31/1980",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename(GEOID10 = "TRTID10") %>%
  rename_at(.vars = vars(ends_with("80")),
            .funs = funs(sub("80", "", .))) %>%
  relocate(year,
           GEOID10,
           state,
           statefp,
           county,
           state_place,
           tract) %>% 
  rename(HISPANIC = "HISP",
         NATIVE = "NTV",
         NHBLACK = "NHBLK") %>%  
  mutate(NONWHITE = (NHBLACK + NATIVE + ASIAN + HISPANIC)
  )


## 1990
tracts90_demogs_places <- tracts90_demogs %>%
  get_tracts_inplaces(.) %>%
  select(TRTID10:tract, POP90:HISP90) %>%
  mutate(year = "01/31/1990",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename(GEOID10 = "TRTID10") %>%
  rename_at(.vars = vars(ends_with("90")),
            .funs = funs(sub("90", "", .))) %>%
  relocate(year,
           GEOID10,
           state,
           statefp,
           county,
           state_place,
           tract) %>%
  rename(HISPANIC = "HISP",
         NATIVE = "NTV",
         NHBLACK = "NHBLK") %>%  
  mutate(NONWHITE = (NHBLACK + NATIVE + ASIAN + HISPANIC)
  )


## 2000
tracts00_demogs_places <- tracts00_demogs %>%
  get_tracts_inplaces(.) %>%
  select(TRTID10:tract, POP00:HISP00) %>%
  mutate(year = "01/31/2000",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename(GEOID10 = "TRTID10") %>%
  rename_at(.vars = vars(ends_with("00")),
            .funs = funs(sub("00", "", .))) %>%
  relocate(year,
           GEOID10,
           state,
           statefp,
           county,
           state_place,
           tract) %>%
  rename(HISPANIC = "HISP",
         NATIVE = "NTV",
         NHBLACK = "NHBLK") %>%  
  mutate(NONWHITE = (NHBLACK + NATIVE + ASIAN + HISPANIC)
  )
  

## 2010
tracts_state_place_ids <- tracts00_demogs_places %>%
  select(GEOID10:state_place)

tracts10_demogs_places <- tracts10_demogs %>%
  mutate(tractid = str_pad(tractid, width = 11, side = "left", pad = "0")
         ) %>% 
  filter(tractid %in% tracts00_demogs_places$TRTID10) %>%
  select(tractid:hisp10) %>% 
  left_join(., tracts_state_place_ids, by = c("tractid" = "GEOID10")) %>%
  rename_at(.vars = vars(ends_with("10")),
            .funs = funs(sub("10", "", .))) %>%
  rename(GEOID10 = "tractid") %>% 
  mutate(year = "01/31/2010",
         year = as.Date(year, format = "%m/%d/%Y")
         ) %>% 
  relocate(year,
           GEOID10,
           state,
           statefp,
           county,
           state_place,
           tract) %>%
  rename_with(toupper, pop:hisp) %>%
  rename(HISPANIC = "HISP",
         NATIVE = "NTV",
         NHBLACK = "NHBLK") %>%  
  mutate(NONWHITE = (NHBLACK + NATIVE + ASIAN + HISPANIC)
  )

  
## STACK ALL DATASETS
all_decades_race_tracts <- bind_rows(
  tracts10_demogs_places, 
  tracts00_demogs_places,
  tracts90_demogs_places,
  tracts80_demogs_places,
  tracts70_demogs_places
)

all_decades_race_tracts_class_shp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., all_decades_race_tracts, by = "GEOID10")

# EXPORT CITY-PLACES -----------------------------------------------------------

sf_tracts <- all_decades_race_tracts_class_shp %>%
  filter(county == "San Francisco County")
st_write(sf_tracts, "sf_tracts_race.shp")


# test the tracts shapefile
leaflet() %>%
  addTiles() %>%
  addPolygons(data = class_tracts_in_places_time %>%
                st_transform(., crs = 4326),
              label = ~(county)
  )



