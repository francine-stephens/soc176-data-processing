# ---------------------------------
# PLACE-BASED SEGREGATION MEASURES
# 
# BY: Francine Stephens
# DATE CREATED: 04/26/2020
# LAST UPDATED: 04/27/2020
#----------------------------------

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
setwd("~/Stanford/SOC176/soc176-data-processing/A5")
wd <- getwd()
shp_repo <- "C:/Users/Franc/Documents/Shapefile_Repository/"
tracts10_path <- "2010USA_CensusGeog_Shp/nhgis0005_shapefile_tl2010_us_tract_2010/"
metros10_path <- "2010USA_CensusGeog_Shp/tl_2010_us_cbsa10/"
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
metros <- st_read(paste0(shp_repo, 
                         metros10_path,
                         "tl_2010_us_cbsa10.shp"),
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
         placefp10,
         cbsa10,
         metdiv10) %>%
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
  mutate(NHWHITE = (WHITE - SPANISH),
         SPANISH = if_else(NHWHITE < 0,
                           0,
                           SPANISH),
         NHWHITE = if_else(NHWHITE < 0,
                           WHITE,
                           NHWHITE)
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
  rename(NHWHITE = "NHWHT",
         HISPANIC = "HISP",
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
  rename(NHWHITE = "NHWHT",
         HISPANIC = "HISP",
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
  rename(NHWHITE = "NHWHT",
         HISPANIC = "HISP",
         NATIVE = "NTV",
         NHBLACK = "NHBLK") %>%  
  mutate(NONWHITE = (NHBLACK + NATIVE + ASIAN + HISPANIC)
  )


## 2010
tracts_state_place_ids <- tracts00_demogs_places %>%
  select(GEOID10, statefp, state_place)

tracts10_demogs_places <- tracts10_demogs %>%
  mutate(tractid = str_pad(tractid, width = 11, side = "left", pad = "0")
  ) %>% 
  filter(tractid %in% tracts00_demogs_places$GEOID10) %>%
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
           statefp,
           state_place,
           tract) %>%
  rename_with(toupper, pop:hisp) %>%
  rename(NHWHITE = "NHWHT",
         HISPANIC = "HISP",
         NATIVE = "NTV",
         NHBLACK = "NHBLK") %>%  
  mutate(NONWHITE = (NHBLACK + NATIVE + ASIAN + HISPANIC)
  )


## 2020
tracts_state_place_ids <- tracts00_demogs_places %>%
  select(GEOID10: state_place)

tracts20_demogs_places <- tracts20_demogs %>% 
  mutate(GEOID10 = str_pad(Geo_FIPS, width = 11, side = "left", pad = "0"), 
         tract = as.character(Geo_TRACT),
         tract = paste0("Census Tract ", tract),
         year = "01/31/2020",
         year = as.Date(year, format = "%m/%d/%Y"),
         ASIAN = (AA + NHPI),
         NONWHITE = (NHBLK + NATIVE + ASIAN + HISPANIC) 
  ) %>% 
  select(-Geo_FIPS) %>%
  filter(GEOID10 %in% tracts00_demogs_places$GEOID10) %>% 
  left_join(., tracts_state_place_ids, by =  "GEOID10") %>%
  rename(NHWHITE = "NHWHT",
         NHBLACK = "NHBLK") %>%
  select(-Geo_GEOID: -Geo_TRACT, -AA:-NHPI) %>%
  relocate(year,
           GEOID10,
           statefp,
           state_place,
           tract, 
           state,
           county,
           POP,
           NHWHITE,
           NHBLACK,
           NATIVE,
           ASIAN,
           HISPANIC,
           NONWHITE)


## STACK ALL DATASETS
all_decades_race_tracts <- bind_rows(
  tracts20_demogs_places,
  tracts10_demogs_places, 
  tracts00_demogs_places,
  tracts90_demogs_places,
  tracts80_demogs_places,
  tracts70_demogs_places
)


# EXTRACT PLACES IN REGION------------------------------------------------------
## SET PARAMS TO PULL REGIONS/METROS
bay_area_counties <- c("Marin County",
                       "Sonoma County",
                       "Napa County",
                       "Solano County", 
                       "Contra Costa County",
                       "San Joaquin County",
                       "Alameda County", 
                       "Santa Clara County",
                       "San Mateo County",
                       "San Francisco County", 
                       "Stanislaus County",
                       "Merced County")

msa_ids <- c("12540",
             "31100",
             "40140",
             "19740",
             "19820",
             "28140",
             "38300",
             "49420"
)

metro_names_in_class <- metros %>% 
  st_set_geometry(NULL) %>%
  filter(CBSAFP10 %in% msa_ids) %>%
  select(CBSAFP10, REGION = "NAME10")
write_csv(metros_in_class, "class_metros.csv")


## Extract all tracts that belong to regions
get_tracts_in_bay <- function(x) { 
  x %>%       
    mutate(TRTID10 = str_pad(TRTID10, width = 11, side = "left", pad = "0"), 
           statefp = str_sub(TRTID10, end = 2),
           state_place = str_c(statefp, placefp10, sep = "")
    ) %>% 
    relocate(TRTID10,
             statefp,
             state_place) %>%
    filter(county %in% bay_area_counties)
}

get_tracts_in_regions <- function(x) { 
  x %>%       
    mutate(TRTID10 = str_pad(TRTID10, width = 11, side = "left", pad = "0"), 
           statefp = str_sub(TRTID10, end = 2),
           state_place = str_c(statefp, placefp10, sep = "")
    ) %>% 
    relocate(TRTID10,
             statefp,
             state_place) %>%
    filter(cbsa10 %in% msa_ids)
}


## Set Place names 
## CA
ca_places <- us_places %>%
  st_set_geometry(NULL) %>%
  filter(STATEFP == "06") %>%
  select(STATEFP, PLACEFP, NAME)

## CO
co_places <- us_places %>%
  st_set_geometry(NULL) %>%
  filter(STATEFP == "08") %>%
  select(STATEFP, PLACEFP, NAME)

## MI
mi_places <- us_places %>%
  st_set_geometry(NULL) %>%
  filter(STATEFP == "26") %>%
  select(STATEFP, PLACEFP, NAME)

## MO/KS
ks_mo_places <- us_places %>%
  st_set_geometry(NULL) %>%
  filter(STATEFP == "29" | STATEFP == "20") %>%
  select(STATEFP, PLACEFP, NAME)

## PA
pa_places <- us_places %>%
  st_set_geometry(NULL) %>%
  filter(STATEFP == "42") %>%
  select(STATEFP, PLACEFP, NAME)

## WA
wa_places <- us_places %>%
  st_set_geometry(NULL) %>%
  filter(STATEFP == "53") %>%
  select(STATEFP, PLACEFP, NAME)

# EXTRACT RACIAL DEMOGRAPHICS OF PLACES BY DECADE-------------------------------
## Get tract demographics of entire region by decade
demog_formatting_tract70_level <- function(x) { 
  x %>%       
    rename(GEOID10 = "TRTID10") %>%
    left_join(., tracts70_supp_modified, by = "GEOID10") %>% 
    mutate(year = "01/31/1970",
           year = as.Date(year, format = "%m/%d/%Y")
    ) %>%  
    rename_at(.vars = vars(ends_with("70")),
              .funs = funs(sub("70", "", .))) %>%
    rename(
      NHBLK = "BLACK") %>% 
    mutate(NHWHITE = (WHITE - SPANISH),
           SPANISH = if_else(NHWHITE < 0,
                             0,
                             SPANISH),
           NHWHITE = if_else(NHWHITE < 0,
                             WHITE,
                             NHWHITE)
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
    mutate(place_id = if_else(placefp10 == 99999,
                              paste0(county, " unincorporated place"),
                              as.character(placefp10)
    )
    )
}

demog_formatting_tract80plus_level <- function(x) { 
  x %>% 
rename(GEOID10 = "TRTID10",
       NHWHITE = "NHWHT",
       HISPANIC = "HISP",
       NATIVE = "NTV",
       NHBLACK = "NHBLK") %>%  
  mutate(NONWHITE = (NHBLACK + NATIVE + ASIAN + HISPANIC)
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
           HISPANIC, 
           NONWHITE) %>%
  select(year:cbsa10) %>%
  mutate(place_id = if_else(placefp10 == 99999,
                            paste0(county, " unincorporated place"),
                            as.character(placefp10))
  ) %>% 
  select(year, 
         place_id, 
         placefp10,
         cbsa10,
         POP,
         NHWHITE,
         NHBLACK,
         NATIVE,
         ASIAN,
         HISPANIC,
         NONWHITE, 
         GEOID10) 
}

aggregate_to_places_in_region <- function(x) { 
  x %>%       
    group_by(year, place_id, placefp10) %>%
    summarize_all(sum, na.rm = TRUE) %>% 
    ungroup() %>%
    mutate(
           placefp10 = as.character(placefp10),
           placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")
    )
}


## 1970
tracts70_demogs_bay <- tracts70_demogs %>% 
  get_tracts_in_bay(.) %>%
  demog_formatting_tract70_level(.) %>% 
  select(year, 
         place_id, 
         placefp10,
         POP,
         NHWHITE,
         NHBLACK,
         NATIVE,
         ASIAN,
         HISPANIC,
         NONWHITE)  # ADD cbsa10 for other regions

places70_race_bay <- tracts70_demogs_bay %>%
  aggregate_to_places_in_region(.) %>%
  mutate(NATIVE = na_if(NATIVE, 0)) %>%
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")
  ) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

tracts70_demogs_regions <- tracts70_demogs %>% 
  get_tracts_in_regions(.) %>%
  demog_formatting_tract70_level(.) %>% 
  select(year, 
         place_id, 
         placefp10,
         cbsa10,
         POP,
         NHWHITE,
         NHBLACK,
         NATIVE,
         ASIAN,
         HISPANIC,
         NONWHITE, 
         GEOID10)  %>% 
  filter(cbsa10 != "49420")

places70_race_regions <- tracts70_demogs_bay %>%
  aggregate_to_places_in_region(.) %>%
  mutate(NATIVE = na_if(NATIVE, 0)) %>%
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")
  ) %>%
  relocate(NAME, .after = place_id) %>%
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)


## 1980
tracts80_demogs_bay <- tracts80_demogs %>%
  get_tracts_in_bay(.) %>%
  mutate(year = "01/31/1980",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("80")),
            .funs = funs(sub("80", "", .))) %>%
  demog_formatting_tract80plus_level(.)

tracts80_demogs_regions <- tracts80_demogs %>%
  get_tracts_in_regions(.) %>%
  mutate(year = "01/31/1980",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("80")),
            .funs = funs(sub("80", "", .))) %>%
  demog_formatting_tract80plus_level(.)

places80_race_bay <- tracts80_demogs_bay %>%
  aggregate_to_places_in_region(.) %>%
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")
  ) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

## 1990
tracts90_demogs_bay <- tracts90_demogs %>%
  get_tracts_in_bay(.) %>%
  mutate(year = "01/31/1990",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("90")),
            .funs = funs(sub("90", "", .))) %>%
  demog_formatting_tract80plus_level(.)

tracts90_demogs_regions <- tracts90_demogs %>%
  get_tracts_in_regions(.) %>%
  mutate(year = "01/31/1990",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("90")),
            .funs = funs(sub("90", "", .))) %>%
  demog_formatting_tract80plus_level(.)

places90_race_bay <- tracts90_demogs_bay %>%
  aggregate_to_places_in_region(.) %>%
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")
  ) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

## 2000
tracts00_demogs_bay <- tracts00_demogs %>%
  get_tracts_in_bay(.) %>%
  mutate(year = "01/31/2000",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("00")),
            .funs = funs(sub("00", "", .))) %>%
  demog_formatting_tract80plus_level(.)

tracts00_demogs_regions <- tracts00_demogs %>%
  get_tracts_in_regions(.) %>%
  mutate(year = "01/31/2000",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("00")),
            .funs = funs(sub("00", "", .))) %>%
  demog_formatting_tract80plus_level(.)

places00_race_bay <- tracts00_demogs_bay %>%
  aggregate_to_places_in_region(.) %>% 
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")
  ) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

## 2010
tracts_place_ids_bay <- tracts00_demogs %>%
  get_tracts_in_bay(.) %>% 
  mutate(place_id = if_else(placefp10 == 99999,
                            paste0(county, " unincorporated place"),
                            as.character(placefp10))
  ) %>% 
  select(GEOID10 = "TRTID10", placefp10, place_id) 

tracts_place_ids_regions <- tracts00_demogs %>%
  get_tracts_in_regions(.) %>% 
  mutate(place_id = if_else(placefp10 == 99999,
                            paste0(county, " unincorporated place"),
                            as.character(placefp10))
  ) %>% 
  select(GEOID10 = "TRTID10", placefp10, place_id, cbsa10) 

tracts10_demogs_bay <- tracts10_demogs %>%
  mutate(tractid = str_pad(tractid, width = 11, side = "left", pad = "0")
  ) %>% 
  filter(tractid %in% tracts_place_ids_bay$GEOID10)  %>%
  select(tractid:hisp10) %>% 
  left_join(., tracts_place_ids_bay, by = c("tractid" = "GEOID10")) %>%
  rename_at(.vars = vars(ends_with("10")),
            .funs = funs(sub("10", "", .))) %>%
  rename(GEOID10 = "tractid") %>%
  mutate(year = "01/31/2010",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%
  rename_with(toupper, pop:hisp) %>%
  rename(NHWHITE = "NHWHT",
         HISPANIC = "HISP",
         NATIVE = "NTV",
         NHBLACK = "NHBLK") %>%
  mutate(NONWHITE = (NHBLACK + NATIVE + ASIAN + HISPANIC)
  ) %>%
  relocate(year,
           GEOID10,
           place_id,
           placefp,
           POP,
           NHWHITE,
           NHBLACK,
           NATIVE, 
           ASIAN, 
           HISPANIC, 
           NONWHITE) %>%
  select(-GEOID10, -state:-tract) %>%
  rename(placefp10 = "placefp")

tracts10_demogs_regions <- tracts10_demogs %>%
  mutate(tractid = str_pad(tractid, width = 11, side = "left", pad = "0")
  ) %>% 
  filter(tractid %in% tracts_place_ids_regions$GEOID10)  %>%
  select(tractid:hisp10) %>% 
  left_join(., tracts_place_ids_regions, by = c("tractid" = "GEOID10")) %>%
  rename_at(.vars = vars(ends_with("10")),
            .funs = funs(sub("10", "", .))) %>%
  rename(GEOID10 = "tractid") %>%
  mutate(year = "01/31/2010",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%
  rename_with(toupper, pop:hisp) %>%
  rename(NHWHITE = "NHWHT",
         HISPANIC = "HISP",
         NATIVE = "NTV",
         NHBLACK = "NHBLK") %>%
  mutate(NONWHITE = (NHBLACK + NATIVE + ASIAN + HISPANIC)
  ) %>%
  relocate(year,
           place_id,
           placefp,
           cbsa,
           POP,
           NHWHITE,
           NHBLACK,
           NATIVE, 
           ASIAN, 
           HISPANIC, 
           NONWHITE) %>%
  select(-state:-tract) %>%
  rename(placefp10 = "placefp",
         cbsa10 = "cbsa")


places10_race_bay <- tracts10_demogs_bay %>%
  aggregate_to_places_in_region(.) %>% 
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")
  ) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

## 2020
tracts20_demogs_bay <- tracts20_demogs %>% 
  mutate(GEOID10 = str_pad(Geo_FIPS, width = 11, side = "left", pad = "0"), 
         tract = as.character(Geo_TRACT),
         tract = paste0("Census Tract ", tract),
         year = "01/31/2020",
         year = as.Date(year, format = "%m/%d/%Y"),
         ASIAN = (AA + NHPI),
         NONWHITE = (NHBLK + NATIVE + ASIAN + HISPANIC) 
  ) %>% 
  right_join(., tracts_place_ids_bay, by =  "GEOID10") %>%
  rename(NHWHITE = "NHWHT",
         NHBLACK = "NHBLK") %>%
  select(-Geo_FIPS, -AA:-NHPI, -Geo_GEOID, -Geo_TRACT, -tract) %>%
  relocate(year,
           GEOID10,
           place_id, 
           placefp10,
           POP,
           NHWHITE,
           NHBLACK,
           NATIVE, 
           ASIAN, 
           HISPANIC, 
           NONWHITE)

tracts20_demogs_regions <- tracts20_demogs %>% 
  mutate(GEOID10 = str_pad(Geo_FIPS, width = 11, side = "left", pad = "0"), 
         tract = as.character(Geo_TRACT),
         tract = paste0("Census Tract ", tract),
         year = "01/31/2020",
         year = as.Date(year, format = "%m/%d/%Y"),
         ASIAN = (AA + NHPI),
         NONWHITE = (NHBLK + NATIVE + ASIAN + HISPANIC) 
  ) %>% 
  right_join(., tracts_place_ids_regions, by =  "GEOID10") %>%
  rename(NHWHITE = "NHWHT",
         NHBLACK = "NHBLK")  %>%
  relocate(year,
           place_id, 
           placefp10,
           cbsa10,
           POP,
           NHWHITE,
           NHBLACK,
           NATIVE, 
           ASIAN, 
           HISPANIC, 
           NONWHITE) %>%
  select(-Geo_FIPS, -AA:-NHPI, -Geo_GEOID, -Geo_TRACT, -tract)

places20_race_bay <- tracts20_demogs_bay %>%
  select(-GEOID10) %>%
  aggregate_to_places_in_region(.) %>%
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")
  ) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)


## STACK ALL DATASETS
all_decades_race_places_bay <- bind_rows(
  places70_race_bay,
  places80_race_bay,
  places90_race_bay,
  places00_race_bay,
  places10_race_bay,
  places20_race_bay
)

  ## OTHER REGIONS STACK ALL TRACT LEVEL DATASETS
all_decades_race_tracts_regions <- bind_rows(
  tracts70_demogs_regions,
  tracts80_demogs_regions,
  tracts90_demogs_regions,
  tracts00_demogs_regions,
  tracts10_demogs_regions,
  tracts20_demogs_regions
)

  ## AGGREGATE TO PLACES
# state_place_ids_regions <- tracts_place_ids_regions %>%
#   mutate(placefp10 = as.character(placefp10),
#   placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0"),
#   statefp = substr(GEOID10, start = 1, stop = 2)
# ) %>%
#   select(-GEOID10) %>%
#   distinct(placefp10, place_id, statefp)

all_decades_race_places_regions <- all_decades_race_tracts_regions %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0"),
         statefp = substr(GEOID10, start = 1, stop = 2)
         ) %>%
  select(-GEOID10) %>%
  group_by(year, statefp, placefp10, place_id, cbsa10) %>%
  summarize_all(sum, na.rm = TRUE) %>% 
  ungroup()


## GET SHAPEFILE OF PLACES IN REGION
# BAY
all_decades_race_places_bay_shp <- census_tracts %>% 
  select(GEOID10) %>% 
  right_join(., tracts_place_ids_bay, by = "GEOID10") %>%
  group_by(place_id, placefp10) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  summarize(n_tracts = n()) %>%
  right_join(., all_decades_race_places_bay, by = c("place_id", "placefp10")
  )
st_write(all_decades_race_places_shp, "Bay_Area_Region_places_race.shp")


# BAKERSFIELD
baker_all_decades_places <- all_decades_race_places_regions %>%
  filter(cbsa10 == 12540) %>%
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)
  
baker_all_decades_race_places_shp <- census_tracts %>% 
  select(GEOID10) %>% 
  right_join(., tracts_place_ids_regions, by = "GEOID10") %>%
  group_by(place_id, placefp10) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  summarize(n_tracts = n()) %>%
  right_join(., baker_all_decades_places, by = c("place_id", "placefp10")
  )
st_write(baker_all_decades_race_places_shp, "Bakersfield_Region_places_race.shp")

# LA
la_all_decades_places <- all_decades_race_places_regions %>%
  filter(cbsa10 == 31100) %>%
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

la_all_decades_race_places_shp <- census_tracts %>% 
  select(GEOID10) %>% 
  mutate(CA = str_sub(GEOID10, end = 2)) %>%
  filter(CA == "06") %>%
  select(-CA) %>%
  right_join(., tracts_place_ids_regions, by = "GEOID10") %>%
  group_by(place_id, placefp10) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  summarize(n_tracts = n()) %>%
  right_join(., la_all_decades_places, by = c("place_id", "placefp10")
  )
st_write(la_all_decades_race_places_shp, "Los_Angeles_Region_places_race.shp")

# RIVERSIDE
river_all_decades_places <- all_decades_race_places_regions %>%
  filter(cbsa10 == 40140) %>%
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

river_all_decades_race_places_shp <- census_tracts %>% 
  select(GEOID10) %>% 
  mutate(CA = str_sub(GEOID10, end = 2)) %>%
  select(-CA) %>%
  right_join(., tracts_place_ids_regions, by = "GEOID10") %>%
  group_by(place_id, placefp10) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  summarize(n_tracts = n()) %>%
  right_join(., river_all_decades_places, by = c("place_id", "placefp10")
  )
st_write(river_all_decades_race_places_shp, "Riverside_Region_places_race.shp")


# DENVER
denver_all_decades_places <- all_decades_race_places_regions %>%
  filter(cbsa10 == 19740) %>%
  left_join(., co_places, by = c("placefp10" = "PLACEFP")) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

denver_all_decades_race_places_shp <- census_tracts %>% 
  select(GEOID10) %>% 
  mutate(CO = str_sub(GEOID10, end = 2)) %>%
  filter(CO == "08") %>%
  select(-CO) %>%
  right_join(., tracts_place_ids_regions, by = "GEOID10") %>%
  group_by(place_id, placefp10) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  summarize(n_tracts = n()) %>%
  right_join(., denver_all_decades_places, by = c("place_id", "placefp10")
  )
st_write(denver_all_decades_race_places_shp, "Denver_Region_places_race.shp")

# DETROIT
detroit_all_decades_places <- all_decades_race_places_regions %>%
  filter(cbsa10 == 19820) %>%
  left_join(., mi_places, by = c("placefp10" = "PLACEFP")) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

detroit_all_decades_race_places_shp <- census_tracts %>% 
  select(GEOID10) %>% 
  mutate(MI = str_sub(GEOID10, end = 2)) %>%
  filter(MI == "26") %>%
  select(-MI) %>%
  right_join(., tracts_place_ids_regions, by = "GEOID10") %>%
  group_by(place_id, placefp10) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  summarize(n_tracts = n()) %>%
  right_join(., detroit_all_decades_places, by = c("place_id", "placefp10"))
st_write(detroit_all_decades_race_places_shp, "Detroit_Region_places_race.shp")
             

# KC
kc_all_decades_places <- all_decades_race_places_regions %>%
  filter(cbsa10 == 28140) %>%
  left_join(., ks_mo_places, by = c("placefp10" = "PLACEFP")) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

kc_all_decades_race_places_shp <- census_tracts %>% 
  select(GEOID10) %>% 
  mutate(MO_KS = str_sub(GEOID10, end = 2)) %>%
  filter(MO_KS == "29" | MO_KS == "20") %>%
  select(-MO_KS) %>%
  right_join(., tracts_place_ids_regions, by = "GEOID10") %>%
  group_by(place_id, placefp10) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  summarize(n_tracts = n()) %>%
  right_join(., kc_all_decades_places, by = c("place_id", "placefp10")
  )
st_write(kc_all_decades_race_places_shp, "Kansas_City_Region_places_race.shp")

# PITT
pitt_all_decades_places <- all_decades_race_places_regions %>%
  filter(cbsa10 == 38300) %>%
  left_join(., pa_places, by = c("placefp10" = "PLACEFP")) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

pitt_all_decades_race_places_shp <- census_tracts %>% 
  select(GEOID10) %>% 
  mutate(PA = str_sub(GEOID10, end = 2)) %>%
  filter(PA == "42") %>%
  select(-PA) %>%
  right_join(., tracts_place_ids_regions, by = "GEOID10") %>%
  group_by(place_id, placefp10) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  summarize(n_tracts = n()) %>%
  right_join(., pitt_all_decades_places, by = c("place_id", "placefp10")
  )
st_write(pitt_all_decades_race_places_shp, "Pittsburgh_Region_places_race.shp")

# YAKIMA
yakima_all_decades_places <- all_decades_race_places_regions %>%
  filter(cbsa10 == 49420) %>%
  left_join(., wa_places, by = c("placefp10" = "PLACEFP")) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP)

yakima_all_decades_race_places_shp <- census_tracts %>% 
  select(GEOID10) %>% 
  mutate(WA = str_sub(GEOID10, end = 2)) %>%
  filter(WA == "53") %>%
  select(-WA) %>%
  right_join(., tracts_place_ids_regions, by = "GEOID10") %>%
  group_by(place_id, placefp10) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  summarize(n_tracts = n()) %>%
  right_join(., yakima_all_decades_places, by = c("place_id", "placefp10")
  )
st_write(yakima_all_decades_race_places_shp, "Yakima_Region_places_race.shp")


# REGION DISSIMILARITY----------------------------------------------------------

## COMPUTE REGION RACE TOTALS
all_decades_race_region <- all_decades_race_places %>%
  select(-place_id:-placefp10) %>%
  group_by(year) %>%
  summarize_all(sum, na.rm = TRUE) %>%
  rename_at(vars(-year), function(x) paste0(x,"_REGION"))


## COMPUTE DISSIMILARITY
all_decades_places_dissim_scores <- all_decades_race_places %>% 
  select(year, placefp10, NAME, POP:NONWHITE) %>%
  left_join(., all_decades_race_region, by = c("year")) %>%
  mutate(D_NWW=abs(NONWHITE/NONWHITE_REGION - NHWHITE/NHWHITE_REGION),
         D_BW=abs(NHBLACK/NHBLACK_REGION - NHWHITE/NHWHITE_REGION),
         D_AW=abs(ASIAN/ASIAN_REGION - NHWHITE/NHWHITE_REGION),
         D_HW=abs(HISPANIC/HISPANIC_REGION - NHWHITE/NHWHITE_REGION),
         D_BA=abs(NHBLACK/NHBLACK_REGION - ASIAN/ASIAN_REGION),
         D_BH=abs(NHBLACK/NHBLACK_REGION - HISPANIC/HISPANIC_REGION),
         D_AH=abs(ASIAN/ASIAN_REGION - HISPANIC/HISPANIC_REGION)
  ) %>%
  group_by(year, placefp10, NAME) %>%
  summarise(Dis_NWW = .5*sum(D_NWW, na.rm=T),
            Dis_BW = .5*sum(D_BW, na.rm=T),
            Dis_AW = .5*sum(D_AW, na.rm=T),
            Dis_HW= .5*sum(D_HW, na.rm=T),
            Dis_BA = .5*sum(D_BA, na.rm=T),
            Dis_BH = .5*sum(D_BH, na.rm=T),
            Dis_AH = .5*sum(D_AH, na.rm=T)
  ) %>% 
  ungroup() %>%
  mutate_at(vars(Dis_NWW:Dis_AH),
            .funs = funs(. * 100)) %>% 
  arrange(placefp10, NAME, year) %>%
  relocate(placefp10, NAME, year) 



# REGION DIVERGENCE-------------------------------------------------------------

all_decades_race_places_prop <- all_decades_race_places %>%
  mutate_at(vars(NHWHITE:HISPANIC), funs("PR" = (./POP))) %>%
  mutate(OTHER_PR = 1.0 - (NHWHITE_PR + NHBLACK_PR + NATIVE_PR + ASIAN_PR + HISPANIC_PR)
  )

all_decades_race_places_seg <- all_decades_race_places_prop

## 2020
bay_places_pr_2020 <- all_decades_race_places_prop %>%
  filter(year > "2020-01-01") %>%
  mutate(divergence = divergence(NHWHITE_PR,
                                 NHBLACK_PR, 
                                 NATIVE_PR, 
                                 ASIAN_PR, 
                                 HISPANIC_PR, 
                                 population = POP,
                                 summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    divergence = na_if(divergence, 0),
    divergence = if_else(divergence <0, 
                         0, 
                         divergence)
  ) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00))) %>% 
  left_join(., all_decades_places_dissim_scores, by = c("placefp10", "year")) %>%
  mutate_at(vars(Dis_NWW:Dis_AH),
            funs(if_else(. > 1.0,
                         0.95,
                         .))
  )

bay_places_seg_2020 <- all_decades_race_places_shp %>% 
  filter(year == "2020-01-31") %>% 
  select(-year,-POP,-NHWHITE:-NONWHITE) %>%
  left_join(., bay_places_pr_2020, by = c("place_id")) %>%
  select(place_id,
         placefp10 = "placefp10.x",
         n_tracts, 
         NAME, 
         year,
         POP,
         NHWHITE_PR:divergence,
         Dis_NWW:Dis_AH) %>%
  distinct(geometry, .keep_all = TRUE)
st_write(bay_places_seg_2020, "bay_region_places_segregation_2020.shp")


## 2010
bay_places_pr_2010 <- all_decades_race_places_prop %>%
  filter(year == "2010-01-31") %>%
  mutate(divergence = divergence(NHWHITE_PR,
                                 NHBLACK_PR, 
                                 NATIVE_PR, 
                                 ASIAN_PR, 
                                 HISPANIC_PR, 
                                 population = POP,
                                 summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    divergence = na_if(divergence, 0),
    divergence = if_else(divergence <0, 
                         0, 
                         divergence)
  ) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00))) %>% 
  left_join(., all_decades_places_dissim_scores, by = c("placefp10", "year")) %>%
  mutate_at(vars(Dis_NWW:Dis_AH),
            funs(if_else(. > 1.0,
                         0.95,
                         .))
  )

bay_places_seg_2010 <- all_decades_race_places_shp %>% 
  filter(year == "2010-01-31") %>% 
  select(-year,-POP,-NHWHITE:-NONWHITE) %>%
  left_join(., bay_places_pr_2010, by = c("place_id")) %>%
  select(place_id,
         placefp10 = "placefp10.x",
         n_tracts, 
         NAME, 
         year,
         POP,
         NHWHITE_PR:divergence,
         Dis_NWW:Dis_AH) %>%
  distinct(geometry, .keep_all = TRUE)
st_write(bay_places_seg_2010, "bay_region_places_segregation_2010.shp")




bay_places_pr_2000 <- all_decades_race_places_prop %>%
  filter(year == "2000-01-31") %>%
  mutate(divergence = divergence(NHWHITE_PR,
                                 NHBLACK_PR, 
                                 NATIVE_PR, 
                                 ASIAN_PR, 
                                 HISPANIC_PR, 
                                 population = POP,
                                 summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    divergence = na_if(divergence, 0),
    divergence = if_else(divergence <0, 
                         0, 
                         divergence)
  )

bay_places_pr_1990 <- all_decades_race_places_prop %>%
  filter(year == "1990-01-31") %>%
  mutate(divergence = divergence(NHWHITE_PR,
                                 NHBLACK_PR, 
                                 NATIVE_PR, 
                                 ASIAN_PR, 
                                 HISPANIC_PR, 
                                 population = POP,
                                 summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    divergence = na_if(divergence, 0),
    divergence = if_else(divergence <0, 
                         0, 
                         divergence)
  )

bay_places_pr_1980 <- all_decades_race_places_prop %>%
  filter(year == "1980-01-31") %>%
  mutate(divergence = divergence(NHWHITE_PR,
                                 NHBLACK_PR, 
                                 NATIVE_PR, 
                                 ASIAN_PR, 
                                 HISPANIC_PR, 
                                 population = POP,
                                 summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    divergence = na_if(divergence, 0),
    divergence = if_else(divergence <0, 
                         0, 
                         divergence)
  )

bay_places_pr_1970 <- all_decades_race_places_prop %>%
  filter(year == "1970-01-31") %>%
  mutate(divergence = divergence(NHWHITE_PR,
                                 NHBLACK_PR, 
                                 NATIVE_PR, 
                                 ASIAN_PR, 
                                 HISPANIC_PR, 
                                 population = POP,
                                 summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    divergence = na_if(divergence, 0),
    divergence = if_else(divergence <0, 
                         0, 
                         divergence)
  )

bay_region_all_decades_seg <- bind_rows(bay_places_pr_1970,
                                        bay_places_pr_1980,
                                        bay_places_pr_1990,
                                        bay_places_pr_2000,
                                        bay_places_pr_2010,
                                        bay_places_pr_2020
)


bay_region_shp <-  census_tracts %>% 
  select(GEOID10) %>% 
  right_join(., tracts_place_ids_region, by = "GEOID10") %>%
  group_by(place_id, placefp10) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  summarize(n_tracts = n()) 


st_write(bay_region_seg_1970, "bay_region_places_segregation_1970.shp")



# View shapefile
leaflet() %>%
  addTiles() %>%
  addPolygons(data = bay_places_seg_2010 %>% 
                st_transform(., crs = 4326),
              label = ~(NAME)
  )
