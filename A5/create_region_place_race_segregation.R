# ------------------------------------------------------------
# PLACE-BASED RACIAL COMPOSITION & SEGREGATION MEASURES
# 
# BY: Francine Stephens
# DATE CREATED: 04/26/2020
# LAST UPDATED: 04/28/2020
#-------------------------------------------------------------

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
         NONWHITE,
         GEOID10)  # ADD cbsa10 for other regions

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


## 1980
tracts80_demogs_bay <- tracts80_demogs %>%
  get_tracts_in_bay(.) %>%
  mutate(year = "01/31/1980",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("80")),
            .funs = funs(sub("80", "", .))) %>%
  demog_formatting_tract80plus_level(.) %>%
  select(-cbsa10)

tracts80_demogs_regions <- tracts80_demogs %>%
  get_tracts_in_regions(.) %>%
  mutate(year = "01/31/1980",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("80")),
            .funs = funs(sub("80", "", .))) %>%
  demog_formatting_tract80plus_level(.)

places80_race_bay <- tracts80_demogs_bay %>% 
  select(-GEOID10) %>%
  aggregate_to_places_in_region(.) %>%
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")
  ) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP, -cbsa10)

## 1990
tracts90_demogs_bay <- tracts90_demogs %>%
  get_tracts_in_bay(.) %>%
  mutate(year = "01/31/1990",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("90")),
            .funs = funs(sub("90", "", .))) %>%
  demog_formatting_tract80plus_level(.) %>%
  select(-cbsa10)

tracts90_demogs_regions <- tracts90_demogs %>%
  get_tracts_in_regions(.) %>%
  mutate(year = "01/31/1990",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("90")),
            .funs = funs(sub("90", "", .))) %>%
  demog_formatting_tract80plus_level(.)

places90_race_bay <- tracts90_demogs_bay %>%
  select(-GEOID10) %>%
  aggregate_to_places_in_region(.) %>%
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")
  ) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP, -cbsa10)

## 2000
tracts00_demogs_bay <- tracts00_demogs %>%
  get_tracts_in_bay(.) %>%
  mutate(year = "01/31/2000",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("00")),
            .funs = funs(sub("00", "", .))) %>%
  demog_formatting_tract80plus_level(.) %>%
  select(-cbsa10)

tracts00_demogs_regions <- tracts00_demogs %>%
  get_tracts_in_regions(.) %>%
  mutate(year = "01/31/2000",
         year = as.Date(year, format = "%m/%d/%Y")
  ) %>%  
  rename_at(.vars = vars(ends_with("00")),
            .funs = funs(sub("00", "", .))) %>%
  demog_formatting_tract80plus_level(.)

places00_race_bay <- tracts00_demogs_bay %>% 
  select(-GEOID10) %>%
  aggregate_to_places_in_region(.) %>% 
  left_join(., ca_places, by = c("placefp10" = "PLACEFP")
  ) %>%
  relocate(NAME, .after = place_id) %>% 
  mutate(NAME = coalesce(NAME, place_id)) %>%
  select(-STATEFP, -cbsa10)

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
           place_id,
           placefp,
           POP,
           NHWHITE,
           NHBLACK,
           NATIVE, 
           ASIAN, 
           HISPANIC, 
           NONWHITE, 
           GEOID10) %>%
  select(-state:-tract) %>%
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
           place_id, 
           placefp10,
           POP,
           NHWHITE,
           NHBLACK,
           NATIVE, 
           ASIAN, 
           HISPANIC, 
           NONWHITE,
           GEOID10)

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
all_decades_race_tracts_bay <-  bind_rows(
  tracts70_demogs_bay,
  tracts80_demogs_bay,
  tracts90_demogs_bay,
  tracts00_demogs_bay,
  tracts10_demogs_bay,
  tracts20_demogs_bay
)
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
compute_race_totals_for_region_by_decade <- function(x) { 
  x %>%       
    select(-place_id, -placefp10, -statefp, -NAME, -cbsa10) %>%
    group_by(year) %>%
    summarize_all(sum, na.rm = TRUE) %>%
    rename_at(vars(-year), function(x) paste0(x,"_REGION"))
}

all_decades_race_bay <- all_decades_race_places_bay %>%
  compute_race_totals_for_region_by_decade(.)

all_decades_race_la <- la_all_decades_places %>%
  compute_race_totals_for_region_by_decade(.) %>%
  filter(!is.na(year))

all_decades_race_baker <- baker_all_decades_places %>%
  compute_race_totals_for_region_by_decade(.)

all_decades_race_river <- river_all_decades_places %>%
  compute_race_totals_for_region_by_decade(.)

all_decades_race_denver <- denver_all_decades_places %>%
  compute_race_totals_for_region_by_decade(.)

all_decades_race_detroit <- detroit_all_decades_places %>%
  compute_race_totals_for_region_by_decade(.)

all_decades_race_kc <- kc_all_decades_places %>%
  compute_race_totals_for_region_by_decade(.)

all_decades_race_pitt <- pitt_all_decades_places %>%
  compute_race_totals_for_region_by_decade(.)

all_decades_race_yakima <- yakima_all_decades_places %>%
  compute_race_totals_for_region_by_decade(.)


## RENAME PLACE RACE TOTALS
rename_place_totals <- function(x) { 
  x %>%       
    rename_at(vars(POP:NONWHITE), function(x) paste0(x,"_PLACE"))
}

all_decades_race_places_bay_d <- all_decades_race_places_bay %>%
  rename_place_totals(.)

all_decades_race_places_la_d <- la_all_decades_places %>%
  rename_place_totals(.)

all_decades_race_places_baker_d <- baker_all_decades_places %>%
  rename_place_totals(.)

all_decades_race_places_river_d <- river_all_decades_places %>%
  rename_place_totals(.)

all_decades_race_places_denver_d <- denver_all_decades_places %>%
  rename_place_totals(.)

all_decades_race_places_detroit_d <- detroit_all_decades_places %>%
  rename_place_totals(.)

all_decades_race_places_kc_d <- kc_all_decades_places %>%
  rename_place_totals(.)

all_decades_race_places_pitt_d <- pitt_all_decades_places %>%
  rename_place_totals(.)

all_decades_race_places_yakima_d <- yakima_all_decades_places %>%
  rename_place_totals(.)

## COMPUTE DISSIMILARITY
compute_dissim_for_place_by_decade <- function(x) { 
  x %>%       
  mutate(D_NWW=abs(NONWHITE/NONWHITE_PLACE - NHWHITE/NHWHITE_PLACE),
         D_BW=abs(NHBLACK/NHBLACK_PLACE - NHWHITE/NHWHITE_PLACE),
         D_AW=abs(ASIAN/ASIAN_PLACE - NHWHITE/NHWHITE_PLACE),
         D_HW=abs(HISPANIC/HISPANIC_PLACE - NHWHITE/NHWHITE_PLACE),
         D_BA=abs(NHBLACK/NHBLACK_PLACE - ASIAN/ASIAN_PLACE),
         D_BH=abs(NHBLACK/NHBLACK_PLACE - HISPANIC/HISPANIC_PLACE),
         D_AH=abs(ASIAN/ASIAN_PLACE - HISPANIC/HISPANIC_PLACE)
  ) %>%
  group_by(year, placefp10, NAME) %>%
  summarise(Dis_NWW = .5*sum(D_NWW, na.rm=F),
            Dis_BW = .5*sum(D_BW, na.rm=F),
            Dis_AW = .5*sum(D_AW, na.rm=F),
            Dis_HW= .5*sum(D_HW, na.rm=F),
            Dis_BA = .5*sum(D_BA, na.rm=F),
            Dis_BH = .5*sum(D_BH, na.rm=F),
            Dis_AH = .5*sum(D_AH, na.rm=F)
  ) %>% 
  ungroup() %>%
  mutate_at(vars(Dis_NWW:Dis_AH),
            .funs = funs(. * 100)) %>%
  arrange(placefp10, NAME, year) %>%
  relocate(placefp10, NAME, year) 
}

# BAY
bay_places_dissim <- all_decades_race_tracts_bay  %>%
  select(year, place_id, placefp10, POP:NONWHITE) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  left_join(., all_decades_race_places_bay_d, by = c("year", "place_id", "placefp10")) %>% 
  compute_dissim_for_place_by_decade(.) %>%
  mutate(NAME = if_else(NAME == "83215",
                        "Waldon CDP",
                        NAME))
# LA
la_places_dissim <- all_decades_race_tracts_regions %>%
  filter(cbsa10 == 31100) %>% 
  select(year, place_id, placefp10, POP:NONWHITE) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  left_join(., all_decades_race_places_la_d, by = c("year", "place_id", "placefp10")) %>% 
  compute_dissim_for_place_by_decade(.)

# BAKERSFIELD
baker_places_dissim <- all_decades_race_tracts_regions %>%
  filter(cbsa10 == 12540) %>% 
  select(year, place_id, placefp10, POP:NONWHITE) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  left_join(., all_decades_race_places_baker_d, by = c("year", "place_id", "placefp10")) %>% 
  compute_dissim_for_place_by_decade(.)

# RIVERSIDE
river_places_dissim <- all_decades_race_tracts_regions %>%
  filter(cbsa10 == 40140) %>% 
  select(year, place_id, placefp10, POP:NONWHITE) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  left_join(., all_decades_race_places_river_d, by = c("year", "place_id", "placefp10")) %>% 
  compute_dissim_for_place_by_decade(.)

# DENVER
denver_places_dissim <- all_decades_race_tracts_regions %>%
  filter(cbsa10 == 19740) %>% 
  select(year, place_id, placefp10, POP:NONWHITE) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  left_join(., all_decades_race_places_denver_d, by = c("year", "place_id", "placefp10")) %>% 
  compute_dissim_for_place_by_decade(.)

# DETROIT 
detroit_places_dissim <- all_decades_race_tracts_regions %>%
  filter(cbsa10 == 19820) %>% 
  select(year, place_id, placefp10, POP:NONWHITE) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  left_join(., all_decades_race_places_detroit_d, by = c("year", "place_id", "placefp10")) %>% 
  compute_dissim_for_place_by_decade(.)

# KC
kc_places_dissim <- all_decades_race_tracts_regions %>%
  filter(cbsa10 == 28140) %>% 
  select(year, place_id, placefp10, POP:NONWHITE) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  left_join(., all_decades_race_places_kc_d, by = c("year", "place_id", "placefp10")) %>% 
  compute_dissim_for_place_by_decade(.)

# PITT
pitt_places_dissim <- all_decades_race_tracts_regions %>%
  filter(cbsa10 == 38300) %>% 
  select(year, place_id, placefp10, POP:NONWHITE) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  left_join(., all_decades_race_places_pitt_d, by = c("year", "place_id", "placefp10")) %>% 
  compute_dissim_for_place_by_decade(.)

# YAKIMA
yakima_places_dissim <- all_decades_race_tracts_regions %>%
  filter(cbsa10 == 49420) %>% 
  select(year, place_id, placefp10, POP:NONWHITE) %>% 
  mutate(placefp10 = as.character(placefp10),
         placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) %>%
  left_join(., all_decades_race_places_yakima_d, by = c("year", "place_id", "placefp10")) %>% 
  compute_dissim_for_place_by_decade(.)


# REGION DIVERGENCE-------------------------------------------------------------
create_race_proportions <- function(x) { 
  x %>% 
    mutate_at(vars(NHWHITE:HISPANIC), funs("PR" = (./POP))) %>%
    mutate(OTHER_PR = 1.0 - (NHWHITE_PR + NHBLACK_PR + NATIVE_PR + ASIAN_PR + HISPANIC_PR)
           )
}

filter70 <- function(x) { 
  x %>% 
    filter(year == "1970-01-31")
}

filter80 <- function(x) { 
  x %>% 
    filter(year == "1980-01-31")
}

filter90 <- function(x) { 
  x %>% 
    filter(year == "1990-01-31")
}

filter00 <- function(x) { 
  x %>% 
    filter(year == "2000-01-31")
}

filter10 <- function(x) { 
  x %>% 
    filter(year == "2010-01-31")
}

filter20 <- function(x) { 
  x %>% 
    filter(year == "2020-01-31")
}

compute_divergence <- function(x) { 
  x %>% 
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
    mutate(divergence = rescale(divergence, to = c(0, 1.00))) 
}

## BAY AREA 
all_decades_race_places_bay_prop <- all_decades_race_places_bay %>% 
  create_race_proportions(.)
  #70
bay_places_seg70 <- all_decades_race_places_bay_prop %>% 
  filter70(.) %>%
  compute_divergence(.) %>%
  left_join(., bay_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

bay_places_seg70_shp <- all_decades_race_places_bay_shp %>%
  filter70(.) %>% 
  select(place_id:NAME) %>%
  right_join(., bay_places_seg70, by = c("placefp10", "place_id", "year", "NAME"))
st_write(bay_places_seg70_shp, "bay_area_places_segregation_1970.shp")

  #80
bay_places_seg80 <- all_decades_race_places_bay_prop %>% 
  filter80(.) %>%
  compute_divergence(.) %>%
  left_join(., bay_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

bay_places_seg80_shp <- all_decades_race_places_bay_shp %>%
  filter80(.) %>% 
  select(place_id:NAME) %>%
  right_join(., bay_places_seg80, by = c("placefp10", "place_id", "year", "NAME"))
st_write(bay_places_seg80_shp, "bay_area_places_segregation_1980.shp")

  #90
bay_places_seg90 <- all_decades_race_places_bay_prop %>% 
  filter90(.) %>%
  compute_divergence(.) %>%
  left_join(., bay_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

bay_places_seg90_shp <- all_decades_race_places_bay_shp %>%
  filter90(.) %>% 
  select(place_id:NAME) %>%
  right_join(., bay_places_seg90, by = c("placefp10", "place_id", "year", "NAME"))
st_write(bay_places_seg90_shp, "bay_area_places_segregation_1990.shp")

  #00
bay_places_seg00 <- all_decades_race_places_bay_prop %>% 
  filter00(.) %>%
  compute_divergence(.) %>%
  left_join(., bay_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

bay_places_seg00_shp <- all_decades_race_places_bay_shp %>%
  filter00(.) %>% 
  select(place_id:NAME) %>%
  right_join(., bay_places_seg00, by = c("placefp10", "place_id", "year", "NAME"))
st_write(bay_places_seg00_shp, "bay_area_places_segregation_2000.shp")

  #10
bay_places_seg10 <- all_decades_race_places_bay_prop %>% 
  filter10(.) %>%
  compute_divergence(.) %>%
  left_join(., bay_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

bay_places_seg10_shp <- all_decades_race_places_bay_shp %>%
  filter10(.) %>% 
  select(place_id:NAME) %>%
  right_join(., bay_places_seg10, by = c("placefp10", "place_id", "year", "NAME"))
st_write(bay_places_seg10_shp, "bay_area_places_segregation_2010.shp")

  #20
bay_places_seg20 <- all_decades_race_places_bay_prop %>% 
  filter20(.) %>%
  compute_divergence(.) %>%
  left_join(., bay_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

bay_places_seg20_shp <- all_decades_race_places_bay_shp %>%
  filter20(.) %>% 
  select(place_id:NAME) %>%
  right_join(., bay_places_seg20, by = c("placefp10", "place_id", "year", "NAME"))
st_write(bay_places_seg20_shp, "bay_area_places_segregation_2020.shp")

########
# LA 
########
all_decades_race_places_la_prop <- la_all_decades_places %>% 
  create_race_proportions(.)

#70
la_places_seg70 <- all_decades_race_places_la_prop %>% 
  filter70(.) %>%
  compute_divergence(.) %>%
  left_join(., la_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

la_places_seg70_shp <- la_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter70(.) %>% 
  select(place_id:NAME) %>%
  right_join(., la_places_seg70, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(la_places_seg70_shp, "Los_Angeles_places_segregation_1970.shp")

#80
la_places_seg80 <- all_decades_race_places_la_prop %>% 
  filter80(.) %>%
  compute_divergence(.) %>%
  left_join(., la_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

la_places_seg80_shp <- la_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter80(.) %>% 
  select(place_id:NAME) %>%
  right_join(., la_places_seg80, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(la_places_seg80_shp, "Los_Angeles_places_segregation_1980.shp")

#90
la_places_seg90 <- all_decades_race_places_la_prop %>% 
  filter90(.) %>%
  compute_divergence(.) %>%
  left_join(., la_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

la_places_seg90_shp <- la_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter90(.) %>% 
  select(place_id:NAME) %>%
  right_join(., la_places_seg90, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(la_places_seg90_shp, "Los_Angeles_places_segregation_1990.shp")

#00
la_places_seg00 <- all_decades_race_places_la_prop %>% 
  filter00(.) %>%
  compute_divergence(.) %>%
  left_join(., la_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

la_places_seg00_shp <- la_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter00(.) %>% 
  select(place_id:NAME) %>%
  right_join(., la_places_seg00, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(la_places_seg00_shp, "Los_Angeles_places_segregation_2000.shp")

#10
la_places_seg10 <- all_decades_race_places_la_prop %>% 
  filter10(.) %>%
  compute_divergence(.) %>%
  left_join(., la_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

la_places_seg10_shp <- la_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter10(.) %>% 
  select(place_id:NAME) %>%
  right_join(., la_places_seg10, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(la_places_seg10_shp, "Los_Angeles_places_segregation_2010.shp")

#20
la_places_seg20 <- all_decades_race_places_la_prop %>% 
  filter20(.) %>%
  compute_divergence(.) %>%
  left_join(., la_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

la_places_seg20_shp <- la_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter20(.) %>% 
  select(place_id:NAME) %>%
  right_join(., la_places_seg20, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(la_places_seg20_shp, "Los_Angeles_places_segregation_2020.shp")


########
# KC
########
all_decades_race_places_kc_prop <- kc_all_decades_places %>% 
  create_race_proportions(.)

#70
kc_places_seg70 <- all_decades_race_places_kc_prop %>% 
  filter70(.) %>%
  compute_divergence(.) %>%
  left_join(., kc_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

kc_places_seg70_shp <- kc_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter70(.) %>% 
  select(place_id:NAME) %>%
  right_join(., kc_places_seg70, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(kc_places_seg70_shp, "Kansas_City_places_segregation_1970.shp")

#80
kc_places_seg80 <- all_decades_race_places_kc_prop %>% 
  filter80(.) %>%
  compute_divergence(.) %>%
  left_join(., kc_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

kc_places_seg80_shp <- kc_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter80(.) %>% 
  select(place_id:NAME) %>%
  right_join(., kc_places_seg80, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(kc_places_seg80_shp, "Kansas_City_places_segregation_1980.shp")

#90
kc_places_seg90 <- all_decades_race_places_kc_prop %>% 
  filter90(.) %>%
  compute_divergence(.) %>%
  left_join(., kc_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

kc_places_seg90_shp <- kc_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter90(.) %>% 
  select(place_id:NAME) %>%
  right_join(., kc_places_seg90, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(kc_places_seg90_shp, "Kansas_City_places_segregation_1990.shp")

#00
kc_places_seg00 <- all_decades_race_places_kc_prop %>% 
  filter00(.) %>%
  compute_divergence(.) %>%
  left_join(., kc_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

kc_places_seg00_shp <- kc_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter00(.) %>% 
  select(place_id:NAME) %>%
  right_join(., kc_places_seg00, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(kc_places_seg00_shp, "Kansas_City_places_segregation_2000.shp")

#10
kc_places_seg10 <- all_decades_race_places_kc_prop %>% 
  filter10(.) %>%
  compute_divergence(.) %>%
  left_join(., kc_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

kc_places_seg10_shp <- kc_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter10(.) %>% 
  select(place_id:NAME) %>%
  right_join(., kc_places_seg10, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(kc_places_seg10_shp, "Kansas_City_places_segregation_2010.shp")

#20
kc_places_seg20 <- all_decades_race_places_kc_prop %>% 
  filter20(.) %>%
  compute_divergence(.) %>%
  left_join(., kc_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

kc_places_seg20_shp <- kc_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter20(.) %>% 
  select(place_id:NAME) %>%
  right_join(., kc_places_seg20, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(kc_places_seg20_shp, "Kansas_City_places_segregation_2020.shp")


#############
# RIVERSIDE
#############
all_decades_race_places_river_prop <- river_all_decades_places %>% 
  create_race_proportions(.)

#70
river_places_seg70 <- all_decades_race_places_river_prop %>% 
  filter70(.) %>%
  compute_divergence(.) %>%
  left_join(., river_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

river_places_seg70_shp <- river_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter70(.) %>% 
  select(place_id:NAME) %>%
  right_join(., river_places_seg70, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(river_places_seg70_shp, "Riverside_San_Bernardino_places_segregation_1970.shp")

#80
river_places_seg80 <- all_decades_race_places_river_prop %>% 
  filter80(.) %>%
  compute_divergence(.) %>%
  left_join(., river_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

river_places_seg80_shp <- river_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter80(.) %>% 
  select(place_id:NAME) %>%
  right_join(., river_places_seg80, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(river_places_seg80_shp, "Riverside_San_Bernardino_places_segregation_1980.shp")

#90
river_places_seg90 <- all_decades_race_places_river_prop %>% 
  filter90(.) %>%
  compute_divergence(.) %>%
  left_join(., river_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

river_places_seg90_shp <- river_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter90(.) %>% 
  select(place_id:NAME) %>%
  right_join(., river_places_seg90, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(river_places_seg90_shp, "Riverside_San_Bernardino_places_segregation_1990.shp")

#00
river_places_seg00 <- all_decades_race_places_river_prop %>% 
  filter00(.) %>%
  compute_divergence(.) %>%
  left_join(., river_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

river_places_seg00_shp <- river_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter00(.) %>% 
  select(place_id:NAME) %>%
  right_join(., river_places_seg00, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(river_places_seg00_shp, "Riverside_San_Bernardino_places_segregation_2000.shp")

#10
river_places_seg10 <- all_decades_race_places_river_prop %>% 
  filter10(.) %>%
  compute_divergence(.) %>%
  left_join(., river_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

river_places_seg10_shp <- river_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter10(.) %>% 
  select(place_id:NAME) %>%
  right_join(., river_places_seg10, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(river_places_seg10_shp, "Riverside_San_Bernardino_places_segregation_2010.shp")

#20
river_places_seg20 <- all_decades_race_places_river_prop %>% 
  filter20(.) %>%
  compute_divergence(.) %>%
  left_join(., river_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

river_places_seg20_shp <- river_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter20(.) %>% 
  select(place_id:NAME) %>%
  right_join(., river_places_seg20, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(river_places_seg20_shp, "Riverside_San_Bernardino_places_segregation_2020.shp")


#############
# PITT
#############
all_decades_race_places_pitt_prop <- pitt_all_decades_places %>% 
  create_race_proportions(.)

#70
pitt_places_seg70 <- all_decades_race_places_pitt_prop %>% 
  filter70(.) %>%
  compute_divergence(.) %>%
  left_join(., pitt_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

pitt_places_seg70_shp <- pitt_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter70(.) %>% 
  select(place_id:NAME) %>%
  right_join(., pitt_places_seg70, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(pitt_places_seg70_shp, "Pittsburgh_places_segregation_1970.shp")

#80
pitt_places_seg80 <- all_decades_race_places_pitt_prop %>% 
  filter80(.) %>%
  compute_divergence(.) %>%
  left_join(., pitt_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

pitt_places_seg80_shp <- pitt_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter80(.) %>% 
  select(place_id:NAME) %>%
  right_join(., pitt_places_seg80, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(pitt_places_seg80_shp, "Pittsburgh_places_segregation_1980.shp")

#90
pitt_places_seg90 <- all_decades_race_places_pitt_prop %>% 
  filter90(.) %>%
  compute_divergence(.) %>%
  left_join(., pitt_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

pitt_places_seg90_shp <- pitt_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter90(.) %>% 
  select(place_id:NAME) %>%
  right_join(., pitt_places_seg90, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(pitt_places_seg90_shp, "Pittsburgh_places_segregation_1990.shp")

#00
pitt_places_seg00 <- all_decades_race_places_pitt_prop %>% 
  filter00(.) %>%
  compute_divergence(.) %>%
  left_join(., pitt_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

pitt_places_seg00_shp <- pitt_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter00(.) %>% 
  select(place_id:NAME) %>%
  right_join(., pitt_places_seg00, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(pitt_places_seg00_shp, "Pittsburgh_places_segregation_2000.shp")

#10
pitt_places_seg10 <- all_decades_race_places_pitt_prop %>% 
  filter10(.) %>%
  compute_divergence(.) %>%
  left_join(., pitt_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

pitt_places_seg10_shp <- pitt_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter10(.) %>% 
  select(place_id:NAME) %>%
  right_join(., pitt_places_seg10, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(pitt_places_seg10_shp, "Pittsburgh_places_segregation_2010.shp")

#20
pitt_places_seg20 <- all_decades_race_places_pitt_prop %>% 
  filter20(.) %>%
  compute_divergence(.) %>%
  left_join(., pitt_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

pitt_places_seg20_shp <- pitt_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter20(.) %>% 
  select(place_id:NAME) %>%
  right_join(., pitt_places_seg20, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(pitt_places_seg20_shp, "Pittsburgh_places_segregation_2020.shp")


#############
# YAKIMA
#############
all_decades_race_places_yakima_prop <- yakima_all_decades_places %>% 
  create_race_proportions(.)

#80
yakima_places_seg80 <- all_decades_race_places_yakima_prop %>% 
  filter80(.) %>%
  compute_divergence(.) %>%
  left_join(., yakima_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

yakima_places_seg80_shp <- yakima_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter80(.) %>% 
  select(place_id:NAME) %>%
  right_join(., yakima_places_seg80, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(yakima_places_seg80_shp, "Yakima_places_segregation_1980.shp")

#90
yakima_places_seg90 <- all_decades_race_places_yakima_prop %>% 
  filter90(.) %>%
  compute_divergence(.) %>%
  left_join(., yakima_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

yakima_places_seg90_shp <- yakima_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter90(.) %>% 
  select(place_id:NAME) %>%
  right_join(., yakima_places_seg90, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(yakima_places_seg90_shp, "Yakima_places_segregation_1990.shp")

#00
yakima_places_seg00 <- all_decades_race_places_yakima_prop %>% 
  filter00(.) %>%
  compute_divergence(.) %>%
  left_join(., yakima_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

yakima_places_seg00_shp <- yakima_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter00(.) %>% 
  select(place_id:NAME) %>%
  right_join(., yakima_places_seg00, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(yakima_places_seg00_shp, "Yakima_places_segregation_2000.shp")

#10
yakima_places_seg10 <- all_decades_race_places_yakima_prop %>% 
  filter10(.) %>%
  compute_divergence(.) %>%
  left_join(., yakima_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

yakima_places_seg10_shp <- yakima_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter10(.) %>% 
  select(place_id:NAME) %>%
  right_join(., yakima_places_seg10, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(yakima_places_seg10_shp, "Yakima_places_segregation_2010.shp")

#20
yakima_places_seg20 <- all_decades_race_places_yakima_prop %>% 
  filter20(.) %>%
  compute_divergence(.) %>%
  left_join(., yakima_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

yakima_places_seg20_shp <- yakima_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter20(.) %>% 
  select(place_id:NAME) %>%
  right_join(., yakima_places_seg20, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(yakima_places_seg20_shp, "Yakima_places_segregation_2020.shp")


#############
# DENVER
#############
all_decades_race_places_denver_prop <- denver_all_decades_places %>% 
  create_race_proportions(.)

#70
denver_places_seg70 <- all_decades_race_places_denver_prop %>% 
  filter70(.) %>%
  compute_divergence(.) %>%
  left_join(., denver_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

denver_places_seg70_shp <- denver_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter70(.) %>% 
  select(place_id:NAME) %>%
  right_join(., denver_places_seg70, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(denver_places_seg70_shp, "Denver_places_segregation_1970.shp")

#80
denver_places_seg80 <- all_decades_race_places_denver_prop %>% 
  filter80(.) %>%
  compute_divergence(.) %>%
  left_join(., denver_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

denver_places_seg80_shp <- denver_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter80(.) %>% 
  select(place_id:NAME) %>%
  right_join(., denver_places_seg80, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(denver_places_seg80_shp, "Denver_places_segregation_1980.shp")

#90
denver_places_seg90 <- all_decades_race_places_denver_prop %>% 
  filter90(.) %>%
  compute_divergence(.) %>%
  left_join(., denver_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

denver_places_seg90_shp <- denver_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter90(.) %>% 
  select(place_id:NAME) %>%
  right_join(., denver_places_seg90, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(denver_places_seg90_shp, "Denver_places_segregation_1990.shp")

#00
denver_places_seg00 <- all_decades_race_places_denver_prop %>% 
  filter00(.) %>%
  compute_divergence(.) %>%
  left_join(., denver_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

denver_places_seg00_shp <- denver_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter00(.) %>% 
  select(place_id:NAME) %>%
  right_join(., denver_places_seg00, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(denver_places_seg00_shp, "Denver_places_segregation_2000.shp")

#10
denver_places_seg10 <- all_decades_race_places_denver_prop %>% 
  filter10(.) %>%
  compute_divergence(.) %>%
  left_join(., denver_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

denver_places_seg10_shp <- denver_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter10(.) %>% 
  select(place_id:NAME) %>%
  right_join(., denver_places_seg10, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(denver_places_seg10_shp, "Denver_places_segregation_2010.shp")

#20
denver_places_seg20 <- all_decades_race_places_denver_prop %>% 
  filter20(.) %>%
  compute_divergence(.) %>%
  left_join(., denver_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

denver_places_seg20_shp <- denver_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter20(.) %>% 
  select(place_id:NAME) %>%
  right_join(., denver_places_seg20, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(denver_places_seg20_shp, "Denver_places_segregation_2020.shp")


#############
# DETROIT
#############
all_decades_race_places_detroit_prop <- detroit_all_decades_places %>% 
  create_race_proportions(.)

#70
detroit_places_seg70 <- all_decades_race_places_detroit_prop %>% 
  filter70(.) %>%
  compute_divergence(.) %>%
  left_join(., detroit_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

detroit_places_seg70_shp <- detroit_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter70(.) %>% 
  select(place_id:NAME) %>%
  right_join(., detroit_places_seg70, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(detroit_places_seg70_shp, "Detroit_places_segregation_1970.shp")

#80
detroit_places_seg80 <- all_decades_race_places_detroit_prop %>% 
  filter80(.) %>%
  compute_divergence(.) %>%
  left_join(., detroit_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

detroit_places_seg80_shp <- detroit_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter80(.) %>% 
  select(place_id:NAME) %>%
  right_join(., detroit_places_seg80, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(detroit_places_seg80_shp, "Detroit_places_segregation_1980.shp")

#90
detroit_places_seg90 <- all_decades_race_places_detroit_prop %>% 
  filter90(.) %>%
  compute_divergence(.) %>%
  left_join(., detroit_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

detroit_places_seg90_shp <- detroit_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter90(.) %>% 
  select(place_id:NAME) %>%
  right_join(., detroit_places_seg90, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(detroit_places_seg90_shp, "Detroit_places_segregation_1990.shp")

#00
detroit_places_seg00 <- all_decades_race_places_detroit_prop %>% 
  filter00(.) %>%
  compute_divergence(.) %>%
  left_join(., detroit_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

detroit_places_seg00_shp <- detroit_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter00(.) %>% 
  select(place_id:NAME) %>%
  right_join(., detroit_places_seg00, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(detroit_places_seg00_shp, "Detroit_places_segregation_2000.shp")

#10
detroit_places_seg10 <- all_decades_race_places_detroit_prop %>% 
  filter10(.) %>%
  compute_divergence(.) %>%
  left_join(., detroit_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

detroit_places_seg10_shp <- detroit_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter10(.) %>% 
  select(place_id:NAME) %>%
  right_join(., detroit_places_seg10, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(detroit_places_seg10_shp, "Detroit_places_segregation_2010.shp")

#20
detroit_places_seg20 <- all_decades_race_places_detroit_prop %>% 
  filter20(.) %>%
  compute_divergence(.) %>%
  left_join(., detroit_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

detroit_places_seg20_shp <- detroit_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter20(.) %>% 
  select(place_id:NAME) %>%
  right_join(., detroit_places_seg20, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(detroit_places_seg20_shp, "Detroit_places_segregation_2020.shp")


##############
# BAKERSFIELD
##############
all_decades_race_places_baker_prop <- baker_all_decades_places %>% 
  create_race_proportions(.)

#70
baker_places_seg70 <- all_decades_race_places_baker_prop %>% 
  filter70(.) %>%
  compute_divergence(.) %>%
  left_join(., baker_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

baker_places_seg70_shp <- baker_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter70(.) %>% 
  select(place_id:NAME) %>%
  right_join(., baker_places_seg70, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(baker_places_seg70_shp, "Bakersfield_places_segregation_1970.shp")

#80
baker_places_seg80 <- all_decades_race_places_baker_prop %>% 
  filter80(.) %>%
  compute_divergence(.) %>%
  left_join(., baker_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

baker_places_seg80_shp <- baker_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter80(.) %>% 
  select(place_id:NAME) %>%
  right_join(., baker_places_seg80, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(baker_places_seg80_shp, "Bakersfield_places_segregation_1980.shp")

#90
baker_places_seg90 <- all_decades_race_places_baker_prop %>% 
  filter90(.) %>%
  compute_divergence(.) %>%
  left_join(., baker_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

baker_places_seg90_shp <- baker_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter90(.) %>% 
  select(place_id:NAME) %>%
  right_join(., baker_places_seg90, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(baker_places_seg90_shp, "Bakersfield_places_segregation_1990.shp")

#00
baker_places_seg00 <- all_decades_race_places_baker_prop %>% 
  filter00(.) %>%
  compute_divergence(.) %>%
  left_join(., baker_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

baker_places_seg00_shp <- baker_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter00(.) %>% 
  select(place_id:NAME) %>%
  right_join(., baker_places_seg00, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(baker_places_seg00_shp, "Bakersfield_places_segregation_2000.shp")

#10
baker_places_seg10 <- all_decades_race_places_baker_prop %>% 
  filter10(.) %>%
  compute_divergence(.) %>%
  left_join(., baker_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

baker_places_seg10_shp <- baker_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter10(.) %>% 
  select(place_id:NAME) %>%
  right_join(., baker_places_seg10, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(baker_places_seg10_shp, "Bakersfield_places_segregation_2010.shp")

#20
baker_places_seg20 <- all_decades_race_places_baker_prop %>% 
  filter20(.) %>%
  compute_divergence(.) %>%
  left_join(., baker_places_dissim, by = c("placefp10", "year", "NAME")) %>%
  select(-NHWHITE:-NONWHITE)

baker_places_seg20_shp <- baker_all_decades_race_places_shp %>% 
  select(-statefp) %>%
  filter20(.) %>% 
  select(place_id:NAME) %>%
  right_join(., baker_places_seg20, by = c("placefp10", "place_id", "year", "NAME")) %>%
  select(-statefp)
st_write(baker_places_seg20_shp, "Bakersfield_places_segregation_2020.shp")



# View shapefile
leaflet() %>%
  addTiles() %>%
  addPolygons(data = baker_places_seg70_shp %>% 
                st_transform(., crs = 4326),
              label = ~(NAME)
  )
