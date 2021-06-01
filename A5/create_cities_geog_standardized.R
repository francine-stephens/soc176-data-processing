#-------------------------------------------------------------------------------
# IDENTIFY CENSUS TRACTS FOR CITIES HISTORICAL APPROACH 
#
# AUTHOR: Francine Stephens
# DATE CREATED: 4/20/21
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

all_decades_race_tracts_prop <- all_decades_race_tracts %>%
  mutate_at(vars(NHWHITE:HISPANIC), funs("PR" = (./POP))) %>%
  mutate(OTHER_PR = 1.0 - (NHWHITE_PR + NHBLACK_PR + NATIVE_PR + ASIAN_PR + HISPANIC_PR)
         )

all_decades_race_tracts_seg <- all_decades_race_tracts_prop

all_decades_race_tracts_class_shp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., all_decades_race_tracts, by = "GEOID10")
  
# EXPORT CITY TRACTS RACIAL COMPOSITION ----------------------------------------

# SF
sf_tracts <- all_decades_race_tracts_class_shp %>%
  filter(county == "San Francisco County") %>%
  filter(GEOID10 != "06075980401")
st_write(sf_tracts, "san_francisco_tracts_race.shp")

# LA
la_tracts <- all_decades_race_tracts_class_shp %>%
  filter(county == "Los Angeles County")
st_write(la_tracts, "los_angeles_tracts_race.shp")

# Oakland
oakland_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "0653000")
st_write(oakland_tracts, "oakland_tracts_race.shp")

# Fremont
fremont_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "0626000")
st_write(fremont_tracts, "fremont_tracts_race.shp")

## Bakersfield
bakersfield_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "063526")
st_write(bakersfield_tracts, "bakersfield_tracts_race.shp")

## Westminster
westminster_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "0684550")
st_write(westminster_tracts, "westminster_tracts_race.shp")

## Chino
chino_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "0613210")
st_write(chino_tracts, "chino_tracts_race.shp")

## San Mateo
san_mateo_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "0668252")
st_write(san_mateo_tracts, "san_mateo_tracts_race.shp")

## East Palo Alto
epa_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "0620956")
st_write(epa_tracts, "east_palo_alto_tracts_race.shp")

## San Jose 
sj_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "0668000")
st_write(sj_tracts, "san_jose_tracts_race.shp")

## Palo Alto
pa_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "0655282")
st_write(pa_tracts, "palo_alto_tracts_race.shp")

## Denver
denver_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "0820000")
st_write(denver_tracts, "denver_tracts_race.shp")

## Detroit
detroit_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "2622000")
st_write(detroit_tracts, "detroit_tracts_race.shp")

## Kansas City
kc_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "2938000")
st_write(kc_tracts, "kansas_city_tracts_race.shp")

## Pittsburgh
pitt_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "4261000")
st_write(pitt_tracts, "pittsburgh_tracts_race.shp")

## Yakima
yakima_tracts <- all_decades_race_tracts_class_shp %>%
  filter(state_place == "5380010")
st_write(yakima_tracts, "yakima_tracts_race.shp")


# COMPUTE DISSIMILARITY INDEX---------------------------------------------------

## COMPUTE PLACE RACE TOTALS
place_city_name_link <- class_places %>%
  st_set_geometry(NULL) %>%
  select(city, state_place) %>%
  distinct() %>%
  filter(city != "East Palo Alto/Menlo Park")

all_decades_race_city <- all_decades_race_tracts %>%
  select(year, state_place, POP:NONWHITE) %>%
  group_by(year, state_place) %>%
  summarize_all(sum, na.rm = TRUE) %>%
  rename_at(vars(-year, -state_place), function(x) paste0(x,"_CITY")) %>% 
  left_join(., place_city_name_link, by = "state_place") %>%
  relocate(city, .after = "state_place")


## COMPUTE DISSIMILARITY
all_decades_city_dissim_scores <- all_decades_race_tracts %>% 
  select(GEOID10, year, state_place, POP:NONWHITE) %>%
  left_join(., all_decades_race_city, by = c("year", "state_place")) %>%
  mutate(D_NWW=abs(NONWHITE/NONWHITE_CITY - NHWHITE/NHWHITE_CITY),
         D_BW=abs(NHBLACK/NHBLACK_CITY - NHWHITE/NHWHITE_CITY),
         D_AW=abs(ASIAN/ASIAN_CITY - NHWHITE/NHWHITE_CITY),
         D_HW=abs(HISPANIC/HISPANIC_CITY - NHWHITE/NHWHITE_CITY),
         D_BA=abs(NHBLACK/NHBLACK_CITY - ASIAN/ASIAN_CITY),
         D_BH=abs(NHBLACK/NHBLACK_CITY - HISPANIC/HISPANIC_CITY),
         D_AH=abs(ASIAN/ASIAN_CITY - HISPANIC/HISPANIC_CITY)
         ) %>%
  group_by(year, state_place, city) %>%
  summarise(Nonwhite_White = .5*sum(D_NWW, na.rm=T),
            Black_White= .5*sum(D_BW, na.rm=T),
            Asian_White = .5*sum(D_AW, na.rm=T),
            Hispanic_White= .5*sum(D_HW, na.rm=T),
            Black_Asian = .5*sum(D_BA, na.rm=T),
            Black_Hispanic = .5*sum(D_BH, na.rm=T),
            Asian_Hispanic = .5*sum(D_AH, na.rm=T)
  ) %>% 
  ungroup() %>%
  mutate_at(vars(Nonwhite_White:Asian_Hispanic),
            .funs = funs(. * 100)) %>% 
  mutate(year = year(year)) %>% 
  arrange(city, year) %>%
  relocate(city, year) %>% 
  select(-state_place) 
names(all_decades_city_dissim_scores) <- gsub(
  "\\_",
  "-",
  names(all_decades_city_dissim_scores)
  )

cities_shp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., all_decades_race_tracts, by = "GEOID10") %>% 
  filter(year == "2010-01-31") %>%
  left_join(., place_city_name_link, by = "state_place") %>%
  group_by(city, state_place) %>%
  summarize(n_tracts = n())


# EXPORT DISSIMILARITY INDEX----------------------------------------------------
# SF
sf_dissim <- all_decades_city_dissim_shp %>%
  filter(city == "San Francisco") %>%
  filter(GEOID10 != "06075980401")
st_write(sf_dissim, "san_francisco_dissimilarity.shp")

# LA
la_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Los Angeles")
write_csv(la_dissim, file = "Los_Angeles_dissimilarity.csv", na = "") 

# OAKLAND
oakland_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Oakland")
write_csv(oakland_dissim, file = "Oakland_dissimilarity.csv", na = "") 

# FREMONT
fremont_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Fremont")
write_csv(fremont_dissim, file = "Fremont_dissimilarity.csv", na = "") 

# Bakersfield
bakersfield_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Bakersfield")
write_csv(bakersfield_dissim, file = "Bakersfield_dissimilarity.csv", na = "") 

# Westminster
westminster_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Westminster")
write_csv(westminster_dissim, file = "Westminster_dissimilarity.csv", na = "") 

# Chino
chino_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Chino")
write_csv(chino_dissim, file = "Chino_dissimilarity.csv", na = "") 

# San Mateo
sm_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "San Mateo")
write_csv(sm_dissim, file = "San_Mateo_dissimilarity.csv", na = "") 

# East Palo Alto
epa_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "East Palo Alto")
write_csv(epa_dissim, file = "East_Palo_Alto_dissimilarity.csv", na = "") 

# San Jose 
sj_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "San Jose")
write_csv(sj_dissim, file = "San_Jose_dissimilarity.csv", na = "")

# Palo Alto
pa_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Palo Alto")
write_csv(pa_dissim, file = "Palo_Alto_dissimilarity.csv", na = "")

# DENVER
denver_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Denver")
write_csv(denver_dissim, file = "Denver_dissimilarity.csv", na = "")  

# DETROIT
detroit_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Detroit")
write_csv(detroit_dissim, file = "Detroit_dissimilarity.csv", na = "")

# KC
kc_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Kansas City")
write_csv(kc_dissim, file = "Kansas_City_dissimilarity.csv", na = "")

# PITT
pitt_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Pittsburgh")
write_csv(pitt_dissim, file = "Pittsburgh_dissimilarity.csv", na = "")

# YAKIMA
yakima_dissim <- all_decades_city_dissim_scores %>%
  filter(city == "Yakima")
write_csv(yakima_dissim, file = "Yakima_dissimilarity.csv", na = "")


# EXPORT DIVERGENCE INDEX-------------------------------------------------------
compute_tract_proportions <- function(x) { 
  x %>%       
    st_set_geometry(NULL) %>%
    mutate_at(vars(NHWHITE:HISPANIC), funs("PR" = (./POP))) %>%
    mutate(OTHER_PR = 1.0 - (NHWHITE_PR + NHBLACK_PR + NATIVE_PR + ASIAN_PR + HISPANIC_PR)
    )
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
    )

}

# SF
sf_tracts_pr <- all_decades_race_tracts_seg %>% 
  filter(county == "San Francisco County") %>%
  mutate_at(vars(NHWHITE:HISPANIC), funs("PR" = (./POP))) %>%
  mutate(OTHER_PR = 1.0 - (NHWHITE_PR + NHBLACK_PR + NATIVE_PR + ASIAN_PR + HISPANIC_PR)
  )

sf_tracts_pr_2020 <- sf_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  mutate(entropy = entropy(NHWHITE_PR,
                           NHBLACK_PR, 
                           NATIVE_PR, 
                           ASIAN_PR, 
                           HISPANIC_PR, 
                           population = POP,
                           summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(entropy = na_if(entropy, 0),
         divergence = na_if(divergence, 0),
         divergence = if_else(divergence <0, 
                              0, 
                              divergence)
  )

sf_tracts_pr_2010 <- sf_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  mutate(entropy = entropy(NHWHITE_PR,
                           NHBLACK_PR, 
                           NATIVE_PR, 
                           ASIAN_PR, 
                           HISPANIC_PR, 
                           population = POP,
                           summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(entropy = na_if(entropy, 0),
         divergence = na_if(divergence, 0),
         divergence = if_else(divergence <0, 
                              0, 
                              divergence)
  )

sf_tracts_pr_2000 <- sf_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  mutate(entropy = entropy(NHWHITE_PR,
                           NHBLACK_PR, 
                           NATIVE_PR, 
                           ASIAN_PR, 
                           HISPANIC_PR, 
                           population = POP,
                           summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(entropy = na_if(entropy, 0),
         divergence = na_if(divergence, 0),
         divergence = if_else(divergence <0, 
                              0, 
                              divergence)
  )

sf_tracts_pr_1990 <- sf_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  mutate(entropy = entropy(NHWHITE_PR,
                           NHBLACK_PR, 
                           NATIVE_PR, 
                           ASIAN_PR, 
                           HISPANIC_PR, 
                           population = POP,
                           summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(entropy = na_if(entropy, 0),
         divergence = na_if(divergence, 0),
         divergence = if_else(divergence <0, 
                              0, 
                              divergence)
  )

sf_tracts_pr_1980 <- sf_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  mutate(entropy = entropy(NHWHITE_PR,
                           NHBLACK_PR, 
                           NATIVE_PR, 
                           ASIAN_PR, 
                           HISPANIC_PR, 
                           population = POP,
                           summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(entropy = na_if(entropy, 0),
         divergence = na_if(divergence, 0),
         divergence = if_else(divergence <0, 
                              0, 
                              divergence)
  )

sf_tracts_pr_1970 <- sf_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  mutate(entropy = entropy(NHWHITE_PR,
                           NHBLACK_PR, 
                           NATIVE_PR, 
                           ASIAN_PR, 
                           HISPANIC_PR, 
                           population = POP,
                           summed = FALSE)
  ) %>%
  ungroup() %>%
  mutate(entropy = na_if(entropy, 0),
         divergence = na_if(divergence, 0),
         divergence = if_else(divergence <0, 
                              0, 
                              divergence)
  )

sf_tracts_all_decades_seg <- bind_rows(sf_tracts_pr_1970,
                                       sf_tracts_pr_1980,
                                       sf_tracts_pr_1990,
                                       sf_tracts_pr_2000,
                                       sf_tracts_pr_2010,
                                       sf_tracts_pr_2020
)

sf_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., sf_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  filter(GEOID10 != "06075980401") %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(sf_tracts_all_decades_seg_exp, "san_francisco_tracts_divergence.shp")

  ## Output 2020 sf tracts racial composition data for story map analysis
write_csv(sf_tracts_pr_2000, "san_francisco_2020_tracts_race.csv")


## LA
la_tracts_pr <- la_tracts %>%
  compute_tract_proportions(.) 

la_tracts_pr_2020 <- la_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

la_tracts_pr_2010 <- la_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)
  
la_tracts_pr_2000 <- la_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

la_tracts_pr_1990 <- la_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

la_tracts_pr_1980 <- la_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

la_tracts_pr_1970 <- la_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

la_tracts_all_decades_seg <- bind_rows(
          la_tracts_pr_1970,
          la_tracts_pr_1980,
          la_tracts_pr_1990,
          la_tracts_pr_2000,
          la_tracts_pr_2010,
          la_tracts_pr_2020)

la_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., la_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(la_tracts_all_decades_seg_exp, "Los_Angeles_tracts_divergence.shp")

## SJ
sj_tracts_pr <- sj_tracts %>%
  compute_tract_proportions(.) 

sj_tracts_pr_2020 <- sj_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

sj_tracts_pr_2010 <- sj_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

sj_tracts_pr_2000 <- sj_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

sj_tracts_pr_1990 <- sj_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

sj_tracts_pr_1980 <- sj_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

sj_tracts_pr_1970 <- sj_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

sj_tracts_all_decades_seg <- bind_rows(
  sj_tracts_pr_1970,
  sj_tracts_pr_1980,
  sj_tracts_pr_1990,
  sj_tracts_pr_2000,
  sj_tracts_pr_2010,
  sj_tracts_pr_2020)

sj_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., sj_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(sj_tracts_all_decades_seg_exp, "San_Jose_tracts_divergence.shp")


## OAKLAND
oakland_tracts_pr <- oakland_tracts %>%
  compute_tract_proportions(.) 

oakland_tracts_pr_2020 <- oakland_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

oakland_tracts_pr_2010 <- oakland_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

oakland_tracts_pr_2000 <- oakland_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

oakland_tracts_pr_1990 <- oakland_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

oakland_tracts_pr_1980 <- oakland_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

oakland_tracts_pr_1970 <- oakland_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

oakland_tracts_all_decades_seg <- bind_rows(
  oakland_tracts_pr_1970,
  oakland_tracts_pr_1980,
  oakland_tracts_pr_1990,
  oakland_tracts_pr_2000,
  oakland_tracts_pr_2010,
  oakland_tracts_pr_2020)

oakland_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., oakland_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(oakland_tracts_all_decades_seg_exp, "Oakland_tracts_divergence.shp")

## Fremont
fremont_tracts_pr <- fremont_tracts %>%
  compute_tract_proportions(.) 

fremont_tracts_pr_2020 <- fremont_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

fremont_tracts_pr_2010 <- fremont_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

fremont_tracts_pr_2000 <- fremont_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

fremont_tracts_pr_1990 <- fremont_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

fremont_tracts_pr_1980 <- fremont_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

fremont_tracts_pr_1970 <- fremont_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

fremont_tracts_all_decades_seg <- bind_rows(
  fremont_tracts_pr_1970,
  fremont_tracts_pr_1980,
  fremont_tracts_pr_1990,
  fremont_tracts_pr_2000,
  fremont_tracts_pr_2010,
  fremont_tracts_pr_2020)

fremont_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., fremont_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(fremont_tracts_all_decades_seg_exp, "fremont_tracts_divergence.shp")


## Bakersfield
baker_tracts_pr <- bakersfield_tracts %>%
  compute_tract_proportions(.) 

baker_tracts_pr_2020 <- baker_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

baker_tracts_pr_2010 <- baker_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

baker_tracts_pr_2000 <- baker_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

baker_tracts_pr_1990 <- baker_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

baker_tracts_pr_1980 <- baker_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

baker_tracts_pr_1970 <- baker_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

baker_tracts_all_decades_seg <- bind_rows(
  baker_tracts_pr_1970,
  baker_tracts_pr_1980,
  baker_tracts_pr_1990,
  baker_tracts_pr_2000,
  baker_tracts_pr_2010,
  baker_tracts_pr_2020)

baker_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., baker_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(baker_tracts_all_decades_seg_exp, "Bakersfield_tracts_divergence.shp")


## Westminster
westmin_tracts_pr <- westminster_tracts %>%
  compute_tract_proportions(.) 

westmin_tracts_pr_2020 <- westmin_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

westmin_tracts_pr_2010 <- westmin_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

westmin_tracts_pr_2000 <- westmin_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

westmin_tracts_pr_1990 <- westmin_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

westmin_tracts_pr_1980 <- westmin_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

westmin_tracts_pr_1970 <- westmin_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

westmin_tracts_all_decades_seg <- bind_rows(
  westmin_tracts_pr_1970,
  westmin_tracts_pr_1980,
  westmin_tracts_pr_1990,
  westmin_tracts_pr_2000,
  westmin_tracts_pr_2010,
  westmin_tracts_pr_2020)

westmin_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., westmin_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(westmin_tracts_all_decades_seg_exp, "Westminster_tracts_divergence.shp")


## Chino
chino_tracts_pr <- chino_tracts %>%
  compute_tract_proportions(.) 

chino_tracts_pr_2020 <- chino_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

chino_tracts_pr_2010 <- chino_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

chino_tracts_pr_2000 <- chino_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

chino_tracts_pr_1990 <- chino_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

chino_tracts_pr_1980 <- chino_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

chino_tracts_pr_1970 <- chino_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

chino_tracts_all_decades_seg <- bind_rows(
  chino_tracts_pr_1970,
  chino_tracts_pr_1980,
  chino_tracts_pr_1990,
  chino_tracts_pr_2000,
  chino_tracts_pr_2010,
  chino_tracts_pr_2020)

chino_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., chino_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(chino_tracts_all_decades_seg_exp, "Chino_tracts_divergence.shp")


## San Mateo
sm_tracts_pr <- san_mateo_tracts %>%
  compute_tract_proportions(.) 

sm_tracts_pr_2020 <- sm_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

sm_tracts_pr_2010 <- sm_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

sm_tracts_pr_2000 <- sm_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

sm_tracts_pr_1990 <- sm_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

sm_tracts_pr_1980 <- sm_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

sm_tracts_pr_1970 <- sm_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

sm_tracts_all_decades_seg <- bind_rows(
  sm_tracts_pr_1970,
  sm_tracts_pr_1980,
  sm_tracts_pr_1990,
  sm_tracts_pr_2000,
  sm_tracts_pr_2010,
  sm_tracts_pr_2020)

sm_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., sm_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(sm_tracts_all_decades_seg_exp, "San_Mateo_tracts_divergence.shp")


## East Palo Alto
epa_tracts_pr <- epa_tracts %>%
  compute_tract_proportions(.) 

epa_tracts_pr_2020 <- epa_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

epa_tracts_pr_2010 <- epa_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

epa_tracts_pr_2000 <- epa_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

epa_tracts_pr_1990 <- epa_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

epa_tracts_pr_1980 <- epa_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

epa_tracts_pr_1970 <- epa_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

epa_tracts_all_decades_seg <- bind_rows(
  epa_tracts_pr_1970,
  epa_tracts_pr_1980,
  epa_tracts_pr_1990,
  epa_tracts_pr_2000,
  epa_tracts_pr_2010,
  epa_tracts_pr_2020)

epa_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., epa_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(epa_tracts_all_decades_seg_exp, "East_Palo_Alto_tracts_divergence.shp")


## Palo Alto
pa_tracts_pr <- pa_tracts %>%
  compute_tract_proportions(.) 

pa_tracts_pr_2020 <- pa_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

pa_tracts_pr_2010 <- pa_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

pa_tracts_pr_2000 <- pa_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

pa_tracts_pr_1990 <- pa_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

pa_tracts_pr_1980 <- pa_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

pa_tracts_pr_1970 <- pa_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

pa_tracts_all_decades_seg <- bind_rows(
  pa_tracts_pr_1970,
  pa_tracts_pr_1980,
  pa_tracts_pr_1990,
  pa_tracts_pr_2000,
  pa_tracts_pr_2010,
  pa_tracts_pr_2020)

pa_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., pa_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(pa_tracts_all_decades_seg_exp, "Palo_Alto_tracts_divergence.shp")


## Denver
denver_tracts_pr <- denver_tracts %>%
  compute_tract_proportions(.) 

denver_tracts_pr_2020 <- denver_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

denver_tracts_pr_2010 <- denver_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

denver_tracts_pr_2000 <- denver_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

denver_tracts_pr_1990 <- denver_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

denver_tracts_pr_1980 <- denver_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

denver_tracts_pr_1970 <- denver_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

denver_tracts_all_decades_seg <- bind_rows(
  denver_tracts_pr_1970,
  denver_tracts_pr_1980,
  denver_tracts_pr_1990,
  denver_tracts_pr_2000,
  denver_tracts_pr_2010,
  denver_tracts_pr_2020)

denver_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., denver_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(denver_tracts_all_decades_seg_exp, "Denver_tracts_divergence.shp")


## Detroit
detroit_tracts_pr <- detroit_tracts %>%
  compute_tract_proportions(.) 

detroit_tracts_pr_2020 <- detroit_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

detroit_tracts_pr_2010 <- detroit_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

detroit_tracts_pr_2000 <- detroit_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

detroit_tracts_pr_1990 <- detroit_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

detroit_tracts_pr_1980 <- detroit_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

detroit_tracts_pr_1970 <- detroit_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

detroit_tracts_all_decades_seg <- bind_rows(
  detroit_tracts_pr_1970,
  detroit_tracts_pr_1980,
  detroit_tracts_pr_1990,
  detroit_tracts_pr_2000,
  detroit_tracts_pr_2010,
  detroit_tracts_pr_2020)

detroit_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., detroit_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(detroit_tracts_all_decades_seg_exp, "Detroit_tracts_divergence.shp")

## KC
kc_tracts_pr <- kc_tracts %>%
  compute_tract_proportions(.) 

kc_tracts_pr_2020 <- kc_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

kc_tracts_pr_2010 <- kc_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

kc_tracts_pr_2000 <- kc_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

kc_tracts_pr_1990 <- kc_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

kc_tracts_pr_1980 <- kc_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

kc_tracts_pr_1970 <- kc_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

kc_tracts_all_decades_seg <- bind_rows(
  kc_tracts_pr_1970,
  kc_tracts_pr_1980,
  kc_tracts_pr_1990,
  kc_tracts_pr_2000,
  kc_tracts_pr_2010,
  kc_tracts_pr_2020)

kc_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., kc_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(kc_tracts_all_decades_seg_exp, "Kansas_City_tracts_divergence.shp")


## Pitt
pitt_tracts_pr <- pitt_tracts %>%
  compute_tract_proportions(.) 

pitt_tracts_pr_2020 <- pitt_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

pitt_tracts_pr_2010 <- pitt_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

pitt_tracts_pr_2000 <- pitt_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

pitt_tracts_pr_1990 <- pitt_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

pitt_tracts_pr_1980 <- pitt_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

pitt_tracts_pr_1970 <- pitt_tracts_pr %>%
  filter(year == "1970-01-31") %>%
  compute_divergence(.)

pitt_tracts_all_decades_seg <- bind_rows(
  pitt_tracts_pr_1970,
  pitt_tracts_pr_1980,
  pitt_tracts_pr_1990,
  pitt_tracts_pr_2000,
  pitt_tracts_pr_2010,
  pitt_tracts_pr_2020)

pitt_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., pitt_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(pitt_tracts_all_decades_seg_exp, "Pittsburgh_tracts_divergence.shp")


## Yakima
yakima_tracts_pr <- yakima_tracts %>%
  compute_tract_proportions(.) 

yakima_tracts_pr_2020 <- yakima_tracts_pr %>%
  filter(year > "2020-01-01") %>%
  compute_divergence(.)

yakima_tracts_pr_2010 <- yakima_tracts_pr %>%
  filter(year == "2010-01-31") %>%
  compute_divergence(.)

yakima_tracts_pr_2000 <- yakima_tracts_pr %>%
  filter(year == "2000-01-31") %>%
  compute_divergence(.)

yakima_tracts_pr_1990 <- yakima_tracts_pr %>%
  filter(year == "1990-01-31") %>%
  compute_divergence(.)

yakima_tracts_pr_1980 <- yakima_tracts_pr %>%
  filter(year == "1980-01-31") %>%
  compute_divergence(.)

yakima_tracts_all_decades_seg <- bind_rows(
  yakima_tracts_pr_1980,
  yakima_tracts_pr_1990,
  yakima_tracts_pr_2000,
  yakima_tracts_pr_2010,
  yakima_tracts_pr_2020)

yakima_tracts_all_decades_seg_exp <- census_tracts %>% 
  select(GEOID10) %>%
  right_join(., yakima_tracts_all_decades_seg, by = "GEOID10") %>% 
  select(GEOID10, year:POP, NHWHITE_PR:divergence) %>%
  mutate(divergence = rescale(divergence, to = c(0, 1.00)))
st_write(yakima_tracts_all_decades_seg_exp, "Yakima_tracts_divergence.shp")


# View shapefile
leaflet() %>%
  addTiles() %>%
  addPolygons(data = yakima_tracts_all_decades_seg_exp %>% 
                st_transform(., crs = 4326),
              label = ~(GEOID10)
  )
  