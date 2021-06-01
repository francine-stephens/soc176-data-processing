#-------------------------------------------------------------------------------
# PREPARE CENSUS TRACT GENTRIFICATION DATA
#
# AUTHOR: Francine Stephens
# DATE CREATED: 4/30/21
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
setwd("~/Stanford/SOC176/soc176-data-processing/A6")
wd <- getwd()
shp_repo <- "C:/Users/Franc/Documents/Shapefile_Repository/"
tracts10_path <- "2010USA_CensusGeog_Shp/nhgis0005_shapefile_tl2010_us_tract_2010/"
metros10_path <- "2010USA_CensusGeog_Shp/tl_2010_us_cbsa10/"
student_path <- "C:/Users/Franc/Documents/Stanford/SOC176/"
LTDB_geog_ids_path <- "C:/Users/Franc/Documents/Stanford/SOC176/soc176-data-processing/A5/LTDB_Std_All_fullcount/"

# APIs
census_api_key("99ccb52a629609683f17f804ca875115e3f0804c",  overwrite = T)
Sys.setenv(CENSUS_KEY="99ccb52a629609683f17f804ca875115e3f0804c")

## IMPORT DATA------------------------------------------------------------------
student_hoods <- read_csv(paste0(student_path,
                                 "student_neighborhoods_list.csv")
)

francine_nhood <- c("06075026100", 
                    "06075026301", 
                    "06075026004",
                    "06075026001",
                    "06075025500"
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
acs19_tract <- read_csv(paste0(wd,
                               "/CT_2019_ACS.csv")
                        )

acs19_place <- read_csv(paste0(wd,
                               "/PLACE_2019_ACS.csv")
)

## 2010 DEMOGS 
census10_tenure_tract <- read_csv(paste0(wd,
                                         "/CT_2010_Decennial_Tenure.csv")
)

acs10_tract <- read_csv(paste0(wd, 
                               "/CT_2012_ACS.csv")
)


census10_tenure_place <- read_csv(paste0(wd,
                                        "/PLACE_2010_Decennial_Tenure.csv")
)

acs10_place <- read_csv(paste0(wd, 
                              "/PLACE_2014_ACS.csv")
)

## 2000 DEMOGS
census00_tract <- read_csv(paste0(wd, 
                                  "/CT_2000_on_2010.csv")
                           )

census00_place <- read_csv(paste0(wd, 
                                  "/PLACE_2000.csv")
                           )

## 1990 DEMOGS 
census90_tract <- read_csv(paste0(wd, 
                                  "/CT_1990_on_2010.csv")
)

census90_place <- read_csv(paste0(wd, 
                                  "/PLACE_1990.csv")
                          )


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


## CLEAN SUB-UNIT DEMOGRAPHIC VARIABLES-----------------------------------------
# PAD GEOIDS
mutate_tract_id <- function(x) { 
  x %>%       
    mutate(GEOID = as.character(GEOID),
         GEOID = str_pad(GEOID, width = 11, side = "left", pad = "0")
         )
}

# Compute & Name Key Gentrification Variables
create_key_vars <- function(x) { 
  x %>%       
    mutate(PCOLL = (COLL/POP_O25) * 100,
           POWN = (HU_OWN/HU_OCC) * 100
    ) %>% 
    rename_at(vars(MD_HINC, MD_HVAL, MD_RVAL), function(x) gsub("_", "", x)) %>%
    select(GEOID,
           COLL, 
           PCOLL,
           MDHINC,
           HU_OCC,
           HU_OWN,
           POWN, 
           HU_RENT,
           MDHVAL,
           MDRVAL
    ) 
}
  
  
# 2020 PREP
acs19_tract_for_stack <- acs19_tract %>% 
  mutate_tract_id(.) %>%
  mutate(NOWN = (OWN_2014LATER + OWN_2010T2013), 
         NRENT = (RENT_2014LATER + RENT_2010T2013)
  ) %>%
  create_key_vars(.) %>%
  mutate(year = as.Date("01/31/2020", format = "%m/%d/%Y")) %>%
  relocate(year, .before = "GEOID")

acs19_tract_for_chg <- acs19_tract %>% 
  mutate_tract_id(.) %>%
  mutate(NOWN = (OWN_2014LATER + OWN_2010T2013), 
         NRENT = (RENT_2014LATER + RENT_2010T2013)
        ) %>%
  create_key_vars(.) %>%
  rename_at(vars(-GEOID),function(x) paste0(x,"20"))


# 2010 PREP
census10_tract_tenure_for_stack <- census10_tenure_tract %>%
  mutate_tract_id(.) %>%
  mutate(POWN = (HU_OWN/HU_OCC) * 100) %>%
  mutate(year = as.Date("01/31/2010", format = "%m/%d/%Y")) %>%
  relocate(year, .before = "GEOID")

census10_tract_tenure_for_chg <- census10_tenure_tract %>%
  mutate_tract_id(.) %>%
  mutate(POWN = (HU_OWN/HU_OCC) * 100) %>% 
  rename_at(vars(-GEOID), function(x) paste0(x,"10"))

acs10_tract_for_stack <- acs10_tract %>%
  mutate_tract_id(.) %>% 
  mutate(NOWN = (OWN_2010AFTER + OWN_2000T2009), 
         NRENT = (RENT_2010AFTER + RENT_2000T2009)
  ) %>%
  mutate(PCOLL = (COLL/POP_O25) * 100,
         PNOWN = (NOWN/OWN_OCC_D) * 100,
         PNRENT = (NRENT/RENT_OCC_D) * 100
  ) %>% 
  rename_at(vars(MD_HINC, MD_HVAL, MD_RVAL),
            function(x) gsub("_", "", x)
            ) %>%
  select(GEOID,
         COLL, 
         PCOLL,
         MDHINC,
         NOWN,
         PNOWN,
         NRENT,
         PNRENT,
         MDHVAL,
         MDRVAL
  ) %>%
  mutate(year = as.Date("01/31/2010", format = "%m/%d/%Y")) %>%
  relocate(year, .before = "GEOID")

acs10_tract_for_chg <- acs10_tract %>%
  mutate_tract_id(.) %>% 
  mutate(NOWN = (OWN_2010AFTER + OWN_2000T2009), 
         NRENT = (RENT_2010AFTER + RENT_2000T2009)
  ) %>%
  mutate(PCOLL = (COLL/POP_O25) * 100,
         PNOWN = (NOWN/OWN_OCC_D) * 100,
         PNRENT = (NRENT/RENT_OCC_D) * 100
  ) %>% 
  rename_at(vars(MD_HINC, MD_HVAL, MD_RVAL),
            function(x) gsub("_", "", x)
  ) %>%
  select(GEOID,
         COLL, 
         PCOLL,
         MDHINC,
         NOWN,
         PNOWN,
         NRENT,
         PNRENT,
         MDHVAL,
         MDRVAL
  ) %>%
  rename_at(vars(-GEOID),function(x) paste0(x,"10"))

all10_tract_for_stack <- census10_tract_tenure_for_stack %>%
  left_join(., acs10_tract_for_stack, by = c("GEOID", "year")) %>%
  relocate(year,
           GEOID, 
           COLL, 
           PCOLL,
           MDHINC,
           HU_OCC,
           HU_OWN,
           POWN, 
           HU_RENT,
           NOWN,
           PNOWN,
           NRENT,
           PNRENT,
           MDHVAL,
           MDRVAL)

all10_tract_for_chg <- census10_tract_tenure_for_chg %>%
  left_join(., acs10_tract_for_chg, by = "GEOID") %>%
  relocate(GEOID, 
           COLL10, 
           PCOLL10,
           MDHINC10,
           HU_OCC10,
           HU_OWN10,
           POWN10, 
           HU_RENT10,
           NOWN10,
           PNOWN10,
           NRENT10,
           PNRENT10,
           MDHVAL10,
           MDRVAL10)


# 2000 PREP
census00_tract_for_stack <- census00_tract %>%
  mutate_tract_id(.) %>%
  mutate(NOWN = (OWN_AFTER1999 + OWN_1995T1998 + OWN_1990T1994), 
         NRENT = (RENT_AFTER1999 + RENT_1995T1998 + RENT_1990T1994)
  ) %>%
  create_key_vars(.) %>%
  mutate(year = as.Date("01/31/2000", format = "%m/%d/%Y")) %>%
  relocate(year, .before = "GEOID")

census00_tract_for_chg <- census00_tract %>% 
  mutate_tract_id(.) %>%
  mutate(NOWN = (OWN_AFTER1999 + OWN_1995T1998 + OWN_1990T1994), 
         NRENT = (RENT_AFTER1999 + RENT_1995T1998 + RENT_1990T1994)
  ) %>%
  create_key_vars(.) %>%
  rename_at(vars(-GEOID),function(x) paste0(x,"00"))


# 1990 PREP
census90_tract_for_stack <- census90_tract %>%
  mutate_tract_id(.) %>%
  mutate(NOWN = (OWN_AFTER1989 + OWN_1985T1988 + OWN_1980T1984), 
         NRENT = (RENT_AFTER1989 + RENT_1985T1988 + RENT_1980T1984)
  ) %>%
  create_key_vars(.) %>%
  mutate(year = as.Date("01/31/1990", format = "%m/%d/%Y")) %>%
  relocate(year, .before = "GEOID")

census90_tract_for_chg <- census90_tract %>%
  mutate_tract_id(.) %>%
  mutate(NOWN = (OWN_AFTER1989 + OWN_1985T1988 + OWN_1980T1984), 
         NRENT = (RENT_AFTER1989 + RENT_1985T1988 + RENT_1980T1984)
  ) %>%
  create_key_vars(.) %>%
  rename_at(vars(-GEOID),function(x) paste0(x,"90"))


## CLEAN SUPER-UNIT DEMOGRAPHIC VARIABLES----------------------------------------
format_place_identifier <- function(x) { 
  x %>%       
    mutate(PLACEFP = as.character(GEOID),
           PLACEFP = str_pad(PLACEFP, width = 7, side = "left", pad = "0")
    ) %>%
    relocate(PLACEFP, .before = "GEOID") 
}

compute_place_pctchg <- function(x) { 
  x %>%       
    mutate(
      CPCOWN = ((CPOWN20 - CPOWN10)/CPOWN10) * 100,
      CPCCOLL = ((CPCOLL20 - CPCOLL10)/CPCOLL10) * 100,
      CPCMDHIN = ((CMDHINC20 - CMDHINC10)/CMDHINC10) * 100,
      CPCMDHVA = ((CMDHVAL20 - CMDHVAL10)/CMDHVAL10) * 100,
      CPCMDRVA = ((CMDRVAL20 - CMDRVAL10)/CMDRVAL10) * 100,
      CPCNOWN = ((CPNOWN20 - CPNOWN10)/CPNOWN10) * 100,
      CPCNRENT = ((CPNRENT20 - CPNRENT10)/CPNRENT10) * 100
    ) %>%
    mutate(across(starts_with("CPC_"), ~na_if(., Inf)))
}

compute_place_pctchg00T10 <- function(x) { 
  x %>%       
    mutate(
      CPCOWN = ((CPOWN10 - CPOWN00)/CPOWN00) * 100,
      CPCCOLL = ((CPCOLL10 - CPCOLL00)/CPCOLL00) * 100,
      CPCMDHIN = ((CMDHINC10 - CMDHINC00)/CMDHINC00) * 100,
      CPCMDHVA = ((CMDHVAL10 - CMDHVAL00)/CMDHVAL00) * 100,
      CPCMDRVA = ((CMDRVAL10 - CMDRVAL00)/CMDRVAL00) * 100,
      CPCNOWN = ((CPNOWN10 - CPNOWN00)/CPNOWN00) * 100,
      CPCNRENT = ((CPNRENT10 - CPNRENT00)/CPNRENT00) * 100
    ) %>%
    mutate(across(starts_with("CPC_"), ~na_if(., Inf)))
}

compute_place_pctchg90T00 <- function(x) { 
  x %>%       
    mutate(
      CPCOWN = ((CPOWN00 - CPOWN90)/CPOWN90) * 100,
      CPCCOLL = ((CPCOLL00 - CPCOLL90)/CPCOLL90) * 100,
      CPCMDHIN = ((CMDHINC00 - CMDHINC90)/CMDHINC90) * 100,
      CPCMDHVA = ((CMDHVAL00 - CMDHVAL90)/CMDHVAL90) * 100,
      CPCMDRVA = ((CMDRVAL00 - CMDRVAL90)/CMDRVAL90) * 100
    ) %>%
    mutate(across(starts_with("CPC_"), ~na_if(., Inf)))
}


# CITY
  # 2020
acs19_place_for_chg <- acs19_place %>% 
  mutate(
    NRENT = (RENTER_AFTER2014 + RENTER_2010T2013),
    NOWN = (OCC_AFTER2014 + OCC_2010T2013) - NRENT
  ) %>%
  create_key_vars(.) %>%
  format_place_identifier(.) %>%
  relocate(PLACEFP, .before = "GEOID") %>%
  rename_at(vars(-PLACEFP, -GEOID), function(x) paste0("C", x, "20"))

  # 2010
acs10_place_for_chg <- acs10_place %>% 
  mutate(
    NRENT = (RENTER_OCC_AFTER2010 + RENTER_OCC_2000T2009),
    NOWN = (OCC_AFTER2010 + OCC_2000T2009) - NRENT
  ) %>% 
  format_place_identifier(.) 

census10_place_tenure_for_chg <- census10_tenure_place %>%
  mutate(POWN = (HU_OWN/HU_OCC) * 100) %>% 
  format_place_identifier(.)  %>%
  select(-HU_OCC)

all10_place_for_chg <- census10_place_tenure_for_chg %>%
  left_join(., acs10_place_for_chg, by = c("PLACEFP", "GEOID")
            ) %>%
  mutate(PCOLL = (COLL/POP_O25) * 100,
         POWN = (HU_OWN/HU_OCC) * 100,
         PNOWN = (NOWN/HU_OWN) * 100,
         PNRENT = (NRENT/HU_RENT) * 100
  ) %>% 
  rename_at(vars(MD_HINC, MD_HVAL, MD_RVAL),
            function(x) gsub("_", "", x)
  ) %>%
  select(PLACEFP,
         GEOID,
         CITY,
         COLL, 
         PCOLL,
         MDHINC,
         HU_OCC,
         HU_OWN,
         POWN, 
         HU_RENT,
         NOWN,
         PNOWN,
         NRENT,
         PNRENT,
         MDHVAL,
         MDRVAL
  ) %>% 
  rename_at(vars(-PLACEFP:-CITY), function(x) paste0("C", x, "10"))

  # 2000
census00_place_for_chg <- census00_place %>%
  mutate(
    NRENT = (RENT_AFTER1999 + RENT_1995T1998 + RENT_1990T1994),
    NOWN = (OWN_AFTER1999 + OWN_1995T1998 + OWN_1990T1994)
  ) %>%
  create_key_vars(.) %>%
  format_place_identifier(.) %>%
  relocate(PLACEFP, .before = "GEOID") %>%
  rename_at(vars(-PLACEFP, -GEOID), function(x) paste0("C", x, "00"))
  
  # 1990
census90_place_for_chg <- census90_place %>%
  create_key_vars(.) %>%
  format_place_identifier(.) %>%
  relocate(PLACEFP, .before = "GEOID") %>%
  rename_at(vars(-PLACEFP, -GEOID), function(x) paste0("C", x, "90")
            ) %>%
  left_join(., all10_place_for_chg %>% select(GEOID, PLACEFP, CITY), by = c("GEOID", "PLACEFP")
            ) %>%
  relocate(CITY, .after = "GEOID")

## JOIN DECADES
# 2010 & 2020
class_places_vars_10T20 <- all10_place_for_chg %>%
  left_join(., acs19_place_for_chg, by = c("PLACEFP", "GEOID")
            ) %>%
  compute_place_pctchg(.) %>%
  rename_at(vars(starts_with("PC_")), function(x) paste0("C", x)) %>%
  select(-GEOID)

class_places_for_stack_10T20 <- class_places_vars_10T20 %>%
  mutate(PERIOD = "2010 to 2020") %>%
  select(PLACEFP, CITY, PERIOD, CPCCOLL:CPCMDRVA)


# 2000 & 2010
class_places_vars_00T10 <-  census00_place_for_chg %>%
  left_join(., all10_place_for_chg, by = c("PLACEFP", "GEOID")
  ) %>%
  compute_place_pctchg00T10(.) %>%
  rename_at(vars(starts_with("PC_")), function(x) paste0("C", x)) %>%
  select(-GEOID)

class_places_for_stack_00T10 <- class_places_vars_00T10 %>%
  mutate(PERIOD = "2000 to 2010") %>%
  select(PLACEFP, CITY, PERIOD, CPCCOLL:CPCMDRVA)


# 1990 & 2000
class_places_vars_90T00 <-  census90_place_for_chg %>%
  left_join(., census00_place_for_chg, by = c("PLACEFP", "GEOID")
  ) %>%
  compute_place_pctchg90T00(.) %>%
  rename_at(vars(starts_with("PC_")), function(x) paste0("C", x)) %>% 
  select(-GEOID)

class_places_for_stack_90T00 <- class_places_vars_90T00 %>%
  mutate(PERIOD = "1990 to 2000") %>%
  select(PLACEFP, CITY, PERIOD, CPCCOLL:CPCMDRVA)


## STACK CITY CHANGE VARIABLES
stacked_gent_indicators_city_change <- bind_rows(class_places_for_stack_90T00,
          class_places_for_stack_00T10,
          class_places_for_stack_10T20)

gent_indicators_city_change_stacked <- stacked_gent_indicators_city_change %>%
  arrange(CITY, PERIOD) %>%
  mutate(across(where(is.numeric), ~percent(., accuracy = 0.01, scale = 1, suffix = "%", decimal.mark = ".")))
write_csv(gent_indicators_city_change_stacked,
          "city_decade_pchg_gent_indicators2.csv")


## CREATE SHAPEFILES------------------------------------------------------------
# STACKED GENT HOUSING & SES INDICATORS 
census90_tract_for_stack #15 74002
census00_tract_for_stack #15, 74002
all10_tract_for_stack #15, 74002
acs19_tract_for_stack #15, 74001

us_tracts_90T20_SES_housing <- bind_rows(
  census90_tract_for_stack,
  census00_tract_for_stack,
  all10_tract_for_stack,
  acs19_tract_for_stack
)

class_tracts_90T20_SES_housing <- us_tracts_90T20_SES_housing %>%
  left_join(., all_geog_identifiers_per_tract, by = "GEOID") %>%
  select(-TRTID10, -cbsa10:-ccflag10) %>%
  mutate(placefp10 = as.character(placefp10),
         place = str_pad(placefp10, width = 5, side = "left", pad = "0"), 
         STATE = str_sub(GEOID, end = 2),
         place = str_c(STATE, place, sep = "")
         ) %>%
  filter(place %in% class_places_vars_10T20$PLACEFP)

class_tracts_90T20_SES_housing_shp <- census_tracts %>% 
  select(GEOID10, TRACTCE10) %>%
  right_join(class_tracts_90T20_SES_housing, by = c("GEOID10" = "GEOID"))



# CREATE SHAPEFILES FOR EACH CITY & EXPORT--------------------------------------
sf_tracts_ses_housing_xt <- class_tracts_90T20_SES_housing_shp %>%
  filter(county == "San Francisco County" & GEOID10 != "06075980401") %>%
  select(year,
         GEOID10,
         TRACTCE10,
         PCOLL,
         MDHINC,
         POWN,
         MDHVAL,
         MDRVAL,
         PNOWN,
         PNRENT,
         COLL,
         HU_OWN,
         HU_RENT,
         NOWN,
         NRENT
         )
st_write(sf_tracts_ses_housing_xt, "San_Francisco_tracts_SES_housing_xt.shp")



# CHANGE OVER DECADES IN GENT INDICATORS----------------------------------------
compute_pctchg_10T20 <- function(x) { 
  x %>%       
    mutate(
      PCOWN = ((POWN20 - POWN10)/POWN10) * 100,
      PCCOLL = ((PCOLL20 - PCOLL10)/PCOLL10) * 100,
      PCMDHIN = ((MDHINC20 - MDHINC10)/MDHINC10) * 100,
      PCMDHVA = ((MDHVAL20 - MDHVAL10)/MDHVAL10) * 100,
      PCMDRVA = ((MDRVAL20 - MDRVAL10)/MDRVAL10) * 100,
      PCNOWN = ((PNOWN20 - PNOWN10)/PNOWN10) * 100,
      PCNRENT = ((PNRENT20 - PNRENT10)/PNRENT10) * 100
    ) %>%
    mutate(across(starts_with("PC_"), ~na_if(., Inf)))
}

compute_pctchg_00T10 <- function(x) { 
  x %>%       
    mutate(
      PCOWN = ((POWN10 - POWN00)/POWN00) * 100,
      PCCOLL = ((PCOLL10 - PCOLL00)/PCOLL00) * 100,
      PCMDHIN = ((MDHINC10 - MDHINC00)/MDHINC00) * 100,
      PCMDHVA = ((MDHVAL10 - MDHVAL00)/MDHVAL00) * 100,
      PCMDRVA = ((MDRVAL10 - MDRVAL00)/MDRVAL00) * 100,
      PCNOWN = ((PNOWN10 - PNOWN00)/PNOWN00) * 100,
      PCNRENT = ((PNRENT10 - PNRENT00)/PNRENT00) * 100
    ) %>%
    mutate(across(starts_with("PC_"), ~na_if(., Inf)))
}

compute_pctchg_90T00 <- function(x) { 
  x %>%       
    mutate(
      PCOWN = ((POWN00 - POWN90)/POWN00) * 100,
      PCCOLL = ((PCOLL00 - PCOLL90)/PCOLL90) * 100,
      PCMDHIN = ((MDHINC00 - MDHINC90)/MDHINC90) * 100,
      PCMDHVA = ((MDHVAL00 - MDHVAL90)/MDHVAL90) * 100,
      PCMDRVA = ((MDRVAL00 - MDRVAL90)/MDRVAL90) * 100
    ) %>%
    mutate(across(starts_with("PC_"), ~na_if(., Inf)))
}


## 2010 to 2020
tracts_us_10to20 <- all10_tract_for_chg %>%
  left_join(., acs19_tract_for_chg, by = "GEOID") %>%
  compute_pctchg_10T20(.)  %>%
  left_join(., all_geog_identifiers_per_tract, by =  "GEOID") %>% 
  mutate(placefp10 = as.character(placefp10),
         place = str_pad(placefp10, width = 5, side = "left", pad = "0"), 
         STATE = str_sub(GEOID, end = 2),
         place = str_c(STATE, place, sep = "")
  ) %>%
  right_join(., class_places_vars_10T20, by = c("place" = "PLACEFP")
             ) %>%
  mutate(ELIG = if_else(MDHINC10 < CMDHINC10,
                        "Gentrifiable",
                        "Not Gentrifiable"), 
         ELIG = if_else(is.na(ELIG),
                        "Not Gentrifiable",
                        ELIG),
         HGHVAL = if_else(PCMDHVA > CPCMDHVA | PCMDRVA > CPCMDRVA,
                          "grew faster", 
                          "did not grow faster"),
         HGHVAL = if_else(is.na(HGHVAL),
                          "did not grow faster",
                          HGHVAL),
         HGSES = if_else(PCCOLL > CPCCOLL | PCMDHIN > CPCMDHIN,
                            "grew faster", 
                            "did not grow faster"), 
         HGSES = if_else(is.na(HGSES),
                         "did not grow faster", 
                         HGSES),
         GENTRIFY = if_else(ELIG == "Gentrifiable" & (HGHVAL == "grew faster" & HGSES == "grew faster"),
                            "Gentrifying",
                            "Not Gentrifying"
         ),
         GENTRIFY = if_else(GENTRIFY == "Not Gentrifying" & ELIG == "Not Gentrifiable",
                            "Not Gentrifiable",
                            GENTRIFY
         )
  ) %>%
  select(GEOID,
         CITY,
         PCOLL10,
         MDHINC10,
         MDHVAL10,
         MDRVAL10, 
         PCOLL20,
         MDHINC20, 
         MDHVAL20,
         MDRVAL20, 
         PCCOLL:PCMDRVA,
         ELIG:GENTRIFY) %>%
  mutate(across(where(is.numeric), ~replace(., is.nan(.), NA))) %>%
  mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA))) %>%
  mutate(PERIOD = "2010 to 2020"
         ) %>%
  relocate(PERIOD,
           GEOID,
           CITY
           )

## 2000 TO 2010 
tracts_us_00to10 <- census00_tract_for_chg %>%
  left_join(., all10_tract_for_chg, by = "GEOID") %>%
  compute_pctchg_00T10(.)  %>%
  left_join(., all_geog_identifiers_per_tract, by =  "GEOID") %>% 
  mutate(placefp10 = as.character(placefp10),
         place = str_pad(placefp10, width = 5, side = "left", pad = "0"), 
         STATE = str_sub(GEOID, end = 2),
         place = str_c(STATE, place, sep = "")
  ) %>%
  right_join(., class_places_vars_00T10, by = c("place" = "PLACEFP")
  ) %>%
  mutate(ELIG = if_else(MDHINC00 < CMDHINC00,
                             "Gentrifiable",
                             "Not Gentrifiable"),
         ELIG = if_else(is.na(ELIG),
                        "Not Gentrifiable",
                        ELIG),
         HGHVAL = if_else(PCMDHVA > CPCMDHVA | PCMDRVA > CPCMDRVA,
                          "grew faster", 
                          "did not grow faster"),
         HGHVAL = if_else(is.na(HGHVAL),
                          "did not grow faster",
                          HGHVAL),
         HGSES = if_else(PCCOLL > CPCCOLL | PCMDHIN > CPCMDHIN,
                         "grew faster", 
                         "did not grow faster"), 
         HGSES = if_else(is.na(HGSES),
                         "did not grow faster", 
                         HGSES),
         GENTRIFY = if_else(ELIG == "Gentrifiable" & (HGHVAL == "grew faster" & HGSES == "grew faster"),
                            "Gentrifying",
                            "Not Gentrifying"
         ),
         GENTRIFY = if_else(GENTRIFY == "Not Gentrifying" & ELIG == "Not Gentrifiable",
                            "Not Gentrifiable",
                            GENTRIFY
         )
  ) %>%
  select(GEOID,
         CITY,
         PCOLL00,
         MDHINC00, 
         MDHVAL00,
         MDRVAL00,
         PCOLL10,
         MDHINC10,
         MDHVAL10,
         MDRVAL10,
         PCCOLL:PCMDRVA,
         ELIG:GENTRIFY) %>%
  mutate(across(where(is.numeric), ~replace(., is.nan(.), NA))) %>%
  mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA))) %>%
  mutate(PERIOD = "2000 to 2010"
  ) %>%
  relocate(PERIOD,
           GEOID,
           CITY
  )

## 1990 to 2000
tracts_us_90to00 <- census90_tract_for_chg %>%
  left_join(., census00_tract_for_chg, by = "GEOID") %>%
  compute_pctchg_90T00(.)  %>%
  left_join(., all_geog_identifiers_per_tract, by =  "GEOID") %>% 
  mutate(placefp10 = as.character(placefp10),
         place = str_pad(placefp10, width = 5, side = "left", pad = "0"), 
         STATE = str_sub(GEOID, end = 2),
         place = str_c(STATE, place, sep = "")
  ) %>%
  right_join(., class_places_vars_90T00, by = c("place" = "PLACEFP")
  ) %>%
  mutate(ELIG = if_else(MDHINC90 < CMDHINC90,
                        "Gentrifiable",
                        "Not Gentrifiable"),
         ELIG = if_else(is.na(ELIG),
                        "Not Gentrifiable",
                        ELIG),
         HGHVAL = if_else(PCMDHVA > CPCMDHVA | PCMDRVA > CPCMDRVA,
                          "grew faster", 
                          "did not grow faster"),
         HGHVAL = if_else(is.na(PCMDHVA) & is.na(PCMDRVA),
                          "did not grow faster",
                          HGHVAL),
         HGHVAL = if_else(is.na(HGHVAL),
                          "did not grow faster",
                          HGHVAL),
         HGSES = if_else(PCCOLL > CPCCOLL | PCMDHIN > CPCMDHIN,
                         "grew faster", 
                         "did not grow faster"), 
         HGSES = if_else(is.na(HGSES),
                         "did not grow faster", 
                         HGSES),
         GENTRIFY = if_else(ELIG == "Gentrifiable" & (HGHVAL == "grew faster" & HGSES == "grew faster"),
                            "Gentrifying",
                            "Not Gentrifying"
         ),
         GENTRIFY = if_else(GENTRIFY == "Not Gentrifying" & ELIG == "Not Gentrifiable",
                            "Not Gentrifiable",
                            GENTRIFY
         )
  ) %>%
  select(GEOID,
         CITY,
         PCOLL90,
         MDHINC90,
         MDHVAL90,
         MDRVAL90, 
         PCOLL00,
         MDHINC00, 
         MDHVAL00,
         MDRVAL00,
         PCCOLL:PCMDRVA,
         ELIG:GENTRIFY) %>%
  mutate(across(where(is.numeric), ~replace(., is.nan(.), NA))) %>%
  mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA))) %>%
  mutate(PERIOD = "1990 to 2000"
  ) %>%
  relocate(PERIOD,
           GEOID,
           CITY
  )


# STACK ALL DECADES
class_tracts_gent_by_decade <- bind_rows(
  tracts_us_90to00,
  tracts_us_00to10,
  tracts_us_10to20
)

class_tracts_gent_by_decade_shp <- census_tracts %>% 
  select(GEOID10, TRACTCE10) %>%
  right_join(., class_tracts_gent_by_decade, by = c("GEOID10" = "GEOID")
             )

class_tracts_gent_90t00_shp <- census_tracts %>% 
  select(GEOID10, TRACTCE10) %>%
  right_join(., tracts_us_90to00, by = c("GEOID10" = "GEOID")
  )
  
class_tracts_gent_00t10_shp <- census_tracts %>% 
  select(GEOID10, TRACTCE10) %>%
  right_join(., tracts_us_00to10, by = c("GEOID10" = "GEOID")
  )
  
class_tracts_gent_10t20_shp <- census_tracts %>% 
  select(GEOID10, TRACTCE10) %>%
  right_join(., tracts_us_10to20, by = c("GEOID10" = "GEOID")
  )
  

# EXPORT GENTRIFICATION PERIODS SHAPEFILES--------------------------------------
## SF 
sf_tracts_gent_by_decade <- class_tracts_gent_by_decade_shp %>%
  filter(CITY == "San Francisco city" & GEOID10 != "06075980401") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
         )
st_write(sf_tracts_gent_by_decade, "San_Francisco_tracts_gentrification_by_decade.shp")

sf_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "San Francisco city" & GEOID10 != "06075980401") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(sf_tracts_gent_90t00, "San_Francisco_tracts_gent_1990_2000.shp")

sf_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "San Francisco city" & GEOID10 != "06075980401") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(sf_tracts_gent_00t10, "San_Francisco_tracts_gent_2000_2010.shp")

sf_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "San Francisco city" & GEOID10 != "06075980401") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(sf_tracts_gent_10t20, "San_Francisco_tracts_gent_2010_2020.shp")

    ## Super-gentrification
sf_tracts_supergent_90t00 <- sf_tracts_gent_90t00 %>%
  mutate(SUPER = if_else(
    GENTRIFY == "Not Gentrifiable" & HGHVAL == "grew faster" & HGSES == "grew faster",
    "Super-gentrifying",
    GENTRIFY)
    )

sf_gent_status_90s_extract <- sf_tracts_supergent_90t00 %>% 
  select(GEOID10, GENTRIFY90S = "GENTRIFY", SUPER90S = "SUPER") %>%
  st_set_geometry(NULL)

sf_tracts_supergent_00t10 <- sf_tracts_gent_00t10 %>% 
  left_join(., sf_gent_status_90s_extract, by = "GEOID10") %>%
  mutate(SUPER = if_else(
    (SUPER90S == "Super-gentrifying" | SUPER90S == "Gentrifying") & GENTRIFY == "Not Gentrifiable" & HGHVAL == "grew faster" & HGSES == "grew faster",
    "Super-gentrifying",
    GENTRIFY)
  ) %>%
  select(-SUPER90S, -GENTRIFY90S)

sf_gent_status_00s_extract <- sf_tracts_supergent_00t10 %>% 
  select(GEOID10, GENTRIFY00S = "GENTRIFY", SUPER00S = "SUPER") %>%
  st_set_geometry(NULL)

sf_tracts_supergent_10t20 <- sf_tracts_gent_10t20 %>% 
  left_join(., sf_gent_status_00s_extract, by = "GEOID10") %>%
  mutate(SUPER = if_else(
    (SUPER00S == "Super-gentrifying" | SUPER00S == "Gentrifying") & GENTRIFY == "Not Gentrifiable" & HGHVAL == "grew faster" & HGSES == "grew faster",
    "Super-gentrifying",
    GENTRIFY)
  ) %>%
  select(-SUPER00S, -GENTRIFY00S)

st_write(sf_tracts_supergent_00t10, "San_Francisco_tracts_supergent_2000_2010.shp")
st_write(sf_tracts_supergent_10t20, "San_Francisco_tracts_supergent_2010_2020.shp")



## SJ
sj_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "San Jose city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(sj_tracts_gent_90t00, "San_Jose_tracts_gent_1990_2000.shp")

sj_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "San Jose city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(sj_tracts_gent_00t10, "San_Jose_tracts_gent_2000_2010.shp")

sj_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "San Jose city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(sj_tracts_gent_10t20, "San_Jose_tracts_gent_2010_2020.shp")

## OAKLAND
oak_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Oakland city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(oak_tracts_gent_90t00, "Oakland_tracts_gent_1990_2000.shp")

oak_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Oakland city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(oak_tracts_gent_00t10, "Oakland_tracts_gent_2000_2010.shp")

oak_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Oakland city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(oak_tracts_gent_10t20, "Oakland_tracts_gent_2010_2020.shp")

## Fremont
fremont_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Fremont city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(fremont_tracts_gent_90t00, "Fremont_tracts_gent_1990_2000.shp")

fremont_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Fremont city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(fremont_tracts_gent_00t10, "Fremont_tracts_gent_2000_2010.shp")

fremont_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Fremont city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(fremont_tracts_gent_10t20, "Fremont_tracts_gent_2010_2020.shp")

## San Mateo
sm_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "San Mateo city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(sm_tracts_gent_90t00, "San_Mateo_tracts_gent_1990_2000.shp")

sm_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "San Mateo city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(sm_tracts_gent_00t10, "San_Mateo_tracts_gent_2000_2010.shp")

sm_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "San Mateo city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(sm_tracts_gent_10t20, "San_Mateo_tracts_gent_2010_2020.shp")

## EPA
epa_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "East Palo Alto city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(epa_tracts_gent_90t00, "East_Palo_Alto_tracts_gent_1990_2000.shp")

epa_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "East Palo Alto city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(epa_tracts_gent_00t10, "East_Palo_Alto_tracts_gent_2000_2010.shp")

epa_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "East Palo Alto city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(epa_tracts_gent_10t20, "East_Palo_Alto_tracts_gent_2010_2020.shp")

## PA
pa_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Palo Alto city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(pa_tracts_gent_90t00, "Palo_Alto_tracts_gent_1990_2000.shp")

class_tracts_gent_90t00_shp %>%
  filter(CITY == "Palo Alto city") %>% 
  summarize(city_median_hinc = median(MDHINC90))

pa_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Palo Alto city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(pa_tracts_gent_00t10, "Palo_Alto_tracts_gent_2000_2010.shp")

pa_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Palo Alto city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(pa_tracts_gent_10t20, "Palo_Alto_tracts_gent_2010_2020.shp")
         
         
## CHINO
chino_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Chino city") %>%
  mutate(HGHVAL = if_else(is.na(PCMDHVA) & is.na(PCMDRVA),
                          "did not grow faster",
                          HGHVAL),
         GENTRIFY = if_else(HGHVAL == "did not grow faster" & HGSES == "grew faster" & ELIG == "Gentrifiable",
                            "Not Gentrifying",
                            GENTRIFY)
  )
st_write(chino_tracts_gent_90t00, "Chino_tracts_gent_1990_2000.shp")

chino_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Chino city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(chino_tracts_gent_00t10, "Chino_tracts_gent_2000_2010.shp")

chino_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Chino city") %>%
  mutate(GENTRIFY = replace_na(GENTRIFY, "Gentrifying")
  )
st_write(chino_tracts_gent_10t20, "Chino_tracts_gent_2010_2020.shp")

## Westminster
west_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Westminster city")
st_write(west_tracts_gent_90t00, "Westminster_tracts_gent_1990_2000.shp")

west_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Westminster city")
st_write(west_tracts_gent_00t10, "Westminster_tracts_gent_2000_2010.shp")

west_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Westminster city")
st_write(west_tracts_gent_10t20, "Westminster_tracts_gent_2010_2020.shp")

## LA
la_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Los Angeles city")
st_write(la_tracts_gent_90t00, "Los_Angeles_tracts_gent_1990_2000.shp")

la_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Los Angeles city")
st_write(la_tracts_gent_00t10, "Los_Angeles_tracts_gent_2000_2010.shp")

la_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Los Angeles city")
st_write(la_tracts_gent_10t20, "Los_Angeles_tracts_gent_2010_2020.shp")

## BAKERSFIELD 
bake_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Bakersfield city")
st_write(bake_tracts_gent_90t00, "Bakersfield_tracts_gent_1990_2000.shp")

bake_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Bakersfield city")
st_write(bake_tracts_gent_00t10, "Bakersfield_tracts_gent_2000_2010.shp")

bake_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Bakersfield city")
st_write(bake_tracts_gent_10t20, "Bakersfield_tracts_gent_2010_2020.shp")


## KC 
kc_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Kansas City city") %>%
  mutate(HGSES = if_else(is.na(PCCOLL) & is.na(PCMDHIN),
                         "did not grow faster",
                         HGSES),
         HGHVAL = if_else(is.na(PCMDHVA) & is.na(PCMDRVA),
                          "did not grow faster",
                          HGHVAL),
         GENTRIFY = if_else(HGHVAL == "did not grow faster" & HGSES == "grew faster" & ELIG == "Gentrifiable",
                            "Not Gentrifying",
                            GENTRIFY),
         GENTRIFY = if_else(HGHVAL == "did not grow faster" & HGSES == "did not grow faster" & ELIG == "Gentrifiable",
                            "Not Gentrifying",
                            GENTRIFY)
  )
st_write(kc_tracts_gent_90t00, "Kansas_City_tracts_gent_1990_2000.shp")

kc_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Kansas City city")
st_write(kc_tracts_gent_00t10, "Kansas_City_tracts_gent_2000_2010.shp")

kc_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Kansas City city")
st_write(kc_tracts_gent_10t20, "Kansas_City_tracts_gent_2010_2020.shp")

## YAKIMA
yakima_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Yakima city")
st_write(yakima_tracts_gent_90t00, "Yakima_tracts_gent_1990_2000.shp")

yakima_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Yakima city")
st_write(yakima_tracts_gent_00t10, "Yakima_tracts_gent_2000_2010.shp")

yakima_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Yakima city")
st_write(yakima_tracts_gent_10t20, "Yakima_tracts_gent_2010_2020.shp")

## PITTSBURGH
pitt_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Pittsburgh city")
st_write(pitt_tracts_gent_90t00, "Pittsburgh_tracts_gent_1990_2000.shp")

pitt_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Pittsburgh city")
st_write(pitt_tracts_gent_00t10, "Pittsburgh_tracts_gent_2000_2010.shp")

pitt_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Pittsburgh city")
st_write(pitt_tracts_gent_10t20, "Pittsburgh_tracts_gent_2010_2020.shp")

## DETROIT
detroit_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Detroit city")
st_write(detroit_tracts_gent_90t00, "Detroit_tracts_gent_1990_2000.shp")

detroit_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Detroit city")
st_write(detroit_tracts_gent_00t10, "Detroit_tracts_gent_2000_2010.shp")

detroit_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Detroit city")
st_write(detroit_tracts_gent_10t20, "Detroit_tracts_gent_2010_2020.shp")

## DENVER
denver_tracts_gent_90t00 <- class_tracts_gent_90t00_shp %>%
  filter(CITY == "Denver city")
st_write(denver_tracts_gent_90t00, "Denver_tracts_gent_1990_2000.shp")

denver_tracts_gent_00t10 <- class_tracts_gent_00t10_shp %>%
  filter(CITY == "Denver city")
st_write(denver_tracts_gent_00t10, "Denver_tracts_gent_2000_2010.shp")

denver_tracts_gent_10t20 <- class_tracts_gent_10t20_shp %>%
  filter(CITY == "Denver city")
st_write(denver_tracts_gent_10t20, "Denver_tracts_gent_2010_2020.shp")


# View shapefile
gent_pal <- colorFactor(
  palette = c('darkorchid4', 'darkgrey', 'violet', 'green'),
  domain = sf_tracts_supergent_10t20$SUPER
)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = sf_tracts_supergent_10t20 %>% 
                st_transform(., crs = 4326),
              fillColor = ~gent_pal(SUPER),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              label = ~(SUPER)
  )
