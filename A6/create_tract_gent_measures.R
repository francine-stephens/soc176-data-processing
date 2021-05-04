#-------------------------------------------------------------------------------
# PREPARE CENSUS TRACT GENTRIFICATION DATA
#
# AUTHOR: Francine Stephens
# DATE CREATED: 4/30/21
# LAST UPDATED: 5/4/21
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
           POWN = (HU_OWN/HU_OCC) * 100,
           PNOWN = (NOWN/HU_OWN) * 100,
           PNRENT = (NRENT/HU_RENT) * 100
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
           NOWN,
           PNOWN,
           NRENT,
           PNRENT,
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
      PCOWN = ((CPOWN20 - CPOWN10)/CPOWN10) * 100,
      PCCOLL = ((CPCOLL20 - CPCOLL10)/CPCOLL10) * 100,
      PCMDHIN = ((CMDHINC20 - CMDHINC10)/CMDHINC10) * 100,
      PCMDHVA = ((CMDHVAL20 - CMDHVAL10)/CMDHVAL10) * 100,
      PCMDRVA = ((CMDRVAL20 - CMDRVAL10)/CMDRVAL10) * 100,
      PCNOWN = ((CPNOWN20 - CPNOWN10)/CPNOWN10) * 100,
      PCNRENT = ((CPNRENT20 - CPNRENT10)/CPNRENT10) * 100
    ) %>%
    mutate(across(starts_with("PC_"), ~na_if(., Inf)))
}

compute_place_pctchg00T10 <- function(x) { 
  x %>%       
    mutate(
      PCOWN = ((CPOWN10 - CPOWN00)/CPOWN00) * 100,
      PCCOLL = ((CPCOLL10 - CPCOLL00)/CPCOLL00) * 100,
      PCMDHIN = ((CMDHINC10 - CMDHINC00)/CMDHINC00) * 100,
      PCMDHVA = ((CMDHVAL10 - CMDHVAL00)/CMDHVAL00) * 100,
      PCMDRVA = ((CMDRVAL10 - CMDRVAL00)/CMDRVAL00) * 100,
      PCNOWN = ((CPNOWN10 - CPNOWN00)/CPNOWN00) * 100,
      PCNRENT = ((CPNRENT10 - CPNRENT00)/CPNRENT00) * 100
    ) %>%
    mutate(across(starts_with("PC_"), ~na_if(., Inf)))
}

compute_place_pctchg90T00 <- function(x) { 
  x %>%       
    mutate(
      PCOWN = ((CPOWN00 - CPOWN90)/CPOWN90) * 100,
      PCCOLL = ((CPCOLL00 - CPCOLL90)/CPCOLL90) * 100,
      PCMDHIN = ((CMDHINC00 - CMDHINC90)/CMDHINC90) * 100,
      PCMDHVA = ((CMDHVAL00 - CMDHVAL90)/CMDHVAL90) * 100,
      PCMDRVA = ((CMDRVAL00 - CMDRVAL90)/CMDRVAL90) * 100,
      PCNOWN = ((CPNOWN00 - CPNOWN90)/CPNOWN90) * 100,
      PCNRENT = ((CPNRENT00 - CPNRENT90)/CPNRENT90) * 100
    ) %>%
    mutate(across(starts_with("PC_"), ~na_if(., Inf)))
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
  mutate(
    NRENT = (RENT_AFTER1989 + RENT_1985T1988 + RENT_1980T1984),
    NOWN = (OWN_AFTER1989 + OWN_1985T1988 + OWN_1980T1984)
  ) %>%
  create_key_vars(.) %>%
  format_place_identifier(.) %>%
  relocate(PLACEFP, .before = "GEOID") %>%
  rename_at(vars(-PLACEFP, -GEOID), function(x) paste0("C", x, "90"))

## JOIN DECADES
# 2010 & 2020
class_places_vars_10T20 <- all10_place_for_chg %>%
  left_join(., acs19_place_for_chg, by = c("PLACEFP", "GEOID")
            ) %>%
  compute_place_pctchg(.) %>%
  rename_at(vars(starts_with("PC_")), function(x) paste0("C", x)) %>%
  select(-GEOID)

# 2000 & 2010
class_places_vars_00T10 <-  census00_place_for_chg %>%
  left_join(., all10_place_for_chg, by = c("PLACEFP", "GEOID")
  ) %>%
  compute_place_pctchg00T10(.) %>%
  rename_at(vars(starts_with("PC_")), function(x) paste0("C", x)) %>%
  select(-GEOID)

# 1990 & 2000
class_places_vars_00T10 <-  census90_place_for_chg %>%
  left_join(., census00_place_for_chg, by = c("PLACEFP", "GEOID")
  ) %>%
  compute_place_pctchg90T00(.) %>%
  rename_at(vars(starts_with("PC_")), function(x) paste0("C", x)) %>%
  select(-GEOID)

# MSAMD




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



# CHANGE OVER DECADES IN GENT INDICATORS
compute_pctchg <- function(x) { 
  x %>%       
    mutate(
      PC_OWN = ((POWN20 - POWN10)/POWN10) * 100,
      PC_COLL = ((PCOLL20 - PCOLL10)/PCOLL10) * 100,
      PC_MDHIN = ((MD_HINC20 - MD_HINC10)/MD_HINC10) * 100,
      PC_MDHVA = ((MD_HVAL20 - MD_HVAL10)/MD_HVAL10) * 100,
      PC_MDRVA = ((MD_RVAL20 - MD_RVAL10)/MD_RVAL10) * 100,
      PC_NOWN = ((PNEW_OWN20 - PNEW_OWN10)/PNEW_OWN10) * 100,
      PC_NRENT = ((PNEW_RENT20 - PNEW_RENT10)/PNEW_RENT10) * 100
    ) %>%
    mutate(across(starts_with("PC_"), ~na_if(., Inf)))
}


tracts_us_00to10 <- census_tracts %>%
  left_join(., all10_tract_fmt,  by = c("GEOID10" = "GEOID")
            ) %>%
  left_join(., acs19_tract_fmt, by = c("GEOID10" = "GEOID") 
            ) %>% 
  compute_pctchg(.) 
## ADD CITY MEASURES
## CREATE GENTRIFICATION MEASURES



# View shapefile
gent_pal <- colorFactor(
  palette = c('darkorchid4', 'darkgrey', 'violet'),
  domain = excelsior_bg$GENTRIFY
)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = excelsior_bg %>% 
                st_transform(., crs = 4326),
              fillColor = ~gent_pal(GENTRIFY),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              label = ~(GENTRIFY)
  )

# EXPORT SHAPEFILES-------------------------------------------------------------
st_write(excelsior_bg, "Excelsior_blckgrps_gentrification.shp")
