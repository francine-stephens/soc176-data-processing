#-------------------------------------------------------------------------------
# PREPARE CENSUS BLOCK GROUP GENTRIFICATION DATA
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
blockgrps_path <- "2010USA_CensusGeog_Shp/us_blck_grp_2010/"
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

blck_grps <- st_read(paste0(shp_repo, 
                            blockgrps_path,
                            "US_blck_grp_2010.shp"),
                     quiet = F)

state_codes <- c(state.abb, "DC")
us_tracts <- map_df(state_codes, ~tracts(state = .x, cb = FALSE))
us_places <- map_df(state_codes, ~places(state = .x, cb = TRUE))

LTDB00 <- read_csv(paste0(LTDB_geog_ids_path, 
                          "LTDB_Std_2000_fullcount.csv")
)

# 2020 DEMOGS
acs19_bg <- read_csv(paste0(wd,
                            "/BG_2019.csv")
)

acs19_place <- read_csv(paste0(wd,
                               "/PLACE_2019_ACS.csv")
)

## 2010 DEMOGS 
census10_tenure_bg <- read_csv(paste0(wd,
                                      "/BG_2010_Decennial_Tenure.csv")
)

acs10_bg <- read_csv(paste0(wd, 
                            "/BG_2014.csv")
)

census10_tenure_place <- read_csv(paste0(wd,
                                         "/PLACE_2010_Decennial_Tenure.csv")
)

acs10_place <- read_csv(paste0(wd, 
                               "/PLACE_2014_ACS.csv")
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
mutate_blckgrp_id <- function(x) { 
  x %>%       
    mutate(GEOID = as.character(GEOID),
           GEOID = str_pad(GEOID, width = 12, side = "left", pad = "0")
    )
}

# Compute & Name Key Gentrification Variables
create_key_vars <- function(x) { 
  x %>%       
    mutate(PCOLL = (COLL/POP_O25) * 100,
           POWN = (HU_OWN/HU_OCC) * 100,
           PNEW_OWN = (NEW_OWN/HU_OWN) * 100,
           PNEW_RENT = (NEW_RENT/HU_RENT) * 100
    ) %>%
    select(GEOID,
           COLL, 
           PCOLL,
           MD_HINC,
           HU_OCC,
           HU_OWN,
           POWN, 
           HU_RENT,
           NEW_OWN,
           PNEW_OWN,
           NEW_RENT,
           PNEW_RENT,
           MD_HVAL,
           MD_RVAL
    ) 
}


# 2020 PREP
acs19_bg_fmt <- acs19_bg %>%
  mutate_blckgrp_id(.) %>% 
  mutate(NEW_OWN = (OWN_2014LATER + OWN_2010T2013), 
         NEW_RENT = (RENT_2014LATER + RENT_2010T2013)
  ) %>%
  create_key_vars(.) %>%
  rename_at(vars(-GEOID),function(x) paste0(x,"20"))


# 2010 PREP
census10_bg_tenure_fmt <- census10_tenure_bg %>% 
  mutate_blckgrp_id(.) %>%
  mutate(POWN = (HU_OWN/HU_OCC) * 100) %>% 
  rename_at(vars(-GEOID),function(x) paste0(x,"10"))

acs10_bg_fmt <- acs10_bg %>%
  mutate_blckgrp_id(.) %>% 
  mutate(NEW_OWN = (OWN_2010AFTER + OWN_2000T2009), 
         NEW_RENT = (RENT_2010AFTER + RENT_2000T2009)
  ) %>%
  mutate(PCOLL = (COLL/POP_O25) * 100,
         PNEW_OWN = (NEW_OWN/OWN_OCC_D) * 100,
         PNEW_RENT = (NEW_RENT/RENT_OCC_D) * 100
  ) %>%
  select(GEOID,
         COLL, 
         PCOLL,
         MD_HINC,
         NEW_OWN,
         PNEW_OWN,
         NEW_RENT,
         PNEW_RENT,
         MD_HVAL,
         MD_RVAL
  ) %>%
  rename_at(vars(-GEOID),function(x) paste0(x,"10"))

all10_bg_fmt <- blck_grps %>%
  left_join(., 
            census10_bg_tenure_fmt, 
            by = c("GEOID10" = "GEOID")
  ) %>%
  left_join(., acs10_bg_fmt, by = c("GEOID10" = "GEOID")
  )

# 2000 PREP



# 1990 PREP



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
      PC_OWN = ((CPOWN20 - CPOWN10)/CPOWN10) * 100,
      PC_COLL = ((CPCOLL20 - CPCOLL10)/CPCOLL10) * 100,
      PC_MDHIN = ((CMD_HINC20 - CMD_HINC10)/CMD_HINC10) * 100,
      PC_MDHVA = ((CMD_HVAL20 - CMD_HVAL10)/CMD_HVAL10) * 100,
      PC_MDRVA = ((CMD_RVAL20 - CMD_RVAL10)/CMD_RVAL10) * 100,
      PC_NOWN = ((CPNEW_OWN20 - CPNEW_OWN10)/CPNEW_OWN10) * 100,
      PC_NRENT = ((CPNEW_RENT20 - CPNEW_RENT10)/CPNEW_RENT10) * 100
    ) %>%
    mutate(across(starts_with("PC_"), ~na_if(., Inf)))
}


# CITY
acs19_place_fmt <- acs19_place %>% 
  mutate(
    NEW_RENT = (RENTER_AFTER2014 + RENTER_2010T2013),
    NEW_OWN = (OCC_AFTER2014 + OCC_2010T2013) - NEW_RENT
  ) %>%
  create_key_vars(.) %>%
  format_place_identifier(.) %>%
  relocate(PLACEFP, .before = "GEOID") %>%
  rename_at(vars(-PLACEFP, -GEOID), function(x) paste0("C", x, "20"))

acs10_place_fmt <- acs10_place %>% 
  mutate(
    NEW_RENT = (RENTER_OCC_AFTER2010 + RENTER_OCC_2000T2009),
    NEW_OWN = (OCC_AFTER2010 + OCC_2000T2009) - NEW_RENT
  ) %>% 
  format_place_identifier(.) 

census10_place_tenure_fmt <- census10_tenure_place %>%
  mutate(POWN = (HU_OWN/HU_OCC) * 100) %>% 
  format_place_identifier(.)  %>%
  select(-HU_OCC)

all10_place_fmt <- census10_place_tenure_fmt %>%
  left_join(., acs10_place_fmt, by = c("PLACEFP", "GEOID")
  ) %>%
  mutate(PCOLL = (COLL/POP_O25) * 100,
         POWN = (HU_OWN/HU_OCC) * 100,
         PNEW_OWN = (NEW_OWN/HU_OWN) * 100,
         PNEW_RENT = (NEW_RENT/HU_RENT) * 100
  ) %>%
  select(PLACEFP,
         GEOID,
         CITY,
         COLL, 
         PCOLL,
         MD_HINC,
         HU_OCC,
         HU_OWN,
         POWN, 
         HU_RENT,
         NEW_OWN,
         PNEW_OWN,
         NEW_RENT,
         PNEW_RENT,
         MD_HVAL,
         MD_RVAL
  ) %>% 
  rename_at(vars(-PLACEFP:-CITY), function(x) paste0("C", x, "10"))

class_places_vars <- all10_place_fmt %>%
  left_join(., acs19_place_fmt, by = c("PLACEFP", "GEOID")
  ) %>%
  compute_place_pctchg(.) %>%
  rename_at(vars(starts_with("PC_")), function(x) paste0("C", x)) %>%
  select(-GEOID)


# MSAMD


## CREATE SHAPEFILES------------------------------------------------------------
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

# BLOCK GROUPS 
bg_us_10to20 <- all10_bg_fmt %>%
  left_join(., acs19_bg_fmt, by = c("GEOID10" = "GEOID")
  ) %>%
  compute_pctchg(.) 

# Francine's BLOCK GROUPS
excelsior_bg_10to20 <- bg_us_10to20 %>%
  mutate(tractid = str_c(STATEFP10,
                         COUNTYFP10,
                         TRACTCE10, 
                         sep = ""),
         PLACEFP = "0667000"
  ) %>%
  filter(tractid %in% francine_nhood) %>%
  select(-STATEFP10:-COUNTYFP10, -MTFCC10:-Shape_len)

excelsior_bg_10to20 <- excelsior_bg_10to20 %>% 
  left_join(., class_places_vars, by = "PLACEFP") 


excelsior_bg <- excelsior_bg_10to20 %>%
  mutate(GENT_ELIG = if_else(MD_HINC10 > CMD_HINC10,
                             "Gentrifiable",
                             "Not Gentrifiable"),
         HG_HVAL = if_else(PC_MDHVA > CPC_MDHVA | PC_MDRVA > CPC_MDRVA,
                           "Yes", 
                           "No"),
         HG_DEMOG = if_else(PC_COLL > CPC_COLL | PC_MDHIN > CPC_MDHIN,
                            "Yes", 
                            "No"),
         GENTRIFY = if_else(GENT_ELIG == "Gentrifiable" & (HG_HVAL == "Yes" & HG_DEMOG == "Yes"),
                            "Gentrifying",
                            "Not Gentrifying"
         ),
         GENTRIFY = if_else(GENTRIFY == "Not Gentrifying" & GENT_ELIG == "Not Gentrifiable",
                            "Not Gentrifiable",
                            GENTRIFY
         )
  ) %>%
  select(-BLKGRPCE10, -tractid:-CITY, -CCOLL10:-CPC_NRENT) %>%
  mutate(across(where(is.numeric), ~replace(., is.nan(.), NA))) 



# CLASS BLOCK GROUPS
class_bg_10to20 <- bg_us_00to10 %>%
  mutate(tractid = str_c(STATEFP10,
                         COUNTYFP10,
                         TRACTCE10, 
                         sep = "")
  ) %>%
  filter(tractid %in% class_all_geoids$GEOID)
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
