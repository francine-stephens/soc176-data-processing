#---------------------------------------------------
# CRIME DATA PREP SCRIPT
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 4/13/21
# LAST UPDATED: 4/15/21
#---------------------------------------------------


# SET-UP -----------------------------------------------------------------------
setwd("~/Stanford/SOC176/A4")

#### PARAMETERS ####
socrata_token <- "EZnaKCxOX6zj0uyTkD340TWLh"
email <- "fis@stanford.edu"
pword <- "SFdata2017"
wgs <- 4326

## LIBRARIES 
packages <- c(
  "readxl", 
  "tidyverse",
  "naniar",
  "sf",
  "sp",
  "leaflet",
  "censusapi", 
  "tidycensus", 
  "tidygeocoder", 
  "RSocrata"
)
lapply(packages, library, character.only = T)

wd <- getwd()


# IMPORT DATA-------------------------------------------------------------------

## READ FROM OPEN DATA WEBSITETS ##
sf <- read.socrata("https://data.sfgov.org/resource/wg3w-h783.json", 
                   app_token = socrata_token, 
                   email = email,
                   password  = pword
)

la <- read.socrata("https://data.lacity.org/resource/2nrs-mtv8.csv", 
             app_token = socrata_token, 
             email = email,
             password  = pword
)

kc <- read.socrata("https://data.kcmo.org/resource/w795-ffu6.json", 
                   app_token = socrata_token, 
                   email = email,
                   password  = pword
                   )

detroit <- st_read("https://opendata.arcgis.com/datasets/0825badfe6304620a998d162be0e135e_0.geojson")




## DOWNLOADED/HAND COLLECTED CSV OR SHAPEFILE ##
denver <- st_read(paste0(wd,
                         "/denver_crime_04132021/crime.shp")
                  )

chino <- read_csv(paste0(wd,
                   "/chino_crime_04132021.csv")
            )


# CLEAN UP OPEN DATA & EXPORT --------------------------------------------------
sf_clean <- sf %>%
  mutate_at(vars(latitude:longitude), as.numeric) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  select(-point.latitude:-filed_online)
write_csv(sf_clean, "SF_crime_geocoded.csv", na = "")



# PROCESS ADDRESSES -----------------------------------------------------------
chino_address <- chino %>%
  mutate(
         street_address = str_replace(Location, "00 BLOCK", "50"),
         street_address = str_replace(street_address, "/", " & "),
         city = "Chino",
         state = "CA"
         )

la_reduced <- la %>%
  filter(lat != 0, lon != 0)
write_csv(la_reduced, "LA_crime_04132021_geocoded.csv", na = "")

kc_address <- kc %>%
  mutate(coords = str_extract_all(location.coordinates, "\\([^()]+\\)"),
         coords = substring(coords, 2, nchar(coords)-1)
         ) %>% 
  separate(coords, c("long", "lat"), sep = ", ") %>%
  mutate(lat = as.numeric(lat), 
         long = as.numeric(long))

kc_filtered <- kc_address %>%
  filter(lat != 0 & long != 0)

denver_filtered <- denver %>%
  filter(GEO_LON < - 103 & GEO_LAT >38)


# GEOCODE & EXPORT--------------------------------------------------------------
chino_geocoded <- chino_address %>%
  geocode(street = street_address,
          city = city,
          state = state,
          #postalcode = zip,
          method = 'census',
          lat = lat,
          long = long)

write_csv(chino_geocoded, "chino_crime_04132021_geocoded.csv", na = "")


# VISUALIZE TO CHECK -----------------------------------------------------------
sf <- denver_filtered
  

leaflet() %>% 
  addTiles(
  ) %>%
  addCircleMarkers(
    data = sf,
    radius = 1,
    label = ~NEIGHBORHO
  )

#### FIX OAKLAND#####
oakland <- read_sf(paste0(wd,
               "/Oakland_CrimeWatch_90days/geo_export_1957098c-4dfc-4efc-b4dd-e1c54f9ccd2f.shp")
        )
oakland_sp <- oakland %>%
 st_cast("POINT") #%>% as("Spatial")

