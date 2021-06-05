#------------------------------------------------------
# CREATE DRIVING & WORK ISOCHRONES
# 
# AUTHOR: Francine Stephens
# DATE CREATED: 6/4/21
# LAST UPDATED: 6/4/21
#------------------------------------------------------

## LIBRARIES

## devtools::install_github("jamgreen/lehdr")
packages <- c(
  "readr", 
  "lehdr",
  "tidyverse",
  "sf",
  "ggplot2",
  "plotly",
  "tigris",
  "leaflet",
  "RColorBrewer", 
  "censusapi", 
  "tidycensus", 
  "stargazer",
  "mapboxapi",
  "tidygeocoder",
  "mapboxapi"
)
lapply(packages, library, character.only = T)

## PATHS
census_api_key("99ccb52a629609683f17f804ca875115e3f0804c")
mb_access_token("sk.eyJ1IjoiZnJhbmNpbmVzdGVwaGVucyIsImEiOiJja2ljb3VrczMwdDdhMnhsNjA4Yjh1c2h1In0.WJjq6TysT6zZZnaxsN0s5g")
readRenviron("~/.Renviron")
setwd("~/Stanford/SOC176/soc176-data-processing/storymap")
wd <- getwd()


## IMPORT DATA------------------------------------------------------------------
excelsior_tracts <- c("025500", 
                      "026001",
                      "026002", 
                      "026003",
                      "026004", 
                      "026301",
                      "026100")

excelsior_tracts <- tracts(state = "CA", 
                    county = "San Francisco",
                    cb = T,
                    progress_bar = F) %>% 
  filter(TRACTCE %in% excelsior_tracts) %>%
  st_transform(26910) %>% 
  mutate(original_area = st_area(.)
         )
  

## CREATE ISOCHRONES FOR EXCELSIOR----------------------------------------------
excelsior_tracts_cents <- excelsior_tracts %>% 
  st_centroid()

# 32.8 min is avg commute time to work for san francisco.
drive_32min_excelsior_tracts <- mb_isochrone(
  excelsior_tracts_cents,
  profile = "driving",
  time = 32
)

drive_32min_excelsior_tracts_identifiers <- excelsior_tracts_cents %>% 
  st_set_geometry(NULL) %>% 
  cbind(drive_32min_excelsior_tracts$geometry) %>% 
  st_as_sf()


leaflet() %>% 
  addMapboxTiles(
    style_id = "streets-v11",
    username = "mapbox"
  ) %>%
  addPolygons(
    data = drive_32min_excelsior_tracts_identifiers,
    label = ~NAME
  )


