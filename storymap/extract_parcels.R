#-------------------------------------------------------------------------------
# EXTRACT PARCEL DATA FROM SF NEIGHBORHOOD 
#
# AUTHOR: Francine Stephens
# DATE CREATED: 5/23/21
# LAST UPDATED: 6/4/21
#-------------------------------------------------------------------------------

## SETUP------------------------------------------------------------------------
# Load Libraries
packages <- c(
  "readxl",
  "tidyverse",
  "sf",
  "ggplot2",
  "plotly",
  "tigris",
  "leaflet",
  "RColorBrewer", 
  "censusapi", 
  "tidycensus",
  "mapboxapi",
  "stargazer"
)
lapply(packages, library, character.only = T)

# Set Parameters
census_api_key("99ccb52a629609683f17f804ca875115e3f0804c")
mb_access_token("sk.eyJ1IjoiZnJhbmNpbmVzdGVwaGVucyIsImEiOiJja2ljb3VrczMwdDdhMnhsNjA4Yjh1c2h1In0.WJjq6TysT6zZZnaxsN0s5g")
readRenviron("~/.Renviron")
sample_tracts <- c("025500",
                   "026100",
                   "026301",
                   "026004",
                   "026001",
                   "026003",
                   "026002")
wgs <- 4326
vacant_codes <-  c('V',
                   'VA1',
                   'VCI',
                   'VPU',
                   'VR')
condo_codes <- c('Z',
                 'ZBM',
                 'CZ',
                 'OZ'
                 )

## PATHS
setwd("~/Stanford/SOC176/soc176-data-processing/storymap")
wd <- getwd()
parceldata_path <- "C:/Users/Franc/Documents/Stanford/landuse_SF"
epa_parcels <- "/EPA Zoning/"


# DATA IMPORTS

## San Francisco
sf_parcels_shape <- 
  st_read("https://data.sfgov.org/api/geospatial/acdm-wktn?method=export&format=GeoJSON") %>%
  filter(active == "true") %>% 
  select(
    apn = blklot,
    zoning = zoning_code,
    zoning_desc = zoning_district
  )

sf_secured <- readRDS(paste0(parceldata_path,
                             "/sf_secured.rds"))
datakey <- readRDS(paste0(parceldata_path, 
                          "/datakey.rds"))
usecode <- readRDS(paste0(parceldata_path, 
                          "/usecode.rds"))

sf_tracts <- tracts(state = "CA", 
                    county = "San Francisco",
                    cb = T,
                    progress_bar = F) %>%
  st_transform(wgs)

## EPA Parcel Data
epa_parcels_shp <- 
  st_read(paste0(wd, 
                 epa_parcels,
                 "Final_Zoning.shp"),
          quiet = F) 


# PREPARE PARCEL DATA FOR EXTRACTION--------------------------------------------

## San Francisco subsets
sf_subset <- sf_tracts %>%
  filter(TRACTCE %in% sample_tracts)

# Link parcels and property data on block lot id
sf_parcels <- sf_parcels_shape %>% 
  left_join(
    sf_secured %>% 
      mutate(
        apn = RP1PRCLID %>% 
          str_replace(" ","")
      )
  )

subset_parcels <- sf_parcels %>% 
  st_centroid() %>% 
  .[sf_subset, ] %>% 
  st_set_geometry(NULL) %>% 
  left_join(sf_parcels %>% select(apn)) %>% 
  st_as_sf() %>%
  st_transform(., 7131) 


subset_parcels_grouped <- subset_parcels %>%
  select(apn: zoning_desc, RP1CLACDE) %>%
  mutate(zone_gen = case_when(
            str_detect(zoning_desc, "COMMERCIAL") ~ "Commercial",
            str_detect(zoning_desc, "RESIDENTIAL") ~ "Residential",
            str_detect(zoning_desc, "PUBLIC") ~ "Public"),
         zone_gen = as.factor(zone_gen),
         vacant = as.factor(if_else(RP1CLACDE %in% vacant_codes,
                          "Vacant",
                          "Not Vacant")
                            ),
         condos = as.factor(if_else(RP1CLACDE %in% condo_codes,
                                    "Condo",
                                    "Not a Condo")
         )
         )

## City-Center EPA general zoning codes
epa_parcels_output <- epa_parcels_shp %>% 
  st_transform(wgs) %>%
  mutate(gen_zones = case_when(
    str_detect(GPLU, "Commercial") ~ "Commercial",
    str_detect(GPLU, "Residential") ~ "Residential",
    str_detect(GPLU, "Industrial") ~ "Industrial",
    str_detect(GPLU, "Open Space") ~ "Public Open Space",
    str_detect(GPLU, "Office") ~ "Office",
    str_detect(GPLU, "Resource Management") ~ "Conservation Areas"),
    gen_zones = as.factor(gen_zones)
    )

# View tract subset
leaflet() %>% 
  addMapboxTiles(
    style_id = "streets-v11",
    username = "mapbox"
  ) %>% 
  addPolygons(
    data = epa_parcels_output,
    fillColor = "blue",
    color = "black",
    weight = 0.5,
    label = ~New_Zone
  )


## VIEW PARCEL GROUPED MAP------------------------------------------------------
zoningpal <- colorFactor(palette = c("blue", "red", "green"), 
                         levels = c("Commercial", "Residential", "Public"))
zoningpal2 <- colorFactor(topo.colors(6), epa_parcels_output$gen_zones)
vacantpal <-  colorFactor(palette = c("green", "red"), 
                          levels = c("Not Vacant", "Vacant")
                          )
condopal <- colorFactor(palette = c("green", "red"), 
                        levels = c("Not a Condo", "Condo")
)

leaflet() %>% 
  addMapboxTiles(
    style_id = "dark-v9",
    username = "mapbox"
  ) %>% 
  addPolygons(
    data = subset_parcels_grouped %>% st_transform(., wgs),
    color = ~condopal(condos),
    weight = 0.5,
    label = ~RP1CLACDE
  )


## EXPORT PARCELS---------------------------------------------------------------
st_write(subset_parcels_grouped, "excelsior_parcels_categorized.shp")
st_write(epa_parcels_output, "EPA_parcels_zoned.shp")
st_write(subset_parcels_grouped, "excelsior_parcels_extra_categories.shp")
