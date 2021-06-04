#-------------------------------------------------------------------------------
# PREP HOLC GRADES DATA LAYER
#
# FRANCINE STEPHENS
# DATE CREATED: 5/28/2021
# DATE LAST UPDATED: 6/3/2021
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
  "scales",
  "mapboxapi"
)
lapply(packages, library, character.only = T)


## PATHS
setwd("~/Stanford/SOC176/soc176-data-processing/storymap")
wd <- getwd()
city_holc_path <- "/OAKLAND_HOLC"                # CHANGE CITY
shp_path <- "/CAOakland1937/cartodb-query.shp"   # CHANGE SHAPEFILE


## PARAMETERS
mb_access_token("sk.eyJ1IjoiZnJhbmNpbmVzdGVwaGVucyIsImEiOiJja2ljb3VrczMwdDdhMnhsNjA4Yjh1c2h1In0.WJjq6TysT6zZZnaxsN0s5g")
readRenviron("~/.Renviron")

A <- "Best"
B <- "Still Desirable"
C <- "Declining"
D <- "Hazardous"

A_description <- 'Only upper- or upper-middle class white neighborhoods that HOLC defined as posing minimal risk for banks and other mortgage lenders as they were "ethnically homogeneous" and had room to be developed.'
B_description <- 'Nearly or completely white, American-born neighborhoods that HOLC defined as "still desirable" and sound investments for mortgage lenders.'
C_description <- 'Neighborhoods where the residents were mostly working-class and/or first or second generation immigrants from Europe. These areas often lacked utilities and were characterized by older building stock.'
D_description <- 'Neighborhoods received this grade because they were poorer and had been "infiltrated" with "undesirable populations" such as Jewish, Asian, Mexican, and Black families. These areas were more likely to be close to industrial areas and to have older housing.'

## IMPORT DATA
holc_import <- st_read(paste0(wd, 
                              city_holc_path,
                              shp_path), 
                       quiet = F)


## DATA MANIPULATION & EXPORT
holc_edited <- holc_import %>%
  select(-name) %>%
  mutate(
    category = case_when(holc_grade == "A" ~ A, 
                         holc_grade == "B" ~ B, 
                         holc_grade == "C" ~ C, 
                         holc_grade == "D" ~ D)
  )



## CHECK VIA MAPPING
gradespal <- colorFactor(topo.colors(4), holc_edited$category)

leaflet() %>% 
  addMapboxTiles(
    style_id = "dark-v9",
    username = "mapbox"
  ) %>% 
  addPolygons(
    data = holc_edited,
    color = ~gradespal(category),
    weight = 0.5,
    label = ~category
  )


## EXPORT
st_write(holc_edited, "Oakland_CA_HOLC_grades_polygons.shp")
