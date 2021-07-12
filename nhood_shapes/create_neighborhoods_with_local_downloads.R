#-------------------------------------------------------------------------------
# CREATE NEIGHBORHOOD GEOGRAPHIES FOR STUDENTS IN THE SOCIAL LIFE OF NEIGHBORHOODS 
# Version that uses downloaded census tract shapefiles rather than the API.
# You will need  to download the census tracts from the Census TIGER website if
# you use this version of the script. 
#
# GEOGRAPHIES CREATED: Census tracts, Neighborhoods polygons & centroids 
# AUTHOR: Francine Stephens
# DATE CREATED: 4/11/21
# LAST UPDATED: 7/11/21
#-------------------------------------------------------------------------------

## SET-UP-----------------------------------------------------------------------
packages <- c(
  "readr",
  "tidyverse",
  "sf",
  "ggplot2",
  "leaflet"
)
lapply(packages, library, character.only = TRUE)

# Paths & Parameters 
  ## Set your working directory and the paths for the shapefile repository and 
  ## census tracts based on where they are saved locally on your computer.

setwd("~/Stanford/SOC176/soc176-data-processing/nhood_shapes") 
wd <- getwd()
shp_repo <- "C:/Users/Franc/Documents/Shapefile_Repository/"
tracts_path <- "2010USA_CensusGeog_Shp/nhgis0005_shapefile_tl2010_us_tract_2010/"

wgs <- 4326 # geographic projection for web-maps: wgs-1984


# Import Data
student_nhoods <- read_csv(paste0(wd,
                                  "student_neighborhoods_list.csv")
)

census_tracts <- st_read(paste0(shp_repo, 
                                tracts_path,
                                "US_tract_2010.shp"),
                         quiet = FALSE)


## CREATE CLASS NEIGHBORHOOD GEOGRAPHIES----------------------------------------- 

student_nhoods_clean <- student_nhoods %>% 
  mutate(GEOID = str_pad(tract_id, width = 11, side = "left", pad = "0")) %>%
  select(-tract_id) %>%
  mutate(full_nhood_name = str_c(nhood_name, city, state_ab, sep = " "))

# Polygon and point spatial objects for all students neighborhoods
full_class_nhoods_polys <- census_tracts %>%
  right_join(., student_nhoods_clean, by = "GEOID") %>% 
  group_by(student, full_nhood_name, nhood, city, state) %>%
  summarize(GEOIDS = toString(GEOID)) %>% 
  st_transform(., crs = wgs)

full_class_nhoods_cents <- st_centroid(full_class_nhoods_polys)


# View interactive map of the neighborhood geographies
  ## The map plots both polygons and centroids so that you can validate the
  ## boundaries with the boundaries that the students selected in Assignment #1. 
  ## Notice that the centroids will appear huge in county-level view, but if you 
  ## zoom in on the map they will shrink according to the zoom level. Feel free 
  ## to subset by a particular student's neighborhood in the interactive map.

leaflet() %>%
  addTiles() %>%
  addPolygons(data = full_class_nhoods_polys,
              fillColor = NULL,
              color = "Black",
              opacity = 0.5,
              fillOpacity = 0.5,
              weight = 1.5,
              label = ~paste0(nhood_name)
  ) %>%
  addCircleMarkers(data = full_class_nhoods_cents,
                   color = "Blue",
                   label = ~paste0(nhood_name))


## EXPORT NEIGHBORHOOD GEOGRAPHIES----------------------------------------------

# Loop over all unique neighborhoods in the polygons object:
for(nhood in unique(full_class_nhoods_polys$full_nhood_name)) {
  
  ## subset only that neighborhood's data. 
  nhood_df = full_class_nhoods_polys[full_class_nhoods_polys$full_nhood_name==nhood,]
  
  ## construct a file name like full_neighborhood_name.shp and save this data:
  file_name = paste0(nhood, ".shp")
  
  ## create a folder for the shapefile boundaries
  dir.create(paste0(nhood), showWarnings = TRUE) 
  
  ## export neighborhood shapefile
  st_write(nhood_df, paste0(nhood, "/", file_name))
  
  ## zip neighborhood shapefile so that you can upload them to ArcGIS Online
  files2zip <- dir(nhood, full.names = TRUE)
  zip(zipfile = nhood, files = files2zip)
  
}

# Output Neighborhood centroids for the class:
  ## At the end of the quarter, this can be a helpful shapefile for building the
  ## story map that contains all the students' story maps. 
dir.create(paste0("nhood_centroids"), showWarnings = TRUE)
st_write(full_class_nhoods_cents,
         paste0("nhood_centroids/", "class_nhoods_centroids.shp"))
files2zip <- dir(paste0(wd, "/nhood_centroids"), full.names = TRUE)
zip(zipfile = paste0(wd, "/nhood_centroids"), files = files2zip)
