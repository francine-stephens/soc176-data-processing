#-------------------------------------------------------------------------------
# CREATE NEIGHBORHOOD GEOGRAPHIES FOR THE SOCIAL LIFE OF NEIGHBORHOODS 
#
# GEOGRAPHIES CREATED: Census Tracts, Neighborhoods
# AUTHOR: Francine Stephens
# DATE CREATED: 4/11/21
# LAST UPDATED: 6/13/21
#-------------------------------------------------------------------------------

## LIBRARIES
packages <- c(
  "readr",
  "tidyverse",
  "sf",
  "ggplot2",
  "tigris",
  "censusapi", 
  "tidycensus", 
  "leaflet"
)
lapply(packages, library, character.only = T)

## PATHS
setwd("~/Stanford/SOC176/soc176-data-processing/A3")
wd <- getwd()
shp_repo <- "C:/Users/Franc/Documents/Shapefile_Repository/"
tracts_path <- "2010USA_CensusGeog_Shp/nhgis0005_shapefile_tl2010_us_tract_2010/"

# APIs
census_api_key("99ccb52a629609683f17f804ca875115e3f0804c",  overwrite = T)
Sys.setenv(CENSUS_KEY="99ccb52a629609683f17f804ca875115e3f0804c")


## DATA
student_nhoods <- read_csv(paste0(wd,
                                 "student_neighborhoods_list.csv")
)

census_tracts <- st_read(paste0(shp_repo, 
                                tracts_path,
                                "US_tract_2010.shp"),
                         quiet = F)

state_codes <- c(state.abb, "DC")

us_tracts <- map_df(state_codes, ~tracts(state = .x, cb = TRUE))


# SOCIAL LIFE OF NEIGHBORHOOD CENSUS TRACTS------------------------------------- 

## Create full class set of neighborhoods and tracts
student_nhoods <- student_hoods %>% 
  mutate(GEOID = str_pad(CTID, width = 11, side = "left", pad = "0")) %>%
  select(-CTID, -6:-8) %>%
  rename(student = "student_name",
         nhood = "neighborhood_name",
         city = "city_name",
         state = "state_abbrev")

full_class_tracts <- us_tracts %>%
  right_join(., student_nhoods, by = "GEOID")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = full_class_tracts %>% st_transform(., crs = 4326),
              fillColor = NULL,
              color = "Black",
              opacity = 0.5,
              fillOpacity = 0.5,
              weight = 1.5,
              label = ~paste0(nhood)
  )

full_class_nhoods <- full_class_tracts %>% 
  group_by(student, nhood, city, state) %>%
  summarize(GEOIDS = toString(GEOID)) %>% 
  st_transform(., crs = 4326)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = full_class_nhoods,
              fillColor = NULL,
              color = "Black",
              opacity = 0.5,
              fillOpacity = 0.5,
              weight = 1.5,
              label = ~paste0(nhood)
  )


## Export neighborhood shapefile by student
andrew_downtown_sanmateo <- full_class_nhoods %>%
  filter(student == "Andrew")
st_write(andrew_downtown_sanmateo, "nhood_downtown_sanmateo.shp")

whitney_oakknoll_oakland <- full_class_nhoods %>%
  filter(student == "Whitney")
st_write(whitney_oakknoll_oakland, "nhood_oak_knoll_oakland.shp")

kyle_hazelwood_pitt <- full_class_nhoods %>%
  filter(student == "Kyle")
st_write(kyle_hazelwood_pitt, "nhood_hazelwood_pitt.shp")

jordan_mission_sf <- full_class_nhoods %>%
  filter(student == "Jordan")
st_write(jordan_mission_sf, "nhood_mission_sf_jordan.shp")

jessica_downtown_yakima <- full_class_nhoods %>%
  filter(student == "Jessica")
st_write(jessica_downtown_yakima, "nhood_downtown_yakima.shp")

pamela_corkdown_detroit <- full_class_nhoods %>%
  filter(student == "Pamela")
st_write(pamela_corkdown_detroit, "nhood_corktown_detroit.shp")

nashira_montbello_denver <- full_class_nhoods %>%
  filter(student == "Nashira")
st_write(nashira_montbello_denver, "nhood_montbello_denver.shp")

anthony_collegepark_chino <- full_class_nhoods %>%
  filter(student == "Anthony")
st_write(anthony_collegepark_chino, "nhood_college_park_chino.shp")

nicole_duveneck_paloalto <- full_class_nhoods %>%
  filter(student == "Nicole")
st_write(nicole_duveneck_paloalto, "nhood_duveneck_palo_alto.shp")

lauren_ravenswood_epa <- full_class_nhoods %>%
  filter(student == "Lauren")
st_write(lauren_ravenswood_epa, "nhood_ravenswood_epa.shp")

olivia_city_center_epa <- full_class_nhoods %>%
  filter(student == "Olivia")
st_write(olivia_city_center_epa, "nhood_city_center_epa.shp")

grace_haight_ashbury_sf <- full_class_nhoods %>%
  filter(student == "Grace")
st_write(grace_haight_ashbury_sf, "nhood_haight_ashbury_sf.shp")

ricky_mission_sf <- full_class_nhoods %>%
  filter(student == "Ricky")
st_write(ricky_mission_sf, "nhood_mission_sf_ricky.shp")

niki_little_sai_gon_westminster <- full_class_nhoods %>%
  filter(student == "Niki")
st_write(niki_little_sai_gon_westminster, "nhood_little_sai_gon_westminster.shp")

claudia_blue_hills_kc <- full_class_nhoods %>%
  filter(student == "Claudia")
st_write(claudia_blue_hills_kc, "nhood_blue_hills_kc.shp")

josh_chinatown_oakland <- full_class_nhoods %>%
  filter(student == "Josh")
st_write(josh_chinatown_oakland, "nhood_chinatown_oakland_v2.shp")

betsayada_boyle_heights_la <- full_class_nhoods %>%
  filter(student == "Betsayada")
st_write(betsayada_boyle_heights_la, "nhood_boyle_heights_la.shp")

sahir_niles_fremont <- full_class_nhoods %>%
  filter(student == "Sahir")
st_write(sahir_niles_fremont, "nhood_niles_fremont.shp")

anushree_overfelt_sanjose <- full_class_nhoods %>%
  filter(student == "Anushree")
st_write(anushree_overfelt_sanjose, "nhood_overfelt_san_jose.shp")

becky_downtown_bakersfield <- full_class_nhoods %>%
  filter(student == "Becky")
st_write(becky_downtown_bakersfield, "nhood_downtown_bakersfield.shp")


## My neighborhood
my_census_tracts <- c("06075025500",
                      "06075026100",
                      "06075026301",
                      "06075026004",
                      "06075026001",
                      "06075026003",
                      "06075026002")
excelsior <- us_tracts %>%
  filter(GEOID %in% my_census_tracts) %>%
  mutate(nhood = "Excelsior") %>%
  group_by(nhood) %>%
  summarize(tracts = n())

# test the tracts shapefile
leaflet() %>%
  addTiles() %>%
  addPolygons(data = josh_chinatown_oakland %>%
                st_transform(., crs = 4326) 
  )

st_write(excelsior, "excelsior_sf.shp")
