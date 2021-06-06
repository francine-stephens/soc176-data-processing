#------------------------------------------------------
# CREATE DRIVING & WORK ISOCHRONES
# 
# AUTHOR: Francine Stephens
# DATE CREATED: 6/4/21
# LAST UPDATED: 6/5/21
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
  "mapboxapi",
  "gghighlight"
)
lapply(packages, library, character.only = T)

## PATHS
census_api_key("99ccb52a629609683f17f804ca875115e3f0804c")
mb_access_token("sk.eyJ1IjoiZnJhbmNpbmVzdGVwaGVucyIsImEiOiJja2ljb3VrczMwdDdhMnhsNjA4Yjh1c2h1In0.WJjq6TysT6zZZnaxsN0s5g",
                overwrite = TRUE)
readRenviron("~/.Renviron")
setwd("~/Stanford/SOC176/soc176-data-processing/storymap")
wd <- getwd()
wgs <- 4326


## IMPORT DATA------------------------------------------------------------------
excelsior_tracts <- c("025500", 
                      "026001",
                      "026002", 
                      "026003",
                      "026004", 
                      "026301",
                      "026100")

excelsior_tracts_sf <- tracts(state = "CA", 
                    county = "San Francisco",
                    cb = T,
                    progress_bar = F) %>% 
  filter(TRACTCE %in% excelsior_tracts) %>%
  st_transform(26910) %>% 
  mutate(original_area = st_area(.)
         )

ca_tracts <- tracts("CA", cb = T, progress_bar = F)

ca_od <- grab_lodes(
  state = "ca", 
  year = 2018, 
  lodes_type = "od", 
  job_type = "JT01",
  state_part = "main", 
  agg_geo = "tract"
)  

excelsior_rac_2010to2018 <- 
  2010:2018 %>% 
  map_dfr(function(year){
  grab_lodes(
  state = "ca", 
  year = year,
  lodes_type = "rac", 
  job_type = "JT01",
  state_part = "main", 
  agg_geo = "tract"
  ) %>%
  filter(h_tract %in% excelsior_tracts_sf$GEOID)  
  })

job_sectors <- c("Agriculture",
                "Mining",
                "Utilities",
                "Construction",
                "Manufacturing",
                "Wholesale Trade",
                "Retail Trade",
                "Transportation",
                "Information",
                "Finance & Insurance",
                "Real Estate",
                "Professional, Scientific, & Technical",
                "Management",
                "Waste Management",
                "Education",
                "Health Care",
                "Arts & Recreation",
                "Food & Hospitality",
                "Other",
                "Public Administration"
                )

job_sector_fill <- c(
                    "#EB7200",
                    "#F5CB11",
                    "#C461EB", 
                    "#00BDD7",
                    "#688052"
                    )


## CREATE ISOCHRONES FOR EXCELSIOR----------------------------------------------
excelsior_centroid <- excelsior_tracts_sf %>% 
  mutate(nhood = "Excelsior") %>%
  group_by(nhood) %>%
  summarize(n_tracts = n()) %>%
  st_centroid()

# 32.8 min is avg commute time to work for san francisco.
drive_33min_excelsior <- mb_isochrone(
  excelsior_centroid,
  profile = "driving-traffic",
  time = 33
)

drive_33min_excelsior_identifiers <- excelsior_centroid %>% 
  st_set_geometry(NULL) %>% 
  cbind(drive_33min_excelsior$geometry) %>% 
  st_as_sf()

leaflet() %>% 
  addMapboxTiles(
    style_id = "streets-v11",
    username = "mapbox"
  ) %>%
  addPolygons(
    data = drive_33min_excelsior_identifiers,
    label = ~nhood
  )

st_write(drive_33min_excelsior_identifiers,
         "excelsior_33min_drive_isochrone.shp")


## WHERE DO EXCELSIOR RESIDENTS WORK--------------------------------------------
excelsior_od <- ca_od %>% 
  filter(h_tract %in% excelsior_tracts_sf$GEOID) %>% 
  left_join(
    ca_tracts, 
    by = c("w_tract" = "GEOID")
  ) %>% 
  st_as_sf() %>% 
  filter(S000 >= 5, 
         COUNTYFP != "037",
         COUNTYFP != "059",
         COUNTYFP != "073")
            ## total job count

lodes_pal <- colorNumeric(
  palette = "YlGnBu",
  domain = excelsior_od$S000
)

leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    data = excelsior_od %>% st_transform(wgs),
    fillColor = ~lodes_pal(S000),
    label = ~paste0(w_tract, ": ", S000),
    color = "black",
    weight = 0.5,
    fillOpacity = 0.25
  ) %>% 
  addLegend(
    data = excelsior_od,
    pal = lodes_pal,
    values = ~S000,
    title = "Where Excelsior residents<br>work, LODES 2018<br>(5 or more workers)"
  )

excelsior_od_shp_export <- excelsior_od %>%
  select(year, w_tract:SI03)

st_write(excelsior_od_shp_export,
         "where_excelsior_residents_work.shp")


## ORIGIN-DESTINATION ROUTES FOR EXCELSIOR RESIDENTS----------------------------

## TROUBLE SHOOT WHY THIS IS NOT WORKING?!?!
excelsior_od_origin <- excelsior_od %>% 
  st_set_geometry(NULL) %>% 
  select(h_tract) %>% 
  left_join(ca_tracts %>% select(h_tract = GEOID)) %>% 
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates()

excelsior_od_destination <- excelsior_od %>% 
  st_centroid() %>% 
  st_coordinates()


excelsior_od_route <- 1:nrow(excelsior_od_origin) %>%
  map_dfr(function(x){
    mb_directions(
      origin = excelsior_od_origin[x, ],
      destination = excelsior_od_destination[x, ],
      profile = "driving"
    )
  })

excelsior_od_route %>% 
  st_as_sf() %>% 
  leaflet() %>%
  addMapboxTiles(
    style_id = "light-v9",
    username = "mapbox"
  ) %>%
  addPolylines()


## JOB CHARACTERISTICS OF RESIDENTIAL AREA--------------------------------------
excelsior_rac_sectors_year_long <- excelsior_rac_2010to2018 %>% 
  select(year, C000, CNS01:CNS20) %>% 
  mutate(year = as.character(year)) %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), ~ sum(., is.na(.), 0))) %>%
  pivot_longer(cols = starts_with("CNS"),
               names_to = "job_sector",
               values_to = "count"
                ) %>%
  mutate(prop = (count/C000),
         percentage = round(prop, digits=2)*100,
         sector = plyr::mapvalues(job_sector, 
                                  from = c("CNS01",
                                           "CNS02",
                                           "CNS03",
                                           "CNS04",
                                           "CNS05", 
                                           "CNS06",
                                           "CNS07",
                                           "CNS08",
                                           "CNS09",
                                           "CNS10",
                                           "CNS11",
                                           "CNS12",
                                           "CNS13", 
                                           "CNS14",
                                           "CNS15",
                                           "CNS16",
                                           "CNS17",
                                           "CNS18",
                                           "CNS19",
                                           "CNS20"),
                                  to = job_sectors),
         year = as.numeric(year)) %>% 
  ungroup() %>%
  group_by(sector) %>% 
  mutate(change = count - count[year==2010],
         pct_change = (change/count[year==2010]) * 100,
         high_growth = if_else(sector == "Health Care" |
                                 sector == "Professional, Scientific, & Technical" | 
                                 sector == "Information" |
                                 sector == "Construction",
           "High Growth",
           "Not High Growth")
         ) %>% 
  ungroup() %>%
  arrange(sector, year)

## line graph
excelsior_job_sector_line <- 
  ggplot(excelsior_rac_sectors_year_long,
         aes(
           x = year %>% as.character(),
           y = count,
           group = sector %>% factor(),
           color = sector %>% factor())
  ) +
    geom_line(size = 1.5) + 
    #geom_point(size = 3) +
    scale_color_manual(values=job_sector_fill) + 
    labs(
      title = "High Growth Job Sectors among Excelsior Residents",
      subtitle = "The highlighted sectors grew by at least 50% between 2010 and 2018.",
      color = "Job Sector",
      y = "Residents",
      x = "Year",
      caption = "Data Source: LODES-2018"
    ) + 
    scale_y_continuous(labels = scales::comma) +
    theme_bw(base_size = 10) + 
    theme( 
      legend.position="bottom", legend.direction="horizontal") + 
    theme(axis.line = element_line(size=1, colour = "black"),
          panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank()) +
    theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
          text=element_text(family="Tahoma"),
          axis.text.x=element_text(colour="black", size = 10),
          axis.text.y=element_text(colour="black", size = 10),
          legend.key=element_rect(fill="white", colour="white")) + 
    gghighlight(high_growth == "High Growth", label_key = sector)
  
ggsave("excelsior_job_sector_lines.png", excelsior_job_sector_line)

