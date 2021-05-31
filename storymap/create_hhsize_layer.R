#-------------------------------------------------------------------------------
# Multi-family housing
#
# AUTHOR: Francine Stephens
# DATE CREATED: 5/28/21
# LAST UPDATED: 5/30/21
#-------------------------------------------------------------------------------


# SET-UP------------------------------------------------------------------------
## LIBRARIES
packages <- c(
  "readr",
  "tidyverse",
  "sf",
  "ggplot2",
  "plotly",
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
setwd("~/Stanford/SOC176/soc176-data-processing/storymap")
wd <- getwd()
shp_repo <- "C:/Users/Franc/Documents/Shapefile_Repository/"
tracts10_path <- "2010USA_CensusGeog_Shp/nhgis0005_shapefile_tl2010_us_tract_2010/"
blockgrps_path <- "2010USA_CensusGeog_Shp/us_blck_grp_2010/"


## IMPORT DATA------------------------------------------------------------------
hh_cbg_data <- read_csv(paste0(wd, "/hh_size_cbg_SF_acs19.csv"))

hh_city_data <- read_csv(paste0(wd, "/SF_hh_size_acs19.csv"))

  
blck_grps <- st_read(paste0(shp_repo, 
                            blockgrps_path,
                            "US_blck_grp_2010.shp"),
                     quiet = F)

excelsior_tracts <- c(25500, 
                      26001,
                      26002, 
                      26003,
                      26004, 
                      26301,
                      26100)

household_size_str <- c("1 Person", 
                        "2 People",
                        "3 People",
                        "4 People",
                        "5 People",
                        "6 or More People")

fill <- c("#24B3A8",
          "#F5C71A",
          "#C8A2C8",
          "#B768A2",
          "#644117",
          "#FFB077"
)

## CLEAN DATA-------------------------------------------------------------------
hh_cbg_data_prepped <- hh_cbg_data %>%
  mutate(Geo_FIPS = as.character(Geo_FIPS),
         Geo_FIPS = str_pad(Geo_FIPS, width = 12, side = "left", pad = "0")
  ) %>%
  mutate(across(starts_with("hu_"), ~(.x/occ_hu)*100)
         )



hh_data_shp <- blck_grps %>% 
  right_join(., hh_cbg_data_prepped, by = c("GEOID10" = "Geo_FIPS")
             ) %>%
  select(TRACTCE10, GEOID10, occ_hu:avg_hh_size)


## Format CBG and City-level data for equity analysis
hh_city_data_prepped <- hh_city_data %>% 
  mutate(hu_size6up = hu_size6 + hu_size7up) %>% 
  select(-hu_size6:-hu_size7up) %>%
  pivot_longer(cols = starts_with("hu_size"),
               names_to = "household_size",
               values_to = "est") %>%
  mutate(household_size = as.factor(str_extract(household_size, "[0-9]+")), 
         geo = "San Francisco") %>%
  select(household_size:geo)


hh_excelsior_long_data <- hh_cbg_data %>%
  mutate(Geo_FIPS = as.character(Geo_FIPS),
         Geo_FIPS = str_pad(Geo_FIPS, width = 12, side = "left", pad = "0"), 
         hu_size6up = hu_size6 + hu_size7up) %>% 
  select(-hu_size6:-hu_size7up) %>%
  filter(Geo_TRACT %in% excelsior_tracts) %>% 
  pivot_longer(cols = starts_with("hu_size"),
               names_to = "household_size",
               values_to = "count") %>%
  mutate(household_size = as.factor(str_extract(household_size, "[0-9]+"))
         ) %>% 
  group_by(household_size) %>%
  summarize(est = sum(count)) %>% 
  mutate(geo = "Excelsior") 

## Create Stacked Bar Chart
hh_size_stackedbars <- hh_city_data_prepped %>%
  rbind(hh_excelsior_long_data) %>%
  mutate(household_size = plyr::mapvalues(household_size,
                                          from = c("1", "2", "3", "4", "5", "6"),
                                          to = household_size_str)
         ) %>% 
  group_by(geo) %>%
  mutate(pct = (est/sum(est)),
         percentage = round(pct, digits=2)*100) %>% 
  ungroup() %>% 
  arrange(desc(household_size)) %>%
  ggplot(aes(
      x = ordered(geo,
                  levels = c("San Francisco", "Excelsior")
                  ),
      y = pct,
      fill = household_size, 
      label = paste0(percentage, "%")
    )) +
  geom_bar(
    stat = "identity",
    position = position_fill(reverse = TRUE)
    ) +  
  geom_text(position = position_stack(vjust =0.5, reverse=TRUE),
            family = "Tahoma",
            color="white",
            size=2.5,
            fontface = "bold") +
  scale_fill_manual(values=fill) +
  scale_y_continuous(labels = label_percent(scale = 100, suffix = "%")) + 
  labs(
    x = "",
    y = "Housing Units",
    title = "Household Size in Excelsior Compared to San Francisco",
    subtitle = "Larger households are more prevalent in Excelsior than in the city as a whole.",
    caption = "Source: ACS-2019 5-year estimates"
  ) +
  coord_flip() + theme_bw() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) + 
  theme(axis.line = element_line(size=1, colour = "black"),
      panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
      panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10))



## EXPORTS----------------------------------------------------------------------
st_write(hh_data_shp, "SF_blckgrps_hh_size.shp")
ggsave("household_size_stacked_bargraph.png", hh_size_stackedbars)
