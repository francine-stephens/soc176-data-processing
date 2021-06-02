#-------------------------------------------------------------------------------
# CREATE 2020 RACIAL/ETHNIC, NATIVITY, AND LINGUISTIC GRAPHICS 
#
# AUTHOR: Francine Stephens
# DATE CREATED: 6/1/21
# LAST UPDATED: 6/1/21
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
  "scales",
  "treemapify"
)
lapply(packages, library, character.only = T)

## PATHS
setwd("~/Stanford/SOC176/soc176-data-processing/storymap")
wd <- getwd()


## IMPORT DATA------------------------------------------------------------------
race_data <- read_csv(paste0(wd,
                             "/san_francisco_2020_tracts_race.csv")
                      )

citizen_lang_data <- read_csv(paste0(wd,
                                     "/san_francisco_2020_tracts_citizen_language.csv")
                              )


excelsior_tracts <- c(25500, 
                      26001,
                      26002, 
                      26003,
                      26004, 
                      26301,
                      26100)

race_names_str <- c("Asian",
                    "Latinx",
                    "Non-Hispanic Black",
                    "Other or Multiple Races",
                    "Non-Hispanic White")

nativity_names_str <- c("Native-born",
                        "Foreign-born", 
                        "Foreign-born, Naturalized citizen",
                        "Foreign-born, Not a citizen")

lang_names_str <- c("Asian and Pacific Islander", 
                    "Indo-European",
                    "Other",
                    "Spanish"
                    )

race_fill <- c("#EB7200",
               "#F5CB11",
               "#C461EB", 
               "#00BDD7",
               "#688052"
               ) # Asian, Latinx, Black,  White, Other/Two more#00EB72

fborn_fill <- c("#B768A2", 
                "#C8A2C8",
                "#F5C71A"
                )

lang_fill <- c("#F5C71A",
               "#C8A2C8",
               "#24B3A8",
               "#FFB077"
)

## CLEAN DATA-------------------------------------------------------------------

# Race Excelsior v. Race in SF
excelsior_race_long <- race_data %>%
  select(GEOID10, POP:HISPANIC) %>%
  mutate(NHOTHER = POP - (NHWHITE + NHBLACK + NATIVE + ASIAN + HISPANIC),
         NHOTHERMULTI = (NHOTHER + NATIVE),
         TRACT = str_sub(GEOID10, start = -5)
         ) %>% 
  select(-NATIVE, -NHOTHER) %>%
  filter(TRACT %in% excelsior_tracts) %>%
  pivot_longer(cols = NHWHITE:NHOTHERMULTI,
               names_to = "RACE",
               values_to = "COUNT") %>%
  group_by(RACE) %>%
  summarize(TOTAL = sum(COUNT),
            TPOP = sum(POP)) %>%
  mutate(geo = "Excelsior") 


san_francisco_race_long <- race_data %>%
  select(GEOID10, POP:HISPANIC) %>%
  mutate(NHOTHER = POP - (NHWHITE + NHBLACK + NATIVE + ASIAN + HISPANIC),
         NHOTHERMULTI = (NHOTHER + NATIVE)
         ) %>% 
  select(-NATIVE, -NHOTHER) %>% 
  pivot_longer(cols = NHWHITE:NHOTHERMULTI,
               names_to = "RACE",
               values_to = "COUNT") %>%
  group_by(RACE) %>%
  summarize(TOTAL = sum(COUNT),
            TPOP = sum(POP)) %>% 
  mutate(geo = "San Francisco") 


## Foreign born 
excelsior_foreignborn_long <- citizen_lang_data %>% 
  mutate(Geo_TRACT = as.character(Geo_TRACT)) %>%
  filter(Geo_TRACT %in% excelsior_tracts) %>% 
  select(TPOP:NOT_CITIZEN) %>% 
  mutate(geo = "Excelsior") %>% 
  group_by(geo) %>%
  summarize(across(.cols = everything(), ~sum(.x))) %>%
  pivot_longer(cols = NATIVE:NOT_CITIZEN,
               names_to = "NATIVITY",
               values_to = "count") %>%
  group_by(NATIVITY) %>%
  mutate(prop = (count/TPOP),
         percentage = round(prop, digits=2)*100,
         NATIVITY = plyr::mapvalues(NATIVITY, 
                                    from = c("NATIVE", "FOREIGN", "NATURALIZED", "NOT_CITIZEN"),
                                    to = nativity_names_str)
         ) %>% 
  ungroup() %>% 
  filter(NATIVITY != "Foreign-born") %>%
  arrange(desc(prop))


## Language
excelsior_language_long <- citizen_lang_data %>% 
  mutate(Geo_TRACT = as.character(Geo_TRACT)) %>%
  filter(Geo_TRACT %in% excelsior_tracts) %>% 
  select(THH:OTHER_NLEP) %>% 
  mutate(geo = "Excelsior") %>% 
  group_by(geo) %>%
  summarize(across(.cols = everything(), ~sum(.x))) %>% 
  select(!starts_with("T")) %>%
  pivot_longer(cols = ENGLISH_ONLY:OTHER_NLEP,
               names_to = "Language",
               values_to = "count"
               ) %>% 
  separate(Language, c("Lang", "Status")) %>% 
  filter(Lang != "ENGLISH") %>%
  mutate(HOUSEHOLDS = 11488,
         Lang = plyr::mapvalues(Lang,
                                from = c("API", "INDOEURO", "OTHER", "SPANISH"),
                                to = lang_names_str),
         Status = case_when(Status == "NLEP" ~ "English Proficient",
                            Status == "LEP" ~ "Limited English Proficiency"
                            )
         ) %>%
  group_by(Status) %>%
  mutate(prop = (count/sum(count)),
         percentage = round(prop, digits=2)*100) %>%
  ungroup() %>%
  arrange(desc(prop))


## BUILD & EXPORT GRAPHICS------------------------------------------------------

# Stacked bargraph of Race Excelsior v. Race in SF
race_stackedbars <- san_francisco_race_long %>%
  rbind(excelsior_race_long) %>%
  mutate(RACE_ETHNICITY = plyr::mapvalues(RACE,
                                          from = c("ASIAN", "HISPANIC", "NHBLACK", "NHOTHERMULTI", "NHWHITE"),
                                          to = race_names_str)
  ) %>% 
  group_by(geo) %>%
  mutate(prop = (TOTAL/TPOP),
         percentage = round(prop, digits=2)*100) %>% 
  ungroup() %>% 
  arrange(desc(prop)) %>%
  ggplot(aes(
    x = ordered(geo,
                levels = c("San Francisco", "Excelsior")
    ),
    y = prop,
    fill = ordered(RACE_ETHNICITY,
                   levels = c("Asian",
                              "Latinx",
                              "Non-Hispanic White",
                              "Non-Hispanic Black",
                              "Other or Multiple Races")),  
    label = paste0(percentage, "%")
  )) +
  geom_bar(
    stat = "identity",
    position = position_fill(reverse = TRUE)
  ) +  
  scale_fill_manual(values=race_fill) +
  scale_y_continuous(labels = label_percent(scale = 100, suffix = "%")) + 
  labs(
    x = "",
    y = "Population",
    title = "Racial/Ethnic Composition in Excelsior Compared to San Francisco",
    subtitle = "Excelsior is the most racially/ethnically diverse neighborhood in San Francisco.",
    caption = "Note: Other includes Non-Hispanic Native American and some other race."
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

ggsave("race_stacked_bargraph.png", race_stackedbars, width=7.5, height=7)


## Chart of Foreign Born
nativity_pie <- ggplot(excelsior_foreignborn_long,
  aes(
    x = "", 
    y = prop, 
    fill = NATIVITY
  )
) + 
  geom_bar(
    stat = "identity", 
    position = position_fill()
  ) +
  geom_text(
    aes(label = paste0(round(prop*100),"%")), 
    position = position_fill(vjust = 0.5),
    family = "Tahoma",
    color="black",
    fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  scale_fill_manual(values=fborn_fill) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = 'bottom',
    axis.text = element_blank(),
    axis.line = element_blank(), 
    plot.title = element_text(hjust=0.5),
    axis.ticks = element_blank()
    ) +
  theme(
    plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
    text=element_text(family="Tahoma")
  ) + 
  labs(
    fill = "Nativity Status",
    title = "Nativity Status in Excelsior",
    subtitle = "Half of the residents in Excelsior are foreign-born and the majority have citizenship.",
    caption = "Data Sources: ACS 2019 5-Year Estimates"
  ) 

ggsave("nativity_piechart.png", nativity_pie)

# Pie chart lang diversity
  ggplot(excelsior_language_long,
         aes(
           x = "", 
           y = prop, 
           fill = Lang
         )
  ) + 
  geom_bar(
    stat = "identity",
    position = position_fill()
  ) +  
  geom_col(size=.8) +
  geom_text(
    aes(label = paste0(round(prop*100),"%")), 
    position = position_fill(vjust = 0.5),
    family = "Tahoma",
    color="black",
    fontface = "bold"
  ) +
  facet_wrap(~ Status) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values=lang_fill) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = 'bottom',
    axis.text = element_blank(),
    axis.line = element_blank(), 
    plot.title = element_text(hjust=0.5),
    axis.ticks = element_blank()
  ) +
  theme(
    plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
    text=element_text(family="Tahoma")
  ) + 
  labs(
    fill = "Native Language",
    title = "Languages Spoken in Excelsior",
    #subtitle = "Half of the residents in Excelsior are foreign-born and the majority have citizenship.",
    caption = "Data Sources: ACS 2019 5-Year Estimates"
  ) 



