#---------------------------------------------------
# CRIME DATA PREP SCRIPT
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 4/13/21
# LAST UPDATED: 4/18/21
#---------------------------------------------------


# SET-UP -----------------------------------------------------------------------
setwd("~/Stanford/SOC176/A4")

#### PARAMETERS ####
socrata_token <- "EZnaKCxOX6zj0uyTkD340TWLh"
email <- "fis@stanford.edu"
pword <- "SFdata2017"
wgs <- 4326
`%notin%` <- Negate(`%in%`)


## LIBRARIES 
packages <- c(
  "readxl", 
  "tidyverse",
  "naniar",
  "sf",
  "sp",
  "leaflet",
  "censusapi",
  "lubridate",
  "chron",
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

oakland <- read.socrata("https://data.oaklandca.gov/resource/ym6k-rx7a.json",
                        app_token = socrata_token, 
                        email = email,
                        password  = pword
)

detroit <- st_read("https://opendata.arcgis.com/datasets/0825badfe6304620a998d162be0e135e_0.geojson")




## DOWNLOADED/HAND COLLECTED CSV OR SHAPEFILE ##
denver <- st_read(paste0(wd,
                         "/denver_crime_04132021/crime.shp")
                  )

pitt <- read_csv(paste0(wd,
                         "/pittsburgh_crime_04132021.csv")
)

palo_alto <- read_csv(paste0(wd, 
                             "/Jan_Mar_2021_Palo_Alto_Police_Department_report.csv")
)

westminster <- read_csv(paste0(wd, 
                               "/westminster_crime_04132021.csv")
)

chino <- read_csv(paste0(wd,
                   "/chino_crime_04132021.csv")
            )


# CLEAN UP OPEN DATA + SHAPEFILES & EXPORT -------------------------------------
## SF
sf_reduced <- sf %>%
  mutate_at(vars(latitude:longitude), as.numeric) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>% 
  mutate(incident_year = as.numeric(incident_year),
         incident_month = month(incident_datetime, label = TRUE, abbr = FALSE),
         incident_date2 = as.factor(incident_date),
         incident_time_12hr = format(strptime(incident_time, format='%H:%M'), '%I:%M %p'), 
         incident_hour = format(strptime(incident_time, format='%H'), '%I %p')
) %>%
  filter(incident_year >= 2020) %>%
  select(-incident_date:-incident_time, -point.latitude:-filed_online) %>% 
relocate(incident_datetime, 
         incident_date = "incident_date2",
         incident_year,
         incident_month,
         incident_day_of_week,
         incident_time_12hr, 
         incident_hour
         )
write_csv(sf_reduced, "SF_crime_geocoded.csv", na = "")


## LA
la_reduced <- la %>%
  filter(lat != 0, lon != 0) %>%
  mutate(year_occ = year(date_occ),
         month_occ = month(date_occ, label = TRUE, abbr = FALSE),
         day_of_week_occ = wday(date_occ, label = TRUE, abbr = FALSE),
         time_occ_str = as.character(time_occ),
         time_occ_str = str_pad(time_occ_str, 4, side = c("left"), pad = "0"),
         time_occ_t = as.POSIXct(time_occ_str, format="%H%M"),
         time_occ_t2 = format(time_occ_t, format='%H:%M'),
         time_occ_12hr = format(strptime(time_occ_t2, format='%H:%M'), '%I:%M %p'),
         time_occ_hour = format(strptime(time_occ_t2, format='%H'), '%I %p')
  ) %>%
  relocate(dr_no,
           date_rptd,
           date_occ,
           year_occ,
           month_occ,
           day_of_week_occ,
           time_occ,
           time_occ_str,
           time_occ_t,
           time_occ_t2, 
           time_occ_12hr,
           time_occ_hour
           ) %>%
  select(-time_occ:-time_occ_t2)
write_csv(la_reduced, "LA_crime_geocoded.csv", na = "")


## DENVER
denver_reduced <- denver %>%
  filter(GEO_LON < - 103 & GEO_LAT >38) %>%
  mutate(incident_year = year(FIRST_OCCU),
         incident_month = month(FIRST_OCCU, label = TRUE, abbr = FALSE),
         incident_day_of_week = wday(FIRST_OCCU, label = TRUE, abbr = FALSE)
         ) %>%
  relocate(incident_year, .after = FIRST_OCCU) %>%
  relocate(incident_month, .after = incident_year) %>%
  relocate(incident_day_of_week, .after= incident_month) %>%
  filter(incident_year >= 2020) %>%
  select(-GEO_X, -GEO_Y) %>%
  rename(
    INCIDENT_ID = "INCIDENT_I",
    OFFENSE_CODE = "OFFENSE_CO",
    OFFENSE_CODE_EXT = "OFFENSE__1",
    OFFENSE_TYPE = "OFFENSE_TY",
    OFFENSE_CAT_ID = "OFFENSE_CA",
    FIRST_OCCURENCE = "FIRST_OCCU",
    REPORTED_DATE = "REPORTED_D",
    INCIDENT_ADDRESS = "INCIDENT_A",
    LONGITUDE = "GEO_LON",
    LATITUDE = "GEO_LAT",
    DISTRICT_ID = "DISTRICT_I",
    PRECINCT_ID = "PRECINCT_I",
    NEIGHBORHOOD = "NEIGHBORHO"
  )
write_csv(denver_reduced, "Denver_crime_geocoded.csv", na = "")


## DETROIT 
detroit_reduced <- detroit %>%
  filter(year >= 2020) %>% 
  rename(incident_year = "year") %>%
  mutate(incident_month = month(incident_timestamp, label = TRUE, abbr = FALSE),
         incident_day_of_week = wday(incident_timestamp, label = TRUE, abbr = FALSE),
         incident_time_12hr = format(incident_timestamp, '%I:%M %p'),
         incident_hour = format(incident_timestamp, '%I %p')
         ) %>%  
  select(-incident_time:-hour_of_day) %>%
  relocate(incident_month, .after = incident_year) %>%
  relocate(incident_day_of_week, .after = incident_month) %>%
  relocate(incident_time_12hr, .after = incident_day_of_week) %>%
  relocate(incident_hour, .after = incident_time_12hr)
write_csv(detroit_reduced, "Detroit_crime_geocoded.csv", na = "")


## KC, MO
kc_reduced <- kc %>%
  mutate(coords = str_extract_all(location.coordinates, "\\([^()]+\\)"),
         coords = substring(coords, 2, nchar(coords)-1)
  ) %>% 
  separate(coords, c("longitude", "latitude"), sep = ", ") %>%
  mutate(latitude = as.numeric(latitude), 
         longitude = as.numeric(longitude)
         ) %>%
  filter(latitude != 0 & longitude != 0) %>%
  mutate(report_year = year(report_date),
         report_month = month(report_date, label = TRUE, abbr = FALSE),
         report_day_of_week = wday(report_date, label = TRUE, abbr = FALSE),
         report_time_12hr = format(report_date, '%I:%M:%S %p'),
         report_hour = format(report_date, '%I %p')
  ) %>%
  relocate(report_no,
           report_date,
           report_year,
           report_month,
           report_day_of_week,
           report_time_12hr,
           report_hour
           ) %>%
  select(-report_time, -location.type, -location.coordinates)
write_csv(kc_reduced, "Kansas_City_crime_geocoded.csv", na = "")


## OAKLAND
oakland_reduced <- oakland %>%
  mutate(incident_year = year(datetime),
         incident_month = month(datetime, label = TRUE, abbr = FALSE),
         incident_day_of_week = wday(datetime, label = TRUE, abbr = FALSE),
         incident_time_12hr = format(datetime, '%I:%M %p'),
         incident_hour = format(datetime, '%I %p')
  ) %>%
  relocate(crimetype, 
           datetime,
           incident_year, 
           incident_month,
           incident_day_of_week,
           incident_time_12hr,
           incident_hour
           ) 


# CLEAN UP DATE-TIMES IN CSVs---------------------------------------------------
## PITTSBURGH
pitt_reduced <- pitt %>%
  filter(X != 0 & Y != 0) %>%
  mutate(incident_year = year(INCIDENTTIME),
         incident_month = month(INCIDENTTIME, label = TRUE, abbr = FALSE),
         incident_day_of_week = wday(INCIDENTTIME, label = TRUE, abbr = FALSE),
         incident_time_12hr = format(INCIDENTTIME, '%I:%M %p'),
         incident_hour = format(INCIDENTTIME, '%I %p')
  ) %>%
  relocate(PK,
           CCR, 
           HIERARCHY, 
           INCIDENTTIME,
           incident_year, 
           incident_month,
           incident_day_of_week,
           incident_time_12hr,
           incident_hour
  ) 
write_csv(pitt_reduced, "Pittsburgh_crime_geocoded.csv", na = "")


## PALO ALTO
palo_alto_date <- palo_alto %>%
  mutate(incident_date = as.Date(str_sub(date, end = 10), format = "%m/%d/%Y"),
         incident_month = month(incident_date, label = TRUE, abbr = FALSE),
         incident_day_of_week = wday(incident_date, label = TRUE, abbr = FALSE),
         incident_time = str_sub(date, start = 13),
         incident_hour = str_sub(incident_time, end = 1), 
         incident_ampm = str_sub(incident_time, start = -2),
         incident_hour = str_c(incident_hour, incident_ampm, sep = " ")
  ) %>%
  relocate(
    ccn, 
    date, 
    incident_date, 
    incident_month,
    incident_day_of_week,
    incident_time,
    incident_hour, 
    incident_ampm
  )


## WESTMINSTER
westminster_date <- westminster %>%
  mutate(incident_date = as.Date(date_time, format = "%m/%d/%Y", tz = "UTC"),
         incident_month = month(incident_date, label = TRUE, abbr = FALSE),
         incident_day_of_week = wday(incident_date, label = TRUE, abbr = FALSE)
  ) %>%
  separate(date_time, c("date", "time_24hr_clock"), sep = " ") %>% 
  mutate(
    time_24hr_num = str_remove(time_24hr_clock, ":"),
    time_24hr_num = as.numeric(time_24hr_num),
    time_ampm = if_else(time_24hr_num < 1159,
                        "AM",
                        "PM")
         ) %>%  
  select(-time_24hr_num) %>%
  relocate(
    type,
    blurred_street,
    date,
    time_24hr_clock,
    incident_date,
    incident_month,
    incident_day_of_week,
    time_ampm
  )
         

# PROCESS ADDRESSES -----------------------------------------------------------
## OAKLAND
oakland_address <- oakland_reduced %>%
  mutate(blurred_address = str_replace(address, "00 ", "50 ")
         )
oakland_intersections <- oakland_address %>% 
  filter(str_detect(blurred_address, " & "))
write_csv(oakland_intersections, "oakland_crime_intersections.csv", na = "")
oakland_blurred <- oakland_address %>%
  filter(blurred_address %notin% oakland_intersections$blurred_address)
write_csv(oakland_blurred, "oakland_crime_blurred.csv", na = "")


## PALO ALTO
palo_alto_address <- palo_alto_date %>%
  mutate(blurred_address = str_replace(blocksizedAddress, "00 Block ", "50 "),
         blurred_address = str_replace(blurred_address, "1 Block ", "15 "),
         blurred_address = str_replace(blurred_address, "\\**", "")
  ) %>%
  filter(blurred_address != ".",
         blurred_address != "UNKNOWN")
  

## WESTMINSTER
westminster_address <- westminster_date %>% 
  mutate(blurred_address = str_replace(blurred_street, "00 BLOCK", "50"),
         city = "Westminster",
         state = "CA",
         postalcode = 92683
         )

  
## CHINO
chino_address <- chino %>%
  mutate(
         street_address = str_replace(Location, "00 BLOCK", "50"),
         street_address = str_replace(street_address, "/", " & "),
         city = "Chino",
         state = "CA"
         ) 


# GEOCODE & EXPORT--------------------------------------------------------------
## PALO ALTO
palo_alto_geocoded <- palo_alto_address %>%
  geocode(street = blurred_address,
          city = city,
          state = state,
          postalcode = postalCode,
          method = 'census',
          lat = lat,
          long = long)
palo_alto_export <- palo_alto_geocoded %>%
  arrange(lat, blurred_address)
write_csv(palo_alto_export, "palo_alto_crime_geocoded2.csv", na = "")


## WESTMINSTER
westminster_geocoded <- westminster_address %>%
  geocode(street = blurred_address,
          city = city,
          state = state,
          postalcode = postalcode,
          method = 'census',
          lat = lat,
          long = long)
westminster_export <- westminster_geocoded %>% 
  arrange(lat, blurred_address)
write_csv(westminster_export, "westminster_crime_geocoded.csv", na = "")


## CHINO
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
sf <- westminster_export %>%
  filter(!is.na(lat)) %>%
  st_as_sf(coords=c("long", "lat"), crs=wgs)


leaflet() %>% 
  addTiles() %>%
  addCircleMarkers(
    data = sf,
    radius = 1,
    label = ~blurred_address
  )


