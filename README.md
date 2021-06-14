## Data Cleaning & Preparation for the Social Life of Neighborhoods Course

### Course and Repo Overview

The Social Life of Neighborhoods was a course offered through Stanford's Sociology Department in the spring of 2021. The underlying goal of the course is to mount a quarter long intellectual investigation of a neighborhood and the various processes that structure its “social life”— that is, the way it developed historically, reproduces itself on a daily basis, and is subjectively experienced by various individuals and groups. View the [course syllabus](https://drive.google.com/file/d/1Osjtn8YP1PE_DuZhy1rr3vSampJ_Pv3t/view) to see a complete description of the course and all of the assignments. This course also had a set of tutorials for students to walk through the data handling and mapping steps required to complete their assignments. These tutorials reference the data created with the scripts stored in this repo. Navigate to the following links to view the [tutorial website](https://bookdown.org/fis/social-life-of-neighborhoods/) and the associated [repo](https://github.com/francine-stephens/social-life-of-neighborhoods) with the code for the tutorial website. 

This repository contains the scripts used to process and prepare data for the course. The scripts read in tabular and spatial data for the neighborhoods (i.e. census tracts) and their encompassing geographies (e.g., cities and metro areas) and subsequently clean and transform them. These scripts output tabular data, shapefiles, and graphs. *Most of these outputted files were shared with students over ArcGIS Online and were not checked into this repo due to file size constraints.*


### Organization of Repo

The repo is organized into subdirectories that represent the assignments. The scripts and data for the assignment are stored in the respective assignment subdirectory. In the list below, the content and function of each assignment subdirectory are listed out. 

**A3** 

* Assignment: Key Non-Profit Organizations in the neighborhood
* Data: The neighborhoods CSV indicates the census tracts that define students' neighborhoods, which is important for defining the filtering and grouping in the script. CSV file with addresses of key non-profit organizations. These data were hand-collected and uploaded to ArcGIS Online for geocoding. 
* Code: Creates neighborhood boundaries by calling on the U.S. Census Tract shapefile and filtering to the set of tracts that students listed in their first assignment to define their neighborhood. 
  + n.b. The census tract shapefile called on in the script is located in the [shapefile repository](https://github.com/francine-stephens/Shapefile_Repository).

**A4**

* Assignment: Crime & Public Order in the neighborhood
* Data: CSV and Excel files of geocoded crime incident locations for students' selected neighborhood's encompassing cities. The eyes on the street CSV includes addresses for locations that represented eyes on the street and were uploaded to ArcGIS Online for geocoding.
* Code: Geocodes the crime data stored in the CSV/Excel files for each city represented in the class and outputs a shapefile of the crime incidents. Some tabular attributes like date-time and type of crime are cleaned as well.

**A5**

* Assignment: Racial/Ethnic Composition and Segregation from 1970 to 2020
* Code: Both scripts - for the city and region/place levels - calls on the census tracts shapefile as well as racial/ethnic composition data from the [Brown University Longitudinal Tract Database](https://s4.ad.brown.edu/projects/diversity/researcher/LTDB.htm). The cities script outputs two shapefiles (1) the racial composition for all tracts in the city stacked by decade and (2) the divergence index measures of segregation for each census tract in the city stacked by decade. The cities script also outputs tabular data of the dissimilarity index of segregation for each city and each racial group pairing stacked by decade. The region/place script outputs shapefiles for (1) racial composition, (2) divergence measure of segregation, and the (3) dissimilarity index of segregation for each *place in the region* stacked by decade (and race-group pair for the dissimilarity index). 
  + n.b. The census tract shapefile called on in the script is located in the [shapefile repository](https://github.com/francine-stephens/Shapefile_Repository).

**A6**

* Assignment: Gentrification
* Data: Imported data are Census and ACS data from 1990 to 2020 with variables for median household income, educational attainment, and median rent and home values. The exported CSVs contain key gentrification indicator values at the city level, which are based on the gentrification measures calculated in the script. The CSVs with the key gentrification and condo locations include addresses that were uploaded into ArcGIS Online for geocoding. 
* Code: The R scripts create gentrification measures for all census tracts or census block groups (see name in script) within the city of the student's chosen neighborhoods from 1990 to 2020 using the imported Census and ACS data. These measures were exported into a series of shapefiles by decade.  

**storymap**

* Final project: Storymap of neighborhood's social life 
* Data: CSVs contain miscellaneous census and geographic data that were used for additional slides in the story map, not for particular assignments. 
* Code: The code outputs the shapefiles, tabular data, and graphs for miscellaneous slides (e.g., HOLC grading, parcel data zoning, etc.) in the story map. Review the description in the script file for more specific information.

### Example Visuals

* View the [map](https://arcg.is/1PiD1D) of key non-profits in the neighborhood (based on A3 data).
* View the [map](https://arcg.is/1K0reX) of crime incidents in the neighborhood (based on A4 data). 
* View the [map](https://arcg.is/mmm8H) of the majority racial/ethnic group in the neighborhood (based on A5 data). 
* View the [map](https://arcg.is/4LGSD) of racial segregation in neighborhood (based on A5 data). 
* View the [map](https://arcg.is/0TPjTG) of gentrification in the neighborhood (based on A6 data).  
* View the [storymap](https://arcg.is/1Xj0v4).

![race_stacked_bargraph](https://user-images.githubusercontent.com/47190395/121839474-c14f5400-cc9f-11eb-846f-110bfee1f402.png)

![household_size_stacked_bargraph](https://user-images.githubusercontent.com/47190395/120125100-e52d7880-c17c-11eb-9068-961917b93c9b.png)

![excelsior_job_sector_lines](https://user-images.githubusercontent.com/47190395/121839455-b5fc2880-cc9f-11eb-907c-e606ebec9c30.png)
