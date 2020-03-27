#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse,stringr,ggplot2,lsr,DataExplorer,lubridate)

#population data from github - fresh up to 2016
pop <- read_csv("https://raw.githubusercontent.com/datasets/population/master/data/population.csv")
pop %<>% group_by(`Country Name`) %>% filter(Year == max(Year)) %>% ungroup()
#function to  combine all the files
source("explore_covid/files_combine_fun.R")

covid <- combine(directory)

#################################################################################################
#CLEAN-UP

covid_df <- covid %>% 
  mutate(province_state = ifelse(is.na(`Province/State`),Province_State,`Province/State`),
         province_state = as_factor(province_state),
         country_region = ifelse(is.na(`Country/Region`),Country_Region,`Country/Region`),
         country_region = as_factor(country_region),
         lat = ifelse(is.na(Lat),Latitude,Lat),
         lon = ifelse(is.na(Long_),Longitude,Long_),
         `Last Update` = substr(`Last Update`,1,10),
         `Last Update2`= `Last Update`,
         `Last Update` = as.Date(`Last Update`,'%m/%d/%y'),
         `Last Update2`=  as.Date(`Last Update2`,'%Y-%m-%d'),
         `Last Update` = ifelse(is.na(`Last Update`),`Last Update2`,`Last Update`),
         Last_Update2 = Last_Update,
         Last_Update = as.Date(Last_Update,'%m/%d/%y'),
         Last_Update2 = as.Date(Last_Update2,'%Y-%m-%d'),
         Last_Update = ifelse(is.na(Last_Update),Last_Update2,Last_Update),
         date = ifelse(is.na(Last_Update),`Last Update`,Last_Update),
         date = as_date(date)
         ) %>%
  select(-`Province/State`,-Province_State,-`Country/Region`,-Country_Region,
         -Lat,-Latitude,-Long_,-Longitude,-`Last Update`,-Last_Update,-Last_Update2,-`Last Update2`,
         -Admin2,-FIPS,-Combined_Key) %>%
  mutate_all(stringr::str_trim) %>%
  rename(confirmed = "Confirmed",
         recovered = "Recovered",
         deaths = "Deaths",
         active = "Active") %>% 
  mutate_at(vars("confirmed","recovered","deaths","active","lat","lon"),.funs = as.numeric) %>% 
  mutate(#confirmed = zoo::na.locf(confirmed, fromLast = TRUE),
         #recovered = zoo::na.locf(recovered, fromLast = TRUE),
         confirmed = replace_na(confirmed,0),
         recovered = replace_na(recovered,0),
         deaths = replace_na(deaths,0),
         active = replace_na(active,0)) %>%
  mutate(confirmed = abs(as.numeric(confirmed)),
         recovered = abs(as.numeric(recovered)),
         deaths = abs(as.numeric(deaths))) %>%
         #date_day = str_extract(date,"[:digit:][:digit:]$"),
         #date_mon = format(as.Date(.$date,format="%Y-%m-%d"), format = "%b")) %>%
    arrange(lubridate::as_date(date)) %>%
  ##city clean-up for lat and lon
  mutate(province_state = case_when(province_state == "Chicago" ~ "Chicago, IL",
                                    province_state == "Bavaria" ~ NA_character_,
                                    province_state == "None" ~ NA_character_,
                                    province_state == "From Diamond Princess" & country_region == "Israel" ~ NA_character_,
                                    (is.na(province_state) | province_state == "Jervis Bay Territory" | province_state == "External territories")& country_region == "Australia" ~ "Australian Capital Territory",
                                    province_state == "Ashland, NE" ~ "New England",
                                    province_state == "Lackland, TX" ~ "Texas",
                                    province_state == "Travis, CA" | province_state == "Cruise Ship" ~ "California",
                                    TRUE ~ province_state),
         country_region = case_when(str_detect(country_region,"Ireland") ~ "Ireland",
                                    #fix cruise ships
                                    province_state == "California" & country_region == "Others" ~ "US",
                                    TRUE ~ country_region),
         lat = case_when(country_region == "Ivory Coast" ~ 7.5400,
                         province_state == "New England" ~ 43.9654,
                         TRUE ~ lat),
         lon = case_when(country_region == "Ivory Coast" ~ 5.5471,
                         province_state == "New England" ~ 70.8227,
                         TRUE ~ lon)) 
#fix the blank lat and lon
lat_lon <- covid_df %>%
  select(province_state,country_region,lat,lon) %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  rename(lat2 = "lat",
         lon2 = "lon") %>%
  distinct()

covid_df %<>% 
  plyr::join(lat_lon,type = "left", by = c("province_state","country_region"),match = "first") %>%
  mutate(lat = ifelse(is.na(lat),lat2,lat),
         lon = ifelse(is.na(lon),lon2,lon)) %>%
  select(-lat2,-lon2)

rm(covid)
##########################################################################################
#
# DATA EXPLORATION
#
covid_df %>% GGally::ggcorr()
covid_df %>% DataExplorer::plot_missing()
# 
# ggplot(covid_df, aes(x = date, y = cases)) + 
#   geom_point(stat="identity",aes(color = type), size = 2) +
#   scale_color_manual(values = c("#00AFBB", "#E7B800","#FC4E07")) +
#   ggtitle("COVID-19") +
#   theme(axis.text.x = element_text(angle = 90),
#         )

########################################################################################
#
# Add in some popuation data
#fix annoying names
names(pop) = str_replace_all(names(pop)," ","_")
names(pop) = str_to_lower(names(pop))
#find out where the countries need match, ~100 do not match
pop_names <- pop %>% #these  don't match
  select(country_name) %>%
  distinct() %>%
  anti_join(covid_df, by = c("country_name" = "country_region"))

#what does match?
pop_matches <- pop %>%
  select(country_name) %>%
  distinct() %>%
  anti_join(pop_names, by = "country_name")

 #these don't match
covid_names <-  covid_df %>% select(country_region) %>% distinct() %>% arrange(country_region)