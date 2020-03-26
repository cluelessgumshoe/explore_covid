#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse,stringr,ggplot2,lsr)

#population data from github - fresh up to 2016
pop <- read_csv("https://raw.githubusercontent.com/datasets/population/master/data/population.csv")

#function to  combine all the files
source("files_combine_fun.R")

covid <- combine(directory)

covid %>% DataExplorer::plot_missing()

#need to rework this cleaning section, was using to coronavirus packages, but the data there is not being updated
covid <- covid %>% 
  mutate(Province.State = ifelse(Province.State == "","Unknown",Province.State),
         Province.State = as_factor(Province.State),
         Country.Region = as_factor(Country.Region),
         type = as_factor(type)) %>%
  mutate_all(stringr::str_trim) %>%
  group_by(Country.Region,Province.State,Lat,Long,date,type) %>%
  arrange(date) %>%
  spread(type,cases) %>%
  mutate(#confirmed = zoo::na.locf(confirmed, fromLast = TRUE),
         #recovered = zoo::na.locf(recovered, fromLast = TRUE),
         confirmed = replace_na(confirmed,0),
         recovered = replace_na(recovered,0),
         death = replace_na(death,0)) %>%
  mutate(confirmed = abs(as.numeric(confirmed)),
         recovered = abs(as.numeric(recovered)),
         death = abs(as.numeric(death))) %>%
  ungroup() %>%
  select(date,recovered,confirmed,death)

covid %>% GGally::ggcorr()


covid <- coronavirus::coronavirus %>% 
  mutate(Province.State = ifelse(Province.State == "","Unknown",Province.State),
         Province.State = as_factor(Province.State),
         Country.Region = as_factor(Country.Region),
         type = as_factor(type),
         data = lubridate::as_date(date),
         date_day = str_extract(date,"[:digit:][:digit:]$"),
         date_mon = format(as.Date(.$date,format="%Y-%m-%d"), format = "%b")
         ) %>%
  mutate_all(stringr::str_trim) %>%
  arrange(lubridate::as_date(date)) %>%
  mutate(date_abb = paste0(date_mon,"-",date_day)) %>%
           group_by(date,date_abb,type) %>%
  summarise(cases = sum(as.numeric(cases))) %>%
  ungroup() %>%
  mutate(date = as.factor(date))
  

ggplot(covid, aes(x = date, y = cases)) + 
  geom_point(stat="identity",aes(color = type), size = 2) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FC4E07")) +
  ggtitle("COVID-19") +
  theme(axis.text.x = element_text(angle = 90),
        )

