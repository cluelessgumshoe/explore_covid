#################
#function to combine files in a directory (i.e. onedrive)
# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse,readxl,magrittr,gtools,stringr)
#clone this repo to keep data updated; fetch to refresh: https://github.com/CSSEGISandData/COVID-19
directory <- paste0("C:/Users/",Sys.info()["login"],"/Documents/GitHub/covid-19/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/")
##personal computer version (github storage is not on C:/)
#directory <- paste0("D:/GitHub/covid-19/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports")
# Define function to read and combine ####
combine <- function(directory) {
  ## 'directory' is a char vector of len 1 indicating location of CSV files
  files <- list.files(directory,full.names = TRUE) # make list of full file names
  files %<>% as.data.frame()
  #filter out unwanted files
  files %<>% filter(!str_detect(files,"gitignore"), !str_detect(files,"README"))
  covid_df <- data.frame()
  i = 1
  for (i in 1:nrow(files)){
    x <- read_csv(paste0(files[i,]),col_types = cols(.default = "c"))
    covid_df <- bind_rows(covid_df,x)
    i = i + 1
  }
  return(covid_df)
}

  