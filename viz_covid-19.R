

# - DRAFTS ONLY - need tweaked for each use - (e.g. gathered above vs of below etc. )
########################################################################################################
# Get some ideas for visuals
########################################################################################################

#######################################################################################################
# case type vs date

fun_bar <- function(df,x_var,y_var,color_var,color_varName,stacked=T,horizontal=F,...) {
  
  library(magrittr)
  plot_ly(
    x = ~x_var,
    y = ~y_var,
    color = ~as.character(color_var),
    colors = colorRamp(list("blue2","green3","red"))
    
  ) %>%
    add_bars() %>%
    layout(
      barmode = ifelse(stacked,'stack','group'),
      orientation = ifelse(horizontal,'h','v'),    
      title = paste0("COVID-19 Cases by ",color_varName," (Cumulative)"),
      xaxis = list(title = "Date", tickangle = 45),
      yaxis = list(title = "Cases")
    )
}

covid_v1 <- covid_df %>% 
  gather(case_type,n_cases,new_confirmed:new_recovered) %>%
  mutate(n_cases = as.numeric(n_cases)) %>%
  group_by(date,case_type) %>%
  #filter(country_region == "US") %>%
  summarise(n_cases = sum(n_cases)) %>%
  distinct()


fun_bar(covid_v1,covid_v1$date,covid_v1$n_cases,covid_v1$case_type,"Date",stacked = F)



#######################################################################################################
# perc pop  - latest date

fun_bar3 <- function(df,x_var,y_var,color_var,color_varName,stacked=T,horizontal=F,...) {
  
  library(magrittr)
  plot_ly(
    x = ~x_var,
    y = ~y_var,
    color = ~as.character(color_var),
    colors = c("blue2","red","green2","yellow")
  ) %>%
    add_bars() %>%
    layout(
      barmode = ifelse(stacked,'stack','group'),
      orientation = ifelse(horizontal,'h','v'),    
      title = paste0("COVID-19 by ",color_varName),
      xaxis = list(title = "", tickangle = 45),
      yaxis = list(title = "Percent")
    )
}

covid_pop_v2 <- covid_pop_df %>% group_by(country_region,day_virus) %>%  gather(case_type,perc_cases,perc_ctry_cnfrm:perc_pop_infect) %>% 
  select(country_region, day_virus,perc_cases,case_type) %>% 
  filter(country_region %in% c("US","Italy","China","Germany")) %>% 
  mutate(perc_cases = as.numeric(perc_cases)) %>%
  group_by(country_region) %>%
  filter(day_virus == max(day_virus)) %>%
  distinct()

fun_bar3(covid_pop_v2,covid_pop_v2$country_region,covid_pop_v2$perc_cases,covid_pop_v2$case_type,"% of Country Population (2018)",stacked = F)


#######################################################################################################
# MAPS MAPS MAPS
#######################################################################################################

#######################################################################################################
# map of confirmed status vs time - this could be a neat viz to have slider bar for in shiny
map_pop <- covid_pop_df %>% select(country_region,province_state,perc_pop_infect,lat,lon) %>% distinct()
sf_covid_pop <- st_as_sf(map_pop, coords = c("lon", "lat"), crs = 4326)
#set the color palette
mapviewOptions(vector.palette =  viridis::inferno)
#map with sizes and locations colored by reason buckets
mapview(sf_covid_pop, legend = F, alpha = 0, burst = T, 
        width = "2500", align = "center", cex = "perc_pop_infect", 
        map.types = c("CartoDB.DarkMatter","CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","OpenTopoMap"))

#######################################################################################################
# What order did the virus hit other countries?
#ok this is scary too, could be cool to render by day
covid_routes <- covid_pop_df %>%
  filter(day_virus == 1) %>% #day 1 would be the first day the virus landed in a country
  select(country_region,day_virus,lat,lon) %>% 
  group_by(country_region) %>%
  mutate(lat = mean(lat),
         lon = mean(lon)) %>%
  distinct() %>% 
  ungroup() %>%
  mutate(from_lat = lat,
         to_lat = lead(from_lat,1),
         from_lon = lon,
         to_lon = lead(lon,1),
         #jitter slightly so that not identical for route map
         from_lat = jitter(from_lat, factor = 0.1),
         from_lon = jitter(from_lon, factor = 0.1),
         to_lat = jitter(to_lat, factor = 0.1),
         to_lon = jitter(to_lon, factor = 0.1))

#https://rstudio-pubs-static.s3.amazonaws.com/259095_2f8cb24b43284692a8af916bd447931d.html
map_setup <- borders("world", colour="grey80", fill="grey3")
virus_travel <- ggplot() + map_setup +
  geom_curve(data=covid_routes,
             aes(x=from_lon, y=from_lat, xend=to_lon, yend=to_lat),
             col="orangered4",
             size=.5,
             curvature=0.2) +
  geom_point(data=covid_routes,
             aes(x=from_lon, y=from_lat), 
             colour="orangered2",
             size=1.5) +
  geom_point(data=covid_routes,
             aes(x=to_lon, y=to_lat), 
             colour="orangered2") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        plot.title=element_text(hjust=0.5, size=12)) +
  ggtitle("COVID-19 Routes for First Appearances in Each Country")# +
#check the min/max of long and lat to get good zoom level if needed or leave out
#coord_cartesian(ylim=c(10, 75), xlim=c(-160, 70))
ggplotly(virus_travel)
virus_travel

########################################################################################################
# glm_us <- glm2::glm2(covid_pop_v1$day_virus ~ covid_pop_v1$n_cases)
# summary(glm_us)
# plot(glm_us)










