# UNITED data from list of deaths
UNITED_df_original <- read_excel(path = "C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\united_data_update.xlsx")

UNITED_df <- UNITED_df_original %>% 
  dplyr::select(c(`Found dead`, Number, latitude, longitude, `Country of death`, `Place of death`, drowned))

UNITED_df$lon <- as.numeric(UNITED_df$longitude)
UNITED_df$lat <- as.numeric(UNITED_df$latitude)

#install.packages("leaflet")
library(leaflet, quietly = T, warn.conflicts = F)
# start basemap
map <- leaflet() %>% 
  
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  
  # add graticules from a NOAA webserver
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL,group = 'Graticules') %>%
  
  # focus map in a certain area / zoom level
  setView(lng = 33, lat = 10, zoom = 3) %>%
  
  # add layers control
  addLayersControl(overlayGroups = c('Place names',
                                     'Graticules',
                                     'Points',
                                     'Lines',
                                     'Polygons'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = 'topright') %>%
  
  # list groups to hide on startup
  hideGroup(c('Place names'))

# show map
map <- map %>%
  addCircleMarkers(data = UNITED_df, ~lon, ~lat,
                   weight = 0.5,
                   col = 'red', 
                   fillColor = 'darkred',
                   radius = 4, 
                   fillOpacity = 0.9, 
                   stroke = T, 
                   label = ~paste0('Point at: ', 
                                   as.character(round(lat,3)), ', ', 
                                   as.character(round(lon,3))), 
                   group = 'Points')


map

UNITED_df <- UNITED_df %>% 
  mutate(date_a = case_when(`Found dead` == "in Jan 21" ~ "01/01/2021",
                            `Found dead` == "in Jan 21" ~ "01/01/2021",
                            `Found dead` == "in Dec 20" ~ "01/12/2020",
                            `Found dead` == "in Dec 20" ~ "01/12/2020",
                            `Found dead` == "in Apr 20" ~ "01/04/2020",
                            `Found dead` == "in Jan 19" ~ "01/01/2019", 
                            `Found dead` == "in Nov 17" ~ "01/11/2017",  
                            `Found dead` == "in Nov 17" ~ "01/11/2017",  
                            `Found dead` == "in Oct 17" ~ "01/10/2017", 
                            `Found dead` == "in Aug 17" ~ "01/08/2017", 
                            `Found dead` == "in July 17" ~ "01/07/2017", 
                            `Found dead` == "in May 17" ~ "01/05/2017",  
                            `Found dead` == "in May 17" ~ "01/05/2017",  
                            `Found dead` == "in Dec 16" ~ "01/12/2016",  
                            `Found dead` == "in Nov 16" ~ "01/11/2016",  
                            `Found dead` == "in Oct 16" ~ "01/10/2016",  
                            `Found dead` == "in Oct 16" ~ "01/10/2016",
                            `Found dead` == "in Oct 16" ~ "01/10/2016",  
                            `Found dead` == "in July 16" ~ "01/07/2016", 
                            `Found dead` == "in May 16" ~ "01/05/2016", 
                            `Found dead` == "in Nov 15" ~ "01/11/2015",  
                            `Found dead` == "in Mar 15" ~ "01/03/2015",  
                            `Found dead` == "in Mar 15" ~ "01/03/2015", 
                            `Found dead` == "in Mar 15" ~ "01/03/2015",  
                            `Found dead` == "in Mar 15" ~ "01/03/2015",  
                            `Found dead` == "in Dec 14" ~ "01/12/2014", 
                            `Found dead` == "in Aug 14" ~ "01/10/2014",  
                            `Found dead` == "in June 12" ~ "01/06/2012", 
                            `Found dead` == "in Apr 11" ~ "01/04/2011",  
                            `Found dead` == "in Mar 11" ~ "01/03/2011"),
         date_a = dmy(date_a)) %>% 
  mutate(date_b = dmy(`Found dead`),
         date = case_when(is.na(date_b) ~ date_a,
                          !is.na(date_b) ~ date_b),
         deaths = Number,
         longitude = as.numeric(longitude),
         latitude = as.numeric(latitude)) %>% 
  filter(!is.na(drowned) & 
           (longitude > 8 & longitude < 22) &
           (latitude > 30 & latitude < 41)) %>% 
  dplyr::select(c(date, deaths)) %>% 
  filter(year(date) >= 2009)

UNITED_df_month <- dplyr::select(UNITED_df, c(date, deaths)) %>% 
  mutate(date_month = ym(paste0(year(date),"-",month(date)))) %>% 
  group_by(date_month) %>% 
  summarise(deaths_month = sum(deaths)) %>% 
  rename(date = date_month)

ggplot(data=UNITED_df_month, aes(x=date, y=deaths_month)) + geom_line() + theme_classic()
