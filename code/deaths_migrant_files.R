# Migrants files
migrantfiles <- read_excel(path="DISCONTINUED ON JUNE 24, 2016 - Events during which someone died trying to reach or stay in Europe.xlsx",
                           sheet = 2)

migrantfiles_df <- migrantfiles %>% 
  dplyr::select(date, dead_and_missing, `route (Frontex)`, latitude, longitude, source) %>% 
  mutate(date = ymd(substr(date, start=1, stop=10)),
         route = case_when(`route (Frontex)` %in% c("Central Mediterranean route", "Central Mediterranean Route") ~ "Central_Mediterranean",
                           `route (Frontex)` == "Eastern Mediterranean route" ~ "Eastern_Mediterranean",
                           `route (Frontex)` %in% c("Western Mediterranean route", "Western Mediterranean Route") ~ "Western_Mediterranean"),
         date_month = ym(paste0(year(date),"-",month(date))),
         project = "MigrantFiles") %>% 
  filter(!is.na(date)) %>% 
  filter(route %in% c("Central_Mediterranean", "Eastern_Mediterranean", "Western_Mediterranean")) %>% 
  dplyr::select(date, date_month, dead_and_missing, route,  latitude, longitude)

migrantfiles_df_month <- migrantfiles_df %>% 
  dplyr::group_by(date_month, route) %>% 
  summarise(dead_and_missing = sum(dead_and_missing, na.rm = T)) %>% 
  ungroup()

migrantfiles_df_month <- migrantfiles_df_month %>% 
  pivot_wider(id_cols=date_month,
              names_from = route,
              names_prefix = "dead_and_missing_",
              values_from = dead_and_missing)

#Fractionalization Index
migrantfiles_df <- migrantfiles_df %>% 
  mutate(death_event = ifelse(dead_and_missing == 1, "less_than_1_dead",
                              ifelse(dead_and_missing %in% c(2:10), "2_to_10_deads", 
                                     ifelse(dead_and_missing > 10, "more_than_10_deads", NA))))

frac_1 <- migrantfiles_df %>% 
  group_by(date_month, route, death_event) %>% 
  summarise(N = n())

frac_2 <- frac_1 %>% 
  group_by(date_month, route) %>% 
  summarise(frac_index = N/sum(N))

frac_1$frac_index <- frac_2$frac_index

frac_wide <- frac_1 %>% 
  pivot_wider(id_cols = c("date_month","route"),
              names_from = "death_event",
              names_prefix = "frac_index_",
              values_from = "frac_index")

frac_wide <- data.frame(date_month=frac_wide$date_month, 
                        route=frac_wide$route,
                        sapply(frac_wide[,c(3,4,5)], function(x) ifelse(is.na(x), 0, x)))

ggplot(frac_wide, aes(x=date_month, y=frac_index_more_than_10_deads)) + geom_line() +
  facet_grid(~ route) +
  theme_classic()

frac_wide <- frac_wide %>% 
  pivot_wider(id_cols=date_month,
              names_from = route,
              values_from = c("frac_index_2_to_10_deads","frac_index_less_than_1_dead","frac_index_more_than_10_deads"))

frac_wide <- data.frame(date_month=frac_wide$date_month, 
                        sapply(frac_wide[,2:10], function(x) ifelse(is.na(x), 0, x)))

#Location of death (min,max,mean,median,sd lon and lat)
loc_1 <- migrantfiles_df %>% 
  group_by(date_month, route) %>% 
  summarise(sd_lat = sd(as.numeric(latitude), na.rm=T),
            sd_lon = sd(as.numeric(longitude), na.rm=T))

loc_wide <- loc_1 %>% 
  pivot_wider(id_cols = date_month,
              names_from = route,
              names_prefix = "_",
              values_from = c(sd_lat, sd_lon))

loc_wide <- data.frame(date_month=loc_wide$date_month, 
                        sapply(loc_wide[,2:7], function(x) ifelse(is.na(x), 0, x)))

#join all data sets
migrantfiles_df_month <- data.frame(date_month=migrantfiles_df_month$date_month,
                                    sapply(migrantfiles_df_month[,2:4], function(x) ifelse(is.na(x), 0, x)))


df_migrantfiles_df_month_wide <- left_join(migrantfiles_df_month, loc_wide, by="date_month")
df_migrantfiles_df_month_wide <- left_join(df_migrantfiles_df_month_wide, frac_wide, by="date_month") %>% 
  filter(date_month < "2014-01-01")

rm(frac_1,frac_2,frac_wide,loc_1,loc_wide,migrantfiles_df_month,migrantfiles,migrantfiles_df)
