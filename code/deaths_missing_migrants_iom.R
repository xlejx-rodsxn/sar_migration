# IOM missing migrants project + Pushbacks
IOM_MMP_pushbacks <- read_excel(path="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\mediterranean-data(2021-11-03).xlsx")
IOM_MMP_pushbacks <- IOM_MMP_pushbacks %>% 
  mutate(date = ym(paste0(Year,"-",Month)))

# MMP
mmp_2021 <- read_excel(path="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\MMP\\MissingMigrants-Global-2022-01-14T10 22 32+0800.xlsx")
mmp_2020 <- read_excel(path="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\MMP\\MissingMigrants-Global-2022-01-14T20 23 43+0800.xlsx")
mmp_2019 <- read_excel(path="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\MMP\\MissingMigrants-Global-2022-01-14T20 23 13+0800.xlsx")
mmp_2018 <- read_excel(path="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\MMP\\MissingMigrants-Global-2022-01-14T18 07 44+0800.xlsx")
mmp_2017 <- read_excel(path="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\MMP\\MissingMigrants-Global-2022-01-14T20 22 12+0800.xlsx")
mmp_2016 <- read_excel(path="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\MMP\\MissingMigrants-Global-2022-01-14T20 21 50+0800.xlsx")
mmp_2015 <- read_excel(path="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\MMP\\MissingMigrants-Global-2022-01-14T11 59 24+0800.xlsx")
mmp_2015 <- mmp_2015 %>% 
  mutate(`Total Number of Dead and Missing` = as.numeric(`Total Number of Dead and Missing`))
mmp_2014 <- read_excel(path="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\MMP\\MissingMigrants-Global-2022-01-14T18 53 57+0800.xlsx")

mmp_2014_2021 <- bind_rows(filter(mmp_2014, Region == "Mediterranean"),
                           filter(mmp_2015, Region == "Mediterranean"),
                           filter(mmp_2016, Region == "Mediterranean"),
                           filter(mmp_2017, Region == "Mediterranean"),
                           filter(mmp_2018, Region == "Mediterranean"),
                           filter(mmp_2019, Region == "Mediterranean"),
                           filter(mmp_2020, Region == "Mediterranean"),
                           filter(mmp_2021, Region == "Mediterranean"))

mmp_2014_2021 <- mmp_2014_2021 %>% 
  mutate(date = mdy(substr(`Incident Date`, start=6, stop=15)))

a <- data.frame(strsplit(mmp_2014_2021$Coordinates, ", "))
a_t <- data.frame(t(a))

mmp_2014_2021$longitude <- a_t$X1
mmp_2014_2021$latitude <- a_t$X2

mmp_2014_2021 <- mmp_2014_2021 %>% 
  rename(dead_and_missing  = `Total Number of Dead and Missing`) %>% 
  mutate(date_month = ym(paste0(year(date),"-",month(date))),
         route = case_when(`Migrantion route` == "Central Mediterranean" ~ "Central_Mediterranean",
                           `Migrantion route` == "Western Mediterranean" ~ "Western_Mediterranean",
                           `Migrantion route` == "Eastern Mediterranean" ~ "Eastern_Mediterranean"))

# data frame of survivors per month and route
mmp_survivors <- mmp_2014_2021 %>% 
  mutate(survivors = ifelse(is.na(`Number of Survivors`), 0, `Number of Survivors`)) %>% 
  group_by(date_month, route) %>% 
  summarise(sum_surv = sum(survivors, na.rm=T))


#ggplot(mmp_survivors, aes(x=date_month, y=sum_surv)) + geom_line() + facet_grid(~route)

#saveRDS(mmp_survivors, file="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\follow up projects\\UNDERREGISTER DEATHS proj\\df_mmp_survivors.RDS")

#Fractionalization Index
mmp_2014_2021 <- mmp_2014_2021 %>% 
  mutate(death_event = ifelse(dead_and_missing == 1, "less_than_1_dead",
                              ifelse(dead_and_missing %in% c(2:10), "2_to_10_deads", 
                                     ifelse(dead_and_missing > 10, "more_than_10_deads", NA))))

frac_1 <- mmp_2014_2021 %>% 
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

frac_wide <- frac_wide %>% 
  pivot_wider(id_cols=date_month,
              names_from = route,
              values_from = c("frac_index_2_to_10_deads","frac_index_less_than_1_dead","frac_index_more_than_10_deads"))

frac_wide <- data.frame(date_month=frac_wide$date_month, 
                        sapply(frac_wide[,2:10], function(x) ifelse(is.na(x), 0, x)))

#Location of death (min,max,mean,median,sd lon and lat)
loc_1 <- mmp_2014_2021 %>% 
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

#ggplot(loc_wide, aes(x=date_month, y=sd_lon__Central_Mediterranean)) + geom_line() +
 # theme_classic()

#join all data sets
mmp_2014_2021_df_month <- mmp_2014_2021 %>% 
  group_by(date_month, route) %>% 
  summarise(dead_and_missing = sum(dead_and_missing, na.rm = T))

mmp_2014_2021_df_month_wide <- mmp_2014_2021_df_month %>% 
  pivot_wider(id_cols = date_month,
              names_from = route,
              names_prefix = "dead_and_missing_",
              values_from = dead_and_missing)

mmp_2014_2021_df_month_wide <- data.frame(date_month=mmp_2014_2021_df_month_wide$date_month, 
                                         sapply(mmp_2014_2021_df_month_wide[,2:4], function(x) ifelse(is.na(x), 0, x)))

mmp_2014_2021_df_month_wide <- left_join(mmp_2014_2021_df_month_wide, loc_wide, by="date_month")
mmp_2014_2021_df_month_wide <- left_join(mmp_2014_2021_df_month_wide, frac_wide, by="date_month") %>% 
  filter(date_month >= "2014-01-01")


df_dead_counts_index <- bind_rows(mmp_2014_2021_df_month_wide, df_migrantfiles_df_month_wide)

#saveRDS(dead_counts_index, file="C:/Users/sanchez/OneDrive - DeZIM-Institut e.V/Dokumente/DEZIM/SAR project/follow up projects/UNDERREGISTER DEATHS proj/dead_counts_index.RDS") 


# MMP+pushbacks

MMP_data <- IOM_MMP_pushbacks %>% 
  filter(Details == "Deaths recorded in Central Med") %>% 
  rename(MMP_deaths_count = Count)

#Libya
df_LCG_pushbacks <- IOM_MMP_pushbacks %>% 
  filter(Details == "Interceptions by Libyan Coast Guard" & Route == "Central Mediterranean") %>% 
  rename(LCG_pushbacks_count = Count)

df_LCG_pushbacks <- df_LCG_pushbacks %>% 
  mutate(LCG_pushbacks_count = ifelse(LCG_pushbacks_count < 0, LCG_pushbacks_count*(-1), LCG_pushbacks_count))

#saveRDS(LCG_pushbacks, file="C:/Users/sanchez/OneDrive - DeZIM-Institut e.V/Dokumente/DEZIM/SAR project/follow up projects/UNDERREGISTER DEATHS proj/LCG_pushbacks.RDS") 
#There was a negative value around the Spring 2019 that was recoded to positive

#Tunisia
df_TCG_pushbacks <- IOM_MMP_pushbacks %>% 
  filter(Details == "Interceptions by Tunisian Coast Guard" & Route == "Central Mediterranean") %>% 
  rename(TCG_pushbacks_count = Count)

rm(MMP_data,IOM_MMP_pushbacks,frac_1,frac_2,frac_wide,loc_1,loc_wide,a,a_t,mmp_2014_2021,
   mmp_2014,mmp_2015,mmp_2016,mmp_2017,mmp_2018,mmp_2019,mmp_2020,mmp_2021,
   mmp_2014_2021_df_month,
   mmp_2014_2021_df_month_wide,df_migrantfiles_df_month_wide,
   mmp_survivors)

#saveRDS(TCG_pushbacks, file="C:/Users/sanchez/OneDrive - DeZIM-Institut e.V/Dokumente/DEZIM/SAR project/follow up projects/UNDERREGISTER DEATHS proj/TCG_pushbacks.RDS") 


#Turkey
#TurkCG_pushbacks <- IOM_MMP_pushbacks %>% 
  #filter(Details == "Interceptions by Turkish Coast Guard" & Route == "Eastern Mediterranean") %>% 
  #rename(TCG_pushbacks_count = Count)

#saveRDS(TurkCG_pushbacks, file="C:/Users/sanchez/OneDrive - DeZIM-Institut e.V/Dokumente/DEZIM/SAR project/follow up projects/UNDERREGISTER DEATHS proj/TurkCG_pushbacks.RDS") 
