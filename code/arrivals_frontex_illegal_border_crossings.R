# Frontex data
#frontex_df <- read_excel(path="C:\\Users\\sanchez\\Documents\\DEZIM\\SAR project\\data\\frontex_monthly_detections.xlsx")
#frontex_df_new <- read_excel(path="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\Monthly_detections_of_IBC_2021_08_04.xlsx")
frontex_df_new <- read_excel(path="C:/Users/sanchez/OneDrive - DeZIM-Institut e.V/Dokumente/DEZIM/SAR project/data/MIGRATORY MOVEMENTS/Monthly_detections_of_IBC_20220204.xlsx")

#reshape data
df_frontex_long <- frontex_df_new %>% 
  pivot_longer(cols=4:159) %>% 
  mutate(continent = countrycode(Nationality, origin="country.name", destination = "continent"),
         region23 = countrycode(Nationality, origin="country.name", destination = "region23"),
         continent = ifelse(Nationality=="Unspecified sub-Saharan nationals", "Africa", continent),
         continent = ifelse(Nationality=="Kosovo*", "Europe", continent),
         region23 = ifelse(Nationality=="Kosovo*", "Southern Europe", region23),
         date = my(name),
         year = year(date)) 

df_frontex <- df_frontex_long %>% 
  group_by(Route, date) %>% 
  summarise(value_sum = sum(value)) %>% 
  pivot_wider(id_cols=date,
              values_from = value_sum,
              names_from = Route) %>%
  ungroup() %>% 
  mutate(year = year(date)) %>% 
  rename(arrivals_CMR = `Central Mediterranean Route`,
         arrivals_EBR = `Eastern Borders Route`,
         arrivals_BSR = `Black Sea Route`,
         arrivals_CRAG = `Circular Route from Albania to Greece`,
         arrivals_EMR = `Eastern Mediterranean Route`,
         arrivals_OR = `Other`,
         arrivals_WAR = `Western African Route`,
         arrivals_WBR = `Western Balkan Route`,
         arrivals_WMR = `Western Mediterranean Route`) %>% 
  dplyr::select(date, year, starts_with("arrivals"))

rm(frontex_df_new)