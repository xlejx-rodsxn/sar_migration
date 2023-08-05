# JRC airflow passengeder data
airflow_JRC <- read.csv(file="airflow_JRC.csv")

airflow_JRC$orig.continent <- countrycode(airflow_JRC$orig.iso3, origin="iso3c", destination = "continent")
airflow_JRC$dest.continent <- countrycode(airflow_JRC$dest.iso3, origin="iso3c", destination = "continent")
airflow_JRC$orig.region23 <- countrycode(airflow_JRC$orig.iso3, origin="iso3c", destination = "region23")
airflow_JRC$dest.region23 <- countrycode(airflow_JRC$dest.iso3, origin="iso3c", destination = "region23")

airflow_JRC <- airflow_JRC %>% 
  mutate(date = ym(paste0(year, "-", month)))

airflow_JRC_toallcountries <- airflow_JRC %>% 
  filter(year>=2010 & dest.continent %in% c("Europe")) %>% 
  group_by(date, orig.iso3) %>% 
  summarise(Passengers_sum = sum(Passengers))

airflow_JRC_toallcountries$orig.continent <- countrycode(airflow_JRC_toallcountries$orig.iso3, origin="iso3c", destination = "continent")
airflow_JRC_toallcountries$orig.region23 <- countrycode(airflow_JRC_toallcountries$orig.iso3, origin="iso3c", destination = "region23")
airflow_JRC_toallcountries$orig.countryname <- countrycode(airflow_JRC_toallcountries$orig.iso3, origin="iso3c", destination = "country.name")

airflow_JRC_toallcountries_wide <- airflow_JRC_toallcountries %>% 
  pivot_wider(id_cols = c("date"),
              names_from = orig.iso3,
              values_from = Passengers_sum,
              names_prefix = "airflow_")

airflow_JRC_toallcountries_wide <- data.frame(date=airflow_JRC_toallcountries_wide$date,
                                              sapply(airflow_JRC_toallcountries_wide[,-1], function(x) (as.numeric(x))))

airflow_JRC_toallcountries_wide <- airflow_JRC_toallcountries_wide %>% 
  dplyr::select(-c("airflow_PSE","airflow_YEM","airflow_SYR"))

airflow_JRC_toallcountries_wide <- data.frame(date=airflow_JRC_toallcountries_wide$date,
                                              sapply(airflow_JRC_toallcountries_wide[,-1], function(x) (na_kalman(x))))

lags <- c(1:6)
lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                   sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

df_airflow_JRC_toallcountries_wide_lags <- airflow_JRC_toallcountries_wide %>% 
  arrange(date) %>% 
  mutate_at(vars("airflow_AGO", "airflow_ARE", "airflow_ARM", "airflow_AUT", "airflow_AZE",
                 "airflow_BDI", "airflow_BEL", "airflow_BEN", "airflow_BFA", "airflow_BGR", "airflow_BHR",
                 "airflow_BWA", "airflow_CHE", "airflow_CIV", "airflow_CMR", "airflow_COD", "airflow_COG",
                 "airflow_COM", "airflow_CPV", "airflow_CYP", "airflow_CZE", "airflow_DEU", "airflow_DJI",
                 "airflow_DNK", "airflow_DZA", "airflow_EGY", "airflow_ERI", "airflow_ESP", "airflow_EST",
                 "airflow_ETH", "airflow_FIN", "airflow_FRA", "airflow_GAB", "airflow_GBR", "airflow_GEO",
                 "airflow_GHA", "airflow_GIN", "airflow_GMB", "airflow_GNB", "airflow_GNQ", "airflow_GRC",
                 "airflow_HRV", "airflow_HUN", "airflow_IRL", "airflow_IRN", "airflow_IRQ", "airflow_ISR",
                 "airflow_ITA", "airflow_JOR", "airflow_KEN", "airflow_KWT", "airflow_LBN", "airflow_LBR",
                 "airflow_LBY", "airflow_LSO", "airflow_LTU", "airflow_LUX", "airflow_LVA", "airflow_MAR",
                 "airflow_MDG", "airflow_MLI", "airflow_MLT", "airflow_MOZ", "airflow_MRT", "airflow_MUS",
                 "airflow_MWI", "airflow_NAM", "airflow_NER", "airflow_NGA", "airflow_NLD", "airflow_OMN",
                 "airflow_POL", "airflow_PRT", "airflow_QAT", "airflow_ROU", "airflow_RWA", "airflow_SAU",
                 "airflow_SDN", "airflow_SEN", "airflow_SLE", "airflow_SOM", "airflow_SSD", "airflow_STP",
                 "airflow_SVK", "airflow_SVN", "airflow_SWE", "airflow_SWZ", "airflow_SYC",
                 "airflow_TCD", "airflow_TGO", "airflow_TUN", "airflow_TUR", "airflow_TZA", "airflow_UGA",
                 "airflow_ZAF", "airflow_ZMB", "airflow_ZWE"), 
            funs_(lag_functions))

rm(airflow_JRC_toallcountries_wide, airflow_JRC_toallcountries, airflow_JRC)
