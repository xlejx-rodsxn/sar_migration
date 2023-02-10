#Europe and OECD unemployment rates
unemp_eu <- read_xlsx(path="C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\Macroeconomics\\unemployment_eu.xlsx")
unemp_eu <- unemp_eu %>% 
  mutate_all(~ na_if(., ':'))

unemp_eu$`2020-02` <- as.numeric(unemp_eu$`2020-02`)
unemp_eu$`2020-03` <- as.numeric(unemp_eu$`2020-03`)
unemp_eu$`2020-04` <- as.numeric(unemp_eu$`2020-04`)
unemp_eu$`2020-05` <- as.numeric(unemp_eu$`2020-05`)
unemp_eu$`2020-06` <- as.numeric(unemp_eu$`2020-06`)
unemp_eu$`2020-07` <- as.numeric(unemp_eu$`2020-07`)
unemp_eu$`2020-08` <- as.numeric(unemp_eu$`2020-08`)
unemp_eu$`2020-09` <- as.numeric(unemp_eu$`2020-09`)
unemp_eu$`2020-10` <- as.numeric(unemp_eu$`2020-10`)
unemp_eu$`2020-11` <- as.numeric(unemp_eu$`2020-11`)
unemp_eu$`2020-12` <- as.numeric(unemp_eu$`2020-12`)
unemp_eu$`2021-01` <- as.numeric(unemp_eu$`2021-01`)
unemp_eu$`2021-02` <- as.numeric(unemp_eu$`2021-02`)
unemp_eu$`2021-03` <- as.numeric(unemp_eu$`2021-03`)
unemp_eu$`2021-04` <- as.numeric(unemp_eu$`2021-04`)
unemp_eu$`2021-05` <- as.numeric(unemp_eu$`2021-05`)
unemp_eu$`2021-06` <- as.numeric(unemp_eu$`2021-06`)

unemp_eu_long <- unemp_eu %>%
  pivot_longer(cols=2:139) %>% 
  mutate(date = ym(name))

unemp_eu_wide <- unemp_eu_long %>% 
  pivot_wider(id_cols = date,
              values_from = value,
              names_from = area)

unemp_eu_wide <- unemp_eu_wide %>% 
  dplyr::select(-c("unem_eu_28countries","unem_eu_28countries","unem_eu_27Bcountries","unem_eu_25countries"))

unemp_eu_wide <- data.frame(date=unemp_eu_wide$date,
                            sapply(unemp_eu_wide[,-1], function(x) na_kalman(x)))

lags <- c(1:12)
lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                   sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

df_unemp_eu_wide_lags <- unemp_eu_wide %>% 
  arrange(date) %>% 
  mutate_at(vars("unem_eu_27Acountries","unem_euro_area_all",
                 "unem_euro_area_19countries","unem_euro_area_18countries","unem_BELGIUM",
                 "unem_BULGARIA","unem_CZECHIA","unem_DENMARK",
                 "unem_GERMANY","unem_ESTONIA","unem_IRELAND",
                 "unem_GREECE","unem_SPAIN","unem_FRANCE",
                 "unem_CROATIA","unem_ITALY","unem_CYPRUS",
                 "unem_LATVIA","unem_LITHUANIA","unem_LUXEMBOURG",
                 "unem_HUNGARY","unem_MALTA","unem_NETHERLANDS",
                 "unem_AUSTRIA","unem_POLAND","unem_PORTUGAL",
                 "unem_ROMANIA","unem_SLOVENIA","unem_SLOVAKIA",
                 "unem_FINLAND","unem_SWEDEN","unem_ICELAND",
                 "unem_NORWAY","unem_SWITZERLAND","unem_UK",
                 "unem_TURKEY","unem_USA","unem_JAPAN"), funs_(lag_functions))

rm(unemp_eu_wide, unemp_eu_long, unemp_eu)
