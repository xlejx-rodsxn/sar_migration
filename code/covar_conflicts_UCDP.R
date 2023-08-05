load("GEDEvent_v21_1.RData")

ucdp_deaths <- GEDEvent_v21_1 %>%
  mutate(date = ymd(date_start),
         date_month = ym(paste0(year(date),"-",month(date)))) %>% 
  select(c(date,date_month,country,best))

ucdp_deaths_month <- ucdp_deaths %>% 
  group_by(country, date_month) %>% 
  summarise(deaths = sum(best,na.rm = T))

ucdp_deaths_month_wide <- ucdp_deaths_month %>% 
  pivot_wider(id_cols=date_month,
              names_from=country,
              names_prefix = "ucdp_deaths_",
              values_from = deaths)

ucdp_deaths_month_wide[is.na(ucdp_deaths_month_wide)] <- 0

ucdp_deaths_month_wide <- ucdp_deaths_month_wide %>% 
  mutate_at(vars("ucdp_deaths_Afghanistan",
                 "ucdp_deaths_Albania","ucdp_deaths_Algeria",
                 "ucdp_deaths_Angola","ucdp_deaths_Argentina",
                 "ucdp_deaths_Armenia","ucdp_deaths_Australia",
                 "ucdp_deaths_Austria","ucdp_deaths_Azerbaijan",
                 "ucdp_deaths_Bahrain","ucdp_deaths_Bangladesh",
                 "ucdp_deaths_Belgium","ucdp_deaths_Benin",
                 "ucdp_deaths_Bhutan","ucdp_deaths_Bolivia",
                 "ucdp_deaths_Bosnia-Herzegovina","ucdp_deaths_Botswana",
                 "ucdp_deaths_Brazil","ucdp_deaths_Burkina Faso",
                 "ucdp_deaths_Burundi","ucdp_deaths_Cambodia (Kampuchea)",
                 "ucdp_deaths_Cameroon","ucdp_deaths_Canada",
                 "ucdp_deaths_Central African Republic","ucdp_deaths_Chad",
                 "ucdp_deaths_China","ucdp_deaths_Colombia",
                 "ucdp_deaths_Comoros","ucdp_deaths_Congo",
                 "ucdp_deaths_Croatia","ucdp_deaths_Djibouti",
                 "ucdp_deaths_DR Congo (Zaire)","ucdp_deaths_Ecuador",
                 "ucdp_deaths_Egypt","ucdp_deaths_El Salvador",
                 "ucdp_deaths_Eritrea","ucdp_deaths_Ethiopia",
                 "ucdp_deaths_France","ucdp_deaths_Gambia",
                 "ucdp_deaths_Georgia","ucdp_deaths_Germany",
                 "ucdp_deaths_Ghana","ucdp_deaths_Guatemala",
                 "ucdp_deaths_Guinea","ucdp_deaths_Guinea-Bissau",
                 "ucdp_deaths_Guyana","ucdp_deaths_Haiti",
                 "ucdp_deaths_Honduras","ucdp_deaths_India",
                 "ucdp_deaths_Indonesia","ucdp_deaths_Iran",
                 "ucdp_deaths_Iraq","ucdp_deaths_Israel",
                 "ucdp_deaths_Italy","ucdp_deaths_Ivory Coast",
                 "ucdp_deaths_Jamaica","ucdp_deaths_Jordan",
                 "ucdp_deaths_Kenya","ucdp_deaths_Kingdom of eSwatini (Swaziland)",
                 "ucdp_deaths_Kuwait","ucdp_deaths_Kyrgyzstan",
                 "ucdp_deaths_Laos","ucdp_deaths_Lebanon",
                 "ucdp_deaths_Lesotho","ucdp_deaths_Liberia",
                 "ucdp_deaths_Libya","ucdp_deaths_Macedonia, FYR",
                 "ucdp_deaths_Madagascar (Malagasy)","ucdp_deaths_Malaysia",
                 "ucdp_deaths_Mali","ucdp_deaths_Malta",
                 "ucdp_deaths_Mauritania","ucdp_deaths_Mexico",
                 "ucdp_deaths_Moldova","ucdp_deaths_Morocco",
                 "ucdp_deaths_Mozambique","ucdp_deaths_Myanmar (Burma)",
                 "ucdp_deaths_Namibia","ucdp_deaths_Nepal",
                 "ucdp_deaths_Netherlands","ucdp_deaths_Nicaragua",
                 "ucdp_deaths_Niger","ucdp_deaths_Nigeria",
                 "ucdp_deaths_Pakistan","ucdp_deaths_Panama",
                 "ucdp_deaths_Papua New Guinea","ucdp_deaths_Paraguay",
                 "ucdp_deaths_Peru","ucdp_deaths_Philippines",
                 "ucdp_deaths_Qatar","ucdp_deaths_Romania",
                 "ucdp_deaths_Russia (Soviet Union)","ucdp_deaths_Rwanda",
                 "ucdp_deaths_Saudi Arabia","ucdp_deaths_Senegal",
                 "ucdp_deaths_Serbia (Yugoslavia)","ucdp_deaths_Sierra Leone",
                 "ucdp_deaths_Solomon Islands","ucdp_deaths_Somalia",
                 "ucdp_deaths_South Africa","ucdp_deaths_South Sudan",
                 "ucdp_deaths_Spain","ucdp_deaths_Sri Lanka",
                 "ucdp_deaths_Sudan","ucdp_deaths_Sweden",
                 "ucdp_deaths_Syria","ucdp_deaths_Tajikistan",
                 "ucdp_deaths_Tanzania","ucdp_deaths_Thailand",
                 "ucdp_deaths_Togo","ucdp_deaths_Trinidad and Tobago",
                 "ucdp_deaths_Tunisia","ucdp_deaths_Turkey",
                 "ucdp_deaths_Uganda","ucdp_deaths_Ukraine",
                 "ucdp_deaths_United Arab Emirates","ucdp_deaths_United Kingdom",
                 "ucdp_deaths_United States of America","ucdp_deaths_Uzbekistan",
                 "ucdp_deaths_Venezuela","ucdp_deaths_Yemen (North Yemen)",
                 "ucdp_deaths_Zambia","ucdp_deaths_Zimbabwe (Rhodesia)"), funs_(lag_functions)) %>% 
  rename(date = date_month)

rm(GEDEvent_v21_1,ucdp_deaths,ucdp_deaths_month)
