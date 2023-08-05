#Environmental disasters
environ_emdat_ <- read_excel(path = "emdat_public_2021_11_04_query_uid-7PdQaV.xlsx",
                            skip=6)
environ_emdat <- filter(environ_emdat_, Continent %in% c("Africa","Asia"))

environ_emdat <- environ_emdat %>% 
  dplyr::select(c(`Start Year`,`Start Month`,`Start Day`, Country,`Disaster Subtype`)) %>% 
  mutate(type_disaster = `Disaster Subtype`,
         date = ym(paste0(`Start Year`,"-",`Start Month`))) %>% 
  filter(date >= "2000-01-02")

environ_emdat_grouped <- environ_emdat %>% 
  group_by(date, Country) %>% 
  summarise(count=n())

environ_emdat_grouped_wide <- environ_emdat_grouped %>% 
  pivot_wider(id_cols = date,
              names_from = Country,
              names_prefix = "disas_count_",
              values_from = count)

df_environ_emdat_grouped_wide <- data.frame(date = environ_emdat_grouped_wide$date, 
                                            sapply(environ_emdat_grouped_wide[,-1], function(x) as.numeric(ifelse(is.na(x), 0, x))))

df_environ_emdat_grouped_wide <- df_environ_emdat_grouped_wide %>% 
  rename(disas_count_Cote.d.Ivoire = `disas_count_Côte.d.Ivoire`,
         disas_count_Reunion = `disas_count_Réunion`)

lags <- c(1:24)
lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                   sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

df_environ_emdat_grouped_wide_lags <- df_environ_emdat_grouped_wide %>% 
  arrange(date) %>% 
  mutate_at(vars("disas_count_Angola",
                 "disas_count_Benin","disas_count_Botswana",
                 "disas_count_China","disas_count_Ethiopia",
                 "disas_count_India","disas_count_Indonesia",
                 "disas_count_Iran..Islamic.Republic.of.","disas_count_Kenya",
                 "disas_count_Madagascar","disas_count_Mongolia",
                 "disas_count_Morocco","disas_count_Mozambique",
                 "disas_count_Namibia","disas_count_Nigeria",
                 "disas_count_Pakistan","disas_count_Philippines..the.",
                 "disas_count_Somalia","disas_count_South.Africa",
                 "disas_count_Tanzania..United.Republic.of","disas_count_Thailand",
                 "disas_count_Uganda","disas_count_Viet.Nam",
                 "disas_count_Bangladesh","disas_count_Central.African.Republic",
                 "disas_count_Cote.d.Ivoire","disas_count_Cyprus",
                 "disas_count_Egypt","disas_count_Japan",
                 "disas_count_Korea..the.Republic.of.","disas_count_Malawi",
                 "disas_count_Niger..the.","disas_count_Saudi.Arabia",
                 "disas_count_Sierra.Leone","disas_count_Sri.Lanka",
                 "disas_count_Zambia","disas_count_Congo..the.Democratic.Republic.of.the.",
                 "disas_count_Congo..the.","disas_count_Kazakhstan",
                 "disas_count_Turkey","disas_count_Afghanistan",
                 "disas_count_Burundi","disas_count_Cameroon",
                 "disas_count_Guinea","disas_count_Liberia",
                 "disas_count_Nepal","disas_count_Senegal",
                 "disas_count_Tajikistan","disas_count_Tunisia",
                 "disas_count_Armenia","disas_count_Djibouti",
                 "disas_count_Ghana","disas_count_Lao.People.s.Democratic.Republic..the.",
                 "disas_count_Mali","disas_count_Cambodia",
                 "disas_count_Hong.Kong","disas_count_Israel",
                 "disas_count_Jordan","disas_count_Mauritania",
                 "disas_count_Rwanda","disas_count_Zimbabwe",
                 "disas_count_Algeria","disas_count_Bahrain",
                 "disas_count_Bhutan","disas_count_Georgia",
                 "disas_count_Korea..the.Democratic.People.s.Republic.of.","disas_count_Malaysia",
                 "disas_count_Sudan..the.","disas_count_Taiwan..Province.of.China.",
                 "disas_count_Uzbekistan","disas_count_Azerbaijan",
                 "disas_count_Iraq","disas_count_Singapore",
                 "disas_count_Yemen","disas_count_Kyrgyzstan",
                 "disas_count_Gambia..the.","disas_count_Lebanon",
                 "disas_count_Syrian.Arab.Republic","disas_count_Chad",
                 "disas_count_Equatorial.Guinea","disas_count_Swaziland",
                 "disas_count_Turkmenistan","disas_count_Burkina.Faso",
                 "disas_count_Togo","disas_count_Myanmar",
                 "disas_count_Saint.Helena..Ascension.and.Tristan.da.Cunha","disas_count_Timor.Leste",
                 "disas_count_United.Arab.Emirates..the.","disas_count_Gabon",
                 "disas_count_Lesotho","disas_count_Mauritius",
                 "disas_count_Reunion","disas_count_Oman",
                 "disas_count_Guinea.Bissau","disas_count_Cabo.Verde",
                 "disas_count_Libya","disas_count_Seychelles",
                 "disas_count_Comoros..the.","disas_count_Kuwait",
                 "disas_count_Macao","disas_count_Eritrea",
                 "disas_count_Maldives","disas_count_Sao.Tome.and.Principe",
                 "disas_count_Palestine..State.of","disas_count_South.Sudan",
                 "disas_count_Mayotte","disas_count_Qatar"), 
            funs_(lag_functions))

rm(df_environ_emdat_grouped_wide,environ_emdat_grouped_wide,environ_emdat_grouped,environ_emdat,environ_emdat_)
