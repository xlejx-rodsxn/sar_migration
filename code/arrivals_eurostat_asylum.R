# Asylum applications EUROSTAT
'
#install.packages("eurostat")
# Load the package
library(eurostat)
library(rvest)
# Get Eurostat data listing
toc <- get_eurostat_toc()
dat <- get_eurostat(id="migr_asyappctzm", time_format = "num")

saveRDS(dat, file = "C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\asylum_eurostat.RDS") 
'
asylum_eurostat <- readRDS(file = "C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\asylum_eurostat.RDS")
#datl <- label_eurostat(dat)

asylum_eurostat$continent <- countrycode(asylum_eurostat$citizen, origin="iso2c", destination = "continent")
asylum_eurostat$country_name <- countrycode(asylum_eurostat$citizen, origin="iso2c", destination = "iso.name.en")
asylum_eurostat$region23 <- countrycode(asylum_eurostat$citizen, origin="iso2c", destination = "region23")

#install.packages("pals")
'library(pals)
pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, polychrome, 
          stepped, tol, watlington,
          show.names=FALSE)
pal.bands(alphabet2())'

df_asylum <- asylum_eurostat %>% 
  filter(asyl_app == "NASY_APP" & sex=="T" & age == "TOTAL" & geo == "EU27_2020" & time >= 2008) 

df_asylum <- df_asylum %>% 
  mutate(year_n = substr(as.character(time), 1, 4),
         month_n = substr(as.character(time), 5, 16),
         month_n = case_when(month_n == "" ~ "01",
                             month_n == ".08333333333" ~ "02",
                             month_n == ".16666666667" ~ "03",
                             month_n == ".25" ~ "04",
                             month_n == ".33333333333" ~ "05",
                             month_n == ".41666666667" ~ "06",
                             month_n == ".5" ~ "07",
                             month_n == ".58333333333" ~ "08",
                             month_n == ".66666666667" ~ "09",
                             month_n == ".75" ~ "10",
                             month_n == ".83333333333" ~ "11",
                             month_n == ".91666666667" ~ "12"),
         date = ym(paste0(year_n,"-",month_n)))

df_asylum_wide <- df_asylum %>% 
  pivot_wider(id_cols = date,
              names_from=citizen,
              values_from = values,
              names_prefix = "asylumclaims_")

rm(df_asylum, asylum_eurostat)
