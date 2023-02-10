library(Hmisc)
library(priceR)
library(lubridate)
library(CausalImpact)
library(janitor)
library(readxl)
library(gtrendsR)
library(countrycode)
library(tidyverse)
library(scales)
library(countrycode)
library(imputeTS)
library(readstata13)

setwd("C:\\Users\\sanchez.DEZIM-INSTITUT\\DEZIM\\SAR project\\code")

# INTERVENTIONS

#dates operations and all NGOs active in the CMR
source("dates_search_and_rescue_EU_NGOS_dates.R")

#saveRDS(df_dates, file="C:/Users/sanchez/OneDrive - DeZIM-Institut e.V/Dokumente/DEZIM/SAR project/follow up projects/UNDERREGISTER DEATHS proj/df_dates.RDS") 

##################### ARRIVALS

#Frontex: illegal border crossings
source("arrivals_frontex_illegal_border_crossings.R")

#Asylum stats from EUROSTAT
source("arrivals_eurostat_asylum.R")

#JRC airflow data
source("arrivals_JRC_airportdata.R")

#Arrivals IOM (not used since is the same as Frontex)

##################### DEATHS

#Migrant Files
source("deaths_migrant_files.R")

#Missing Migrants Project
source("deaths_missing_migrants_iom.R")

#Deaths UNITED (not used since is part of the Migrant Files)

##################### COVARIATES

# Weather in Italy and Malta
source("covar_weather_italy_malta.R")

# Syrian conflict (Google search)
source("covar_syrian_conflict_googletrends.R")

# Currency Exchange rates - remittances
source("covar_exchange_rates.R")

# Unemployment rates Europe
source("covar_EU_unemploymentrates.R")

# Environmental disasters in Africa and Asia
source("covar_environ_disasters.R")

# Conflicts ACLED
source("covar_conflicts_ACLED.R")

# Conflicts UCDP
source("covar_conflicts_UCDP.R")

# Commodities prices
source("covar_commodities_prices.R")

# Job search in Arabic in North Africa (NOTE: do not source file, characters in arabic are not read properly and change)
#source("covar_AFRICA_unemploymentrates.R")
df_googlesearch_unemployment_lags <- readRDS(file="C:\\Users\\sanchez.DEZIM-INSTITUT\\DEZIM\\SAR project\\code\\df_googlesearch_unemployment_lags.RDS")

# Changing some variable names before merge
df_dead_counts_index <- df_dead_counts_index %>% 
  rename(date = date_month)

#################### JOINING ALL VARIABLES INTO ONE DATA SET
# Merge of various arrivals
df <- left_join(df_frontex, df_asylum_wide, by="date")
df <- left_join(df, df_airflow_JRC_toallcountries_wide_lags, by="date")
# Merge of dead and missing counts
df <- left_join(df, df_dead_counts_index, by="date")
# Merge of pushbacks
df <- left_join(df, dplyr::select(df_LCG_pushbacks, c(date, LCG_pushbacks_count)), by="date")
df <- left_join(df, dplyr::select(df_TCG_pushbacks, c(date, TCG_pushbacks_count)), by="date")
# Merge of covariate series and their lags
df <- left_join(df, df_weather_covars_lags, by="date")
df <- left_join(df, df_syrianconflict, by="date")
df <- left_join(df, df_currencies_lags, by="date")
df <- left_join(df, df_unemp_eu_wide_lags, by="date")
df <- left_join(df, df_environ_emdat_grouped_wide_lags, by="date")
df <- left_join(df, df_acled_africa_df_all_lags, by="date")
#df <- left_join(df, ucdp_deaths_month_wide, by="date")
df <- left_join(df, df_commodityprice_lags, by="date")
df <- left_join(df, df_googlesearch_unemployment_lags, by="date")

saveRDS(df, file="C:\\Users\\sanchez.DEZIM-INSTITUT\\DEZIM\\SAR project\\data\\compiled\\df.RDS")

#After joining the data set, check which covariates have missing values for the starting period and drop those

