# North Africa unemployment (through google search for jobs)
#Sys.setlocale("LC_CTYPE", "arabic" )

#Terms in Arabic
"?????? - work"
"?????????? - job"
"?????????? ---  employment"

search_term_work <- c("??????")
output_results_work <- gtrends(keyword = search_term_work,
                               geo = c("DZ","MA","LY","TN","EG"),
                               time = "2008-01-01 2021-12-31")
df_googlesearch_work <- output_results_work$interest_over_time
df_googlesearch_work_wide <- df_googlesearch_work %>% 
  mutate(date=ymd(date)) %>% 
  dplyr::select(c(date,hits,geo)) %>% 
  pivot_wider(id_cols = date, names_from = geo, values_from=hits, names_prefix = "googlesearch_work_")

search_term_job <- c("??????????")
output_results_job <- gtrends(keyword = search_term_job,
                               geo = c("DZ","MA","LY","TN","EG"),
                              time = "2008-01-01 2021-12-31")
df_googlesearch_job <- output_results_job$interest_over_time
df_googlesearch_job_wide <- df_googlesearch_job %>% 
  mutate(date=ymd(date)) %>% 
  dplyr::select(c(date,hits,geo)) %>% 
  pivot_wider(id_cols = date, names_from = geo, values_from=hits, names_prefix = "googlesearch_job_")

search_term_employment <- c("??????????")
output_results_employment <- gtrends(keyword = search_term_employment,
                              geo = c("DZ","MA","LY","TN","EG"),
                              time = "2008-01-01 2021-12-31")
df_googlesearch_employment <- output_results_employment$interest_over_time
df_googlesearch_employment_wide <- df_googlesearch_employment %>% 
  mutate(date=ymd(date)) %>% 
  dplyr::select(c(date,hits,geo)) %>% 
  pivot_wider(id_cols = date, names_from = geo, values_from=hits, names_prefix = "googlesearch_employment_")

df_googlesearch_unemployment <- left_join(df_googlesearch_employment_wide, df_googlesearch_job_wide, by="date")
df_googlesearch_unemployment <- left_join(df_googlesearch_unemployment, df_googlesearch_work_wide, by="date")

df_googlesearch_unemployment <- df_googlesearch_unemployment %>% 
  mutate(googlesearch_employment_DZ = as.numeric(ifelse(googlesearch_employment_DZ == "<1", 0, googlesearch_employment_DZ)),
         googlesearch_employment_MA = as.numeric(ifelse(googlesearch_employment_MA == "<1", 0, googlesearch_employment_MA)),
         googlesearch_employment_LY = as.numeric(ifelse(googlesearch_employment_LY == "<1", 0, googlesearch_employment_LY)),
         googlesearch_employment_TN = as.numeric(ifelse(googlesearch_employment_TN == "<1", 0, googlesearch_employment_TN)),
         googlesearch_employment_EG = as.numeric(ifelse(googlesearch_employment_EG == "<1", 0, googlesearch_employment_EG)))

#computing lags

lags <- c(1:24)
lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                   sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

df_googlesearch_unemployment_lags <- df_googlesearch_unemployment %>% 
  arrange(date) %>% 
  mutate_at(vars("googlesearch_employment_DZ","googlesearch_employment_MA",
                 "googlesearch_employment_LY","googlesearch_employment_TN","googlesearch_employment_EG",
                 "googlesearch_job_DZ","googlesearch_job_MA","googlesearch_job_LY",
                 "googlesearch_job_TN","googlesearch_job_EG","googlesearch_work_DZ",
                 "googlesearch_work_MA","googlesearch_work_LY","googlesearch_work_TN",
                 "googlesearch_work_EG"), funs_(lag_functions))

saveRDS(df_googlesearch_unemployment_lags, file="df_googlesearch_unemployment_lags.RDS")

rm(df_googlesearch_unemployment, 
   search_term_employment, search_term_work, search_term_job, 
   output_results_job, output_results_work, output_results_employment,
   df_googlesearch_employment, df_googlesearch_job, df_googlesearch_work,
   df_googlesearch_employment_wide, df_googlesearch_job_wide, df_googlesearch_work_wide)
