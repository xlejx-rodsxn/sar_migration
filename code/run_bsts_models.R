library(lubridate)
library(CausalImpact)
library(janitor)
library(countrycode)
library(tidyverse)
library(scales)
library(imputeTS)

#read file
df <- readRDS(file="D:\\Postdoc\\DEZIM\\DeZIM\\SAR project\\data\\compiled\\df.RDS")
df_y_rec <- readRDS(file="D:\\Postdoc\\DEZIM\\DeZIM\\SAR project\\code\\df_y_rec.RDS")

df <- left_join(df, df_y_rec, by="date")

#Identifying relevant/important predictors to include in the model
df <- df %>% 
  mutate(LCG_pushbacks_count = as.numeric(ifelse(is.na(LCG_pushbacks_count), 0, LCG_pushbacks_count)),
         TCG_pushbacks_count = as.numeric(ifelse(is.na(TCG_pushbacks_count), 0, TCG_pushbacks_count)),
         dead_and_missing_Central_Mediterranean = as.numeric(ifelse(is.na(dead_and_missing_Central_Mediterranean), 0, dead_and_missing_Central_Mediterranean)),
         #crossings_CMR_r = arrivals_CMR + LCG_pushbacks_count + TCG_pushbacks_count + y_rec,
         crossings_CMR = arrivals_CMR + LCG_pushbacks_count + TCG_pushbacks_count + dead_and_missing_Central_Mediterranean,
         mortality_rate = (dead_and_missing_Central_Mediterranean/crossings_CMR)*1000)

#modeling
#Time series decomposition
#attempted crossings
y <- ts(df$crossings_CMR, start = c(2009,1), frequency=12)

stl_decomp1 <- stl(y, t.window = 24, s.window=12)
plot(stl_decomp1)
stl_decomp2 <- decompose(y, type="additive")
plot(stl_decomp2)

#install.packages("forecast")
library(forecast)
y_seasadj <- seasadj(stl_decomp2)
autoplot(cbind(y, y_seasadj))
y_star1 <- y - stl_decomp1$time.series[,1] - stl_decomp1$time.series[,2]
autoplot(cbind(y, y_star1))
y_star2 <- y - stl_decomp2$seasonal - stl_decomp2$trend
autoplot(cbind(y, y_star2))

#Structural changes in the series by means of chow test

#install.packages("strucchange")
library(strucchange)
bp_ts <- breakpoints(log(y) ~ 1, h=12)
summary(bp_ts)
ci_ts <- confint(bp_ts)
plot(log(y), 
     main = "Structural breakpoints in montly number of attempted crossings (log)",
     ylab = "Log of number of attempted crossings",
     xlab = "Date")
lines(bp_ts, col="darkred")
lines(ci_ts)

# no breakpoints are found for the mortality rate
library(strucchange)
bp_ts <- breakpoints(df$mortality_rate ~ 1, h=12)
summary(bp_ts)
ci_ts <- confint(bp_ts)
plot(df$y_rec,type="l")
lines(df$dead_and_missing_Central_Mediterranean, col="red")
lines(ci_ts)

#deaths
y_d1 <- ts(df$dead_and_missing_Central_Mediterranean, start = c(2009,1), frequency=12)
y_d2 <- ts(df$y_rec, start = c(2009,1), frequency=12)

stl_decomp1_d <- stl(y_d, t.window = 24, s.window=12)
plot(stl_decomp1_d)
stl_decomp2_d <- decompose(y_d, type="additive")
plot(stl_decomp2_d)

## seasonality decomposition of death rate
y_seasadj_d <- seasadj(stl_decomp2_d)
autoplot(cbind(y_d, y_seasadj_d))
y_star1_d <- y_d - stl_decomp1_d$time.series[,1] - stl_decomp1_d$time.series[,2]
autoplot(cbind(y_d, y_star1_d))
y_star2_d <- y_d - stl_decomp2_d$seasonal - stl_decomp2_d$trend
autoplot(cbind(y_d, y_star2_d))

#Structural changes in the series by means of chow test on the death rate
bp_ts_d1 <- breakpoints(log(y_d1 + 1) ~ 1, h=12)
summary(bp_ts_d1)
ci_ts_d1 <- confint(bp_ts_d1)
plot(log(y_d1), main="Original data")
lines(bp_ts_d1)
lines(ci_ts_d1)

bp_ts_d2 <- breakpoints(log(y_d2 + 1) ~ 1, h=12)
summary(bp_ts_d2)
ci_ts_d2 <- confint(bp_ts_d2)
plot(log(y_d2), main="Reconstructed")
lines(bp_ts_d2)
lines(ci_ts_d2)

# no breakpoints are found for the mortality rate
library(strucchange)
bp_ts <- breakpoints(df$mortality_rate ~ 1, h=12)
summary(bp_ts)
ci_ts <- confint(bp_ts)
plot(df$mortality_rate)
lines(bp_ts)
lines(ci_ts)

# Estimating a first model

#dropping some of the variables before model selection
df_reduced <- df %>% 
  filter(date>="2011-02-01" & date<"2021-10-01") %>% 
  dplyr::select(-c(contains("lag_24",ignore.case = TRUE),
                   contains("lag_23",ignore.case = TRUE),
                   contains("lag_22",ignore.case = TRUE),
                   contains("lag_21",ignore.case = TRUE),
                   contains("lag_20",ignore.case = TRUE),
                   contains("lag_19",ignore.case = TRUE),
                   contains("lag_18",ignore.case = TRUE),
                   contains("lag_17",ignore.case = TRUE),
                   contains("lag_16",ignore.case = TRUE),
                   contains("lag_15",ignore.case = TRUE),
                   contains("lag_14",ignore.case = TRUE),
                   contains("lag_13",ignore.case = TRUE),
                   contains("lag_12",ignore.case = TRUE),
                   contains("lag_11",ignore.case = TRUE),
                   contains("lag_10",ignore.case = TRUE),
                   contains("lag_09",ignore.case = TRUE),
                   contains("lag_08",ignore.case = TRUE),
                   contains("lag_07",ignore.case = TRUE),
                   starts_with("airflow_Palestinian.Territories"),
                   starts_with("asylum"),
                   "arrivals_BSR","arrivals_CMR","arrivals_CRAG","arrivals_EBR","arrivals_EMR","arrivals_OR","arrivals_WAR","arrivals_WBR","arrivals_WMR",
                   "dead_and_missing_Eastern_Mediterranean","dead_and_missing_Central_Mediterranean","dead_and_missing_Western_Mediterranean",
                   "sd_lat__Eastern_Mediterranean","sd_lat__Central_Mediterranean","sd_lat__Western_Mediterranean","sd_lon__Eastern_Mediterranean","sd_lon__Central_Mediterranean","sd_lon__Western_Mediterranean",
                   "frac_index_2_to_10_deads_Eastern_Mediterranean","frac_index_2_to_10_deads_Central_Mediterranean","frac_index_2_to_10_deads_Western_Mediterranean",
                   "frac_index_less_than_1_dead_Eastern_Mediterranean","frac_index_less_than_1_dead_Central_Mediterranean","frac_index_less_than_1_dead_Western_Mediterranean",
                   "frac_index_more_than_10_deads_Eastern_Mediterranean","frac_index_more_than_10_deads_Central_Mediterranean","frac_index_more_than_10_deads_Western_Mediterranean",
                   "LCG_pushbacks_count","TCG_pushbacks_count",
                   "y_rec","mortality_rate"))

df_reduced_A <- data.frame(date=df_reduced$date,
                           crossings_CMR = log(df_reduced$crossings_CMR),
                           dplyr::select(df_reduced, -c(date,crossings_CMR)))

df_reduced_A <- df_reduced_A %>% 
  mutate(month = month(date),
         semester = semester(date),
         quarter = quarter(date))

df_min_A <- df_reduced_A %>% na.omit()

#Spike and slap prior to select variables
model <- logit.spike((crossings_CMR) ~ .,
                     data = dplyr::select(df_min_A, -c(date,month,semester,quarter)),
                     niter = 1000,
                     nthreads = 7,
                     seed = 270488)
smry.model <- summary(model)
plot(model, inc = 0.0001)
d_coefs <- data.frame(smry.model$coefficients)
d_coefs$names <- row.names(d_coefs)
d_coefs_best <- d_coefs %>% dplyr::filter(inc.prob >= 0.0001) %>% 
  dplyr::filter(names != "(Intercept)")
dplyr::select(row.names(d_coefs %>% dplyr::filter(inc.prob >= 0.0001)), -"(Intercept)")

df_min_A_ <- dplyr::select(df_min_A, c(date,crossings_CMR,
                                       starts_with("temperature"), starts_with("precipitation"), starts_with("daysstorm"),
                                       starts_with("ucdp_deaths_Syria"),
                                       airflow_LTU_lag_2,
                                       airflow_LUX_lag_2,
                                       PWOOLF_lag_05,
                                       airflow_NLD_lag_1,
                                       LBP_to_EURO_price_avg_lag_05,
                                       KES_to_EURO_price_avg_lag_04,
                                       airflow_QAT_lag_3,
                                       unem_GREECE_lag_04,
                                       num_expvio_Somalia,
                                       num_riots_Malawi_lag_06,
                                       PFSHMEAL,
                                       airflow_EST_lag_2,
                                       num_expvio_Cameroon_lag_04,
                                       disas_count_Botswana_lag_05,
                                       PWOOLF_lag_01,
                                       airflow_ITA_lag_2,
                                       airflow_MLI_lag_3,
                                       PZINC_lag_04,
                                       KHR_to_EURO_price_avg_lag_05,
                                       airflow_FIN_lag_4,
                                       num_battles_Togo_lag_04,
                                       airflow_GIN_lag_2,
                                       KES_to_EURO_price_avg_lag_02,
                                       NAD_to_EURO_price_avg_lag_01,
                                       PBARL_lag_03,
                                       airflow_ZMB_lag_6,
                                       airflow_GMB_lag_6,
                                       airflow_GAB_lag_6,
                                       airflow_ZWE_lag_2,
                                       airflow_SLE_lag_2,
                                       PCOFFOTM_lag_04,
                                       num_expvio_Egypt_lag_05,
                                       num_expvio_Burundi_lag_02,
                                       num_protest_Central.African.Republic_lag_01,
                                       TOP_to_EURO_price_avg_lag_06,
                                       BWP_to_EURO_price_avg_lag_06,
                                       TZS_to_EURO_price_avg_lag_03,
                                       airflow_BEN_lag_6,
                                       airflow_CMR_lag_5,
                                       airflow_GEO_lag_1))


'
starts_with("temperature"), starts_with("precipitation"), starts_with("daysstorm"),
                                       unem_ITALY_lag_06,
                                       PALUM_lag_05,
                                       airflow_EGY_lag_4,
                                       PWOOLC_lag_05,
                                       PALUM_lag_06,
                                       PNFUEL,
                                       PPOTASH_lag_02,
                                       PTOMATO_lag_05,
                                       PWOOLC_lag_04,
                                       IQD_to_EURO_price_avg_lag_03,
                                       airflow_TUN_lag_5,
                                       num_riots_Mauritania_lag_05,
                                       PEXGMETA_lag_05,
                                       num_expvio_Guinea.Bissau_lag_04,
                                       JOD_to_EURO_price_avg_lag_04,
                                       daysstorm_italy_lag_03,
                                       PFANDB,
                                       PNFUEL_lag_04,
                                       PAPPLE_lag_05,
                                       num_riots_Cameroon_lag_05,
                                       PWHEAMT_lag_04,
                                       airflow_SSD_lag_5,
                                       airflow_MRT_lag_4,
                                       disas_count_Viet.Nam_lag_05,
                                       PNGASEU_lag_03
'

df_reduced <- df_reduced %>% 
  mutate(crossings_CMR = log(df_reduced$crossings_CMR))

df_reduced_cc <- na.omit(df_reduced)
df_reduced_cc <- data.frame(date=df_reduced_cc$date,
                           crossings_CMR = df_reduced_cc$crossings_CMR,
                           dplyr::select(df_reduced_cc, -c(date,crossings_CMR))) %>% 
  mutate(month = month(date),
         semester = semester(date),
         quarter = quarter(date))

#Mare Nostrum Oct 18, 2013 - Oct 31, 2014
pre.period_mare_nostrum <- ymd(min(df_min_A$date), "2013-09-01")
post.period_mare_nostrum <- ymd("2013-10-01", max(df_min_A$date))
#SAR by NGOs
pre.period_sar_ngos <- ymd(min(df_min_A$date), "2014-10-01")
post.period_sar_ngos <- ymd("2014-11-01", max(df_min_A$date))
#EU Libya cooperation
pre.period_sarlibya <- ymd(min(df_min_A$date), "2017-01-01")
post.period_sar_libya <- ymd("2017-02-01", max(df_min_A$date))

set.seed(270488)

#Models
impact_marenostrum <- CausalImpact(df_min_A,
                                   pre.period = pre.period_mare_nostrum,
                                   post.period = post.period_mare_nostrum,
                                   alpha = 0.05,
                                   model.args = list(dynamic.regression=F, standardize.data=T, max.flips=100, niter=10000))
plot(impact_marenostrum, "original")
plot(impact_marenostrum$model$bsts.model, "coefficients", inc = 0.01)

impact_marenostrum$summary$AbsEffect
impact_marenostrum$summary$RelEffect

impact_sarngos <- CausalImpact(df_min_A,
                               pre.period = pre.period_sar_ngos,
                               post.period = post.period_sar_ngos,
                               alpha = 0.05,
                               model.args = list(dynamic.regression=F, standardize.data=T, max.flips=100, niter=10000))
plot(impact_sarngos, "original")
plot(impact_sarngos$model$bsts.model, "coefficients", inc=0.01)

impact_sarlibya <- CausalImpact(df_min_A,
                                pre.period = pre.period_sarlibya, post.period = post.period_sar_libya,
                                alpha = 0.05,
                                model.args = list(dynamic.regression=F, standardize.data=T, max.flips=100, niter=10000))
plot(impact_sarlibya)
plot(impact_sarlibya, "original")
plot(impact_sarlibya$model$bsts.model, "coefficients", inc=0.01)

saveRDS(impact_marenostrum, file="D:/Postdoc/DEZIM/DeZIM/SAR project/manuscript/impact_marenostrum.RDS")
saveRDS(impact_sarngos, file="D:/Postdoc/DEZIM/DeZIM/SAR project/manuscript/impact_sarngos.RDS")
saveRDS(impact_sarlibya, file="D:/Postdoc/DEZIM/DeZIM/SAR project/manuscript/impact_sarlibya.RDS")

#Storing model results
model_df_results_A <- data.frame(Original = impact_marenostrum$series$response,
                                 Prediction = impact_marenostrum$series$point.pred,
                                 Prediction_lower = impact_marenostrum$series$point.pred.lower,
                                 Prediction_upper = impact_marenostrum$series$point.pred.upper,
                                 Pointwise_effect = impact_marenostrum$series$point.effect,      
                                 Pointwise_effect_lower = impact_marenostrum$series$point.effect.lower,
                                 Pointwise_effect_upper = impact_marenostrum$series$point.effect.upper,
                                 Cumulative_effect = impact_marenostrum$series$cum.effect,
                                 Cumulative_effect_lower = impact_marenostrum$series$cum.effect.lower,
                                 Cumulative_effect_upper = impact_marenostrum$series$cum.effect.upper)
model_df_results_A$date <- ymd(row.names(model_df_results_A))

model_df_results_C <- data.frame(Original = impact_sarlibya$series$response,
                                 Prediction = impact_sarlibya$series$point.pred,
                                 Prediction_lower = impact_sarlibya$series$point.pred.lower,
                                 Prediction_upper = impact_sarlibya$series$point.pred.upper,
                                 Pointwise_effect = impact_sarlibya$series$point.effect,      
                                 Pointwise_effect_lower = impact_sarlibya$series$point.effect.lower,
                                 Pointwise_effect_upper = impact_sarlibya$series$point.effect.upper,
                                 Cumulative_effect = impact_sarlibya$series$cum.effect,
                                 Cumulative_effect_lower = impact_sarlibya$series$cum.effect.lower,
                                 Cumulative_effect_upper = impact_sarlibya$series$cum.effect.upper)
model_df_results_C$date <- ymd(row.names(model_df_results_C))


model_df_results_B <- data.frame(Original = impact_sarngos$series$response,
                                 Prediction = impact_sarngos$series$point.pred,
                                 Prediction_lower = impact_sarngos$series$point.pred.lower,
                                 Prediction_upper = impact_sarngos$series$point.pred.upper,
                                 Pointwise_effect = impact_sarngos$series$point.effect,      
                                 Pointwise_effect_lower = impact_sarngos$series$point.effect.lower,
                                 Pointwise_effect_upper = impact_sarngos$series$point.effect.upper,
                                 Cumulative_effect = impact_sarngos$series$cum.effect,
                                 Cumulative_effect_lower = impact_sarngos$series$cum.effect.lower,
                                 Cumulative_effect_upper = impact_sarngos$series$cum.effect.upper)
model_df_results_B$date <- ymd(row.names(model_df_results_B))

saveRDS(model_df_results_A, file="D:/Postdoc/DEZIM/DeZIM/SAR project/manuscript/model_df_results_A_all.RDS")
saveRDS(model_df_results_B, file="D:/Postdoc/DEZIM/DeZIM/SAR project/manuscript/model_df_results_B_all.RDS")
saveRDS(model_df_results_C, file="D:/Postdoc/DEZIM/DeZIM/SAR project/manuscript/model_df_results_C_all.RDS")

#####################################################################
#####################################################################
##### DEATHS

y_d <- ts(df$dead_and_missing_Central_Mediterranean, start = c(2009,1), frequency=12)
library(strucchange)
bp_ts_d <- breakpoints(log(y_d+0.1) ~ 1, h=12)
summary(bp_ts_d)
ci_ts_d <- confint(bp_ts_d)
plot(log(y_d), 
     main = "Structural breakpoints in montly number of dead and missing in CMR",
     ylab = "Number of dead and missing",
     xlab = "Date")
lines(bp_ts_d, col="darkred")
lines(ci_ts_d)


df_reduced_d <- df %>% 
  filter(date>="2011-02-01" & date<"2021-10-01") %>% 
  dplyr::select(-c(contains("lag_24",ignore.case = TRUE),
                   contains("lag_23",ignore.case = TRUE),
                   contains("lag_22",ignore.case = TRUE),
                   contains("lag_21",ignore.case = TRUE),
                   contains("lag_20",ignore.case = TRUE),
                   contains("lag_19",ignore.case = TRUE),
                   contains("lag_18",ignore.case = TRUE),
                   contains("lag_17",ignore.case = TRUE),
                   contains("lag_16",ignore.case = TRUE),
                   contains("lag_15",ignore.case = TRUE),
                   contains("lag_14",ignore.case = TRUE),
                   contains("lag_13",ignore.case = TRUE),
                   contains("lag_12",ignore.case = TRUE),
                   contains("lag_11",ignore.case = TRUE),
                   contains("lag_10",ignore.case = TRUE),
                   contains("lag_09",ignore.case = TRUE),
                   contains("lag_08",ignore.case = TRUE),
                   contains("lag_07",ignore.case = TRUE),
                   starts_with("airflow_Palestinian.Territories"),
                   starts_with("asylum"),
                   "mortality_rate","y_rec",
                   "arrivals_BSR","arrivals_CRAG","arrivals_EBR","arrivals_EMR","arrivals_OR","arrivals_WAR","arrivals_WBR","arrivals_WMR",
                   "dead_and_missing_Eastern_Mediterranean","dead_and_missing_Western_Mediterranean",
                   "sd_lat__Eastern_Mediterranean","sd_lat__Central_Mediterranean","sd_lat__Western_Mediterranean","sd_lon__Eastern_Mediterranean","sd_lon__Central_Mediterranean","sd_lon__Western_Mediterranean",
                   "frac_index_2_to_10_deads_Eastern_Mediterranean","frac_index_2_to_10_deads_Central_Mediterranean","frac_index_2_to_10_deads_Western_Mediterranean",
                   "frac_index_less_than_1_dead_Eastern_Mediterranean","frac_index_less_than_1_dead_Central_Mediterranean","frac_index_less_than_1_dead_Western_Mediterranean",
                   "frac_index_more_than_10_deads_Eastern_Mediterranean","frac_index_more_than_10_deads_Central_Mediterranean","frac_index_more_than_10_deads_Western_Mediterranean"))

df_reduced_B <- data.frame(date=df_reduced_d$date,
                           dead_and_missing_Central_Mediterranean = log(df_reduced_d$dead_and_missing_Central_Mediterranean + 1),
                           dplyr::select(df_reduced_d, -c(date,dead_and_missing_Central_Mediterranean)))

df_reduced_B <- df_reduced_B %>% 
  mutate(month = month(date),
         semester = semester(date),
         quarter = quarter(date))

df_min_B <- df_reduced_B %>% na.omit()

# Deaths
model_d <- logit.spike((dead_and_missing_Central_Mediterranean) ~ .,
                     data = dplyr::select(df_min_B, -c(date,month,semester,quarter)),
                     niter = 1000,
                     nthreads = 7,
                     seed = 270488)
smry.model_d <- summary(model_d)
plot(model_d, inc = 0.1)
d_coefs <- data.frame(smry.model_d$coefficients)
d_coefs$names <- row.names(d_coefs)
d_coefs_best <- d_coefs %>% dplyr::filter(inc.prob >= 0.0001) %>% 
  dplyr::filter(names != "(Intercept)")
dplyr::select(row.names(d_coefs %>% dplyr::filter(inc.prob >= 0.0001)), -"(Intercept)")


impact_marenostrum_d <- CausalImpact(df_min_B,
                                   pre.period = pre.period_mare_nostrum,
                                   post.period = post.period_mare_nostrum,
                                   alpha = 0.05,
                                   model.args = list(dynamic.regression=F, standardize.data=T, max.flips=100, niter=10000))
plot(impact_marenostrum_d)
plot(impact_marenostrum_d$model$bsts.model, "coefficients", inc=0.1)

impact_sarngos_d <- CausalImpact(df_min_B,
                               pre.period = pre.period_sar_ngos,
                               post.period = post.period_sar_ngos,
                               alpha = 0.05,
                               model.args = list(dynamic.regression=F, standardize.data=T, max.flips=100, niter=10000))
plot(impact_sarngos_d)
plot(impact_sarngos$model$bsts.model, "coefficients", inc=0.001)

impact_sarlibya_d <- CausalImpact(df_min_B,
                                pre.period = pre.period_sarlibya, post.period = post.period_sar_libya,
                                alpha = 0.05,
                                model.args = list(dynamic.regression=F, standardize.data=T, max.flips=100, niter=10000))
plot(impact_sarngos_d)

#df_min_B <- df_reduced_B %>% na.omit()

#Spike and slap prior to select variables
model <- logit.spike((dead_and_missing_Central_Mediterranean) ~ .,
                     data = dplyr::select(df_reduced_B, -c(date,month,semester,quarter)),
                     niter = 1000,
                     nthreads = 7,
                     seed = 270488)

#Plots
plotA <- ggplot(model_df_results_A, aes(x=date, y=prediction, ymin=prediction_lower, ymax=prediction_upper)) + 
  geom_line(lty=2, col="darkblue") +
  geom_ribbon(alpha=0.4, fill="lightblue") +
  geom_line(aes(y=original)) + 
  geom_vline(col="darkred", xintercept = ym("2013-10"), lty=2) +
  annotate("rect", xmin = ym("2013-10"), xmax = ym("2014-10"), ymin = -1, ymax = 20,
           alpha = .08, fill = "red") +
  ylim(c(-1,20)) +
  ylab(" ") + 
  xlab(" ") +
  labs(title="A. Mare Nostrum") +
  scale_color_manual(name = "Inflow of migrants", values = c("prediction" = "darkblue", "original" = "red")) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90))

plotB <- ggplot(model_df_results_B, aes(x=date, y=prediction, ymin=prediction_lower, ymax=prediction_upper)) + 
  geom_line(lty=2, col="darkblue") +
  geom_ribbon(alpha=0.4, fill="lightblue") +
  geom_line(aes(y=original)) + 
  geom_vline(col="darkred", xintercept = ym("2014-11"), lty=2) +
  annotate("rect", xmin = ym("2014-11"), xmax = ym("2017-02"), ymin = -1, ymax = 20,
           alpha = .08, fill = "red") +
  ylim(c(-1,20)) +
  ylab("Log of attempted crossings") + 
  xlab(" ") +
  labs(title="B. NGOs search-and-rescue") +
  scale_color_manual(name = "Inflow of migrants", values = c("prediction" = "darkblue", "original" = "red")) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90))

plotC <- ggplot(model_df_results_C, aes(x=date, y=prediction, ymin=prediction_lower, ymax=prediction_upper)) + 
  geom_line(lty=2, col="darkblue") +
  geom_ribbon(alpha=0.4, fill="lightblue") +
  geom_line(aes(y=original)) + 
  geom_vline(col="darkred", xintercept = ym("2017-02"), lty=2) +
  annotate("rect", xmin = ym("2017-02"), xmax = ym("2021-01"), ymin = -1, ymax = 20,
           alpha = .08, fill = "red") +
  ylim(c(-1,20)) +
  ylab(" ") + 
  xlab("Date") +
  labs(title="C. EU and Libya cooperation") +
  scale_color_manual(name = "Inflow of migrants", values = c("prediction" = "darkblue", "original" = "red")) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90))

gridExtra::grid.arrange(plotA, plotB, plotC, ncol=1)

#original plots
summary(impact_sarlibya)
plot(impact_sarlibya)
plot(impact_sarlibya$model$bsts.model, "coefficients")
summary(impact_sarngos)
plot(impact_sarngos)
plot(impact_sarngos$model$bsts.model, "coefficients")
summary(impact_marenostrum)
plot(impact_marenostrum)
plot(impact_marenostrum$model$bsts.model, "coefficients")




d <- data.frame(vars=names(colMeans(impact_sarlibya$model$bsts.model$coefficients)),
                coefs = colMeans(impact_sarlibya$model$bsts.model$coefficients))

ggplot(filter(d, coefs>0), aes(x=reorder(vars, abs(coefs)), y=coefs)) + 
  geom_bar(stat="identity") + 
  xlab("Predictors") + ylab("Std. Coefficient Size") +
  theme_classic() + 
  theme(axis.text.y = element_text(size = 12)) +
  coord_flip()

library(gridExtra)
pdf(file = "C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\results_plot_more.pdf", width = 15, height = 12)
grid.arrange(arrangeGrob(plotA, plotB, ncol=2), 
             arrangeGrob(plotC, ncol=1, nrow=1), nrow=2)
dev.off()


# Custom model
attach(df_min)
post.period2 <- c(29, 90)
y <- df_min$arrivals_CMR
post.period.response <- y[post.period2[1] : post.period2[2]]
y[post.period2[1] : post.period2[2]] <- NA

ss <- AddLocalLevel(list(), y)
ss <- AddLocalLinearTrend(ss, y)
ss <- AddSeasonal(list(), y, nseasons = 12, season.duration = 1)
ss <- AddSeasonal(ss, y, nseasons = 4, season.duration = 3)
ss <- AddTrig(ss, y, period = 3, frequencies = 1)
ss <- AddAutoAr(ss, y, lags = 1)
ss <- AddSemilocalLinearTrend(ss, y)

#ss <- AddAr(ss, lags = 3, sigma.prior = SdPrior(3.0, 1.0))

bsts.model <- bsts(y ~ 
                     ALG_passengers_lag12 + EGY_passengers_lag12 + LYB_passengers_lag12 + MOR_passengers_lag12 + TUN_passengers_lag12 + 
                     unem_euro_area_all_lag1 + unem_euro_area_all_lag3 + unem_euro_area_all_lag6 + unem_euro_area_all_lag12 + 
                     num_battles_Libya + num_battles_Libya_lag1 + num_battles_Libya_lag3 + num_battles_Libya_lag6 + num_battles_Libya_lag12 + 
                     num_battles_Somalia + num_battles_Somalia_lag1 + num_battles_Somalia_lag3 + num_battles_Somalia_lag6 + num_battles_Somalia_lag12 + 
                     syria_trend_google + syria_trend_google_lag1 + syria_trend_google_lag3 + syria_trend_google_lag6 + syria_trend_google_lag12 + 
                     oilprice_open_avg + oilprice_open_avg_lag1 + oilprice_open_avg_lag3 + oilprice_open_avg_lag6 + oilprice_open_avg_lag12 + 
                     LDY_to_EURO_price_avg + LDY_to_EURO_price_avg_lag1 + LDY_to_EURO_price_avg_lag3 + LDY_to_EURO_price_avg_lag6 + LDY_to_EURO_price_avg_lag12 + 
                     temperature_malta +  precipitation_malta + daysstorm_malta +  
                     temperature_italy + precipitation_italy + daysstorm_italy,  
                   state.specification = ss, 
                   family = "gaussian",
                   niter = 10000,
                   seed=270488)
bsts.model1 <- bsts(y ~ temperature_malta +  precipitation_malta + daysstorm_malta +  
                      temperature_italy + precipitation_italy + daysstorm_italy,  
                    state.specification = ss, 
                    family = "gaussian",
                    niter = 1000)
bsts.model2 <- bsts(y ~ oilprice_open_avg + oilprice_open_avg_lag1 + oilprice_open_avg_lag3 + oilprice_open_avg_lag6 + oilprice_open_avg_lag12 + 
                      LDY_to_EURO_price_avg + LDY_to_EURO_price_avg_lag1 + LDY_to_EURO_price_avg_lag3 + LDY_to_EURO_price_avg_lag6 + LDY_to_EURO_price_avg_lag12 + 
                      unem_euro_area_all_lag1 + unem_euro_area_all_lag3 + unem_euro_area_all_lag6 + unem_euro_area_all_lag12 + 
                      temperature_malta +  precipitation_malta + daysstorm_malta +  
                      temperature_italy + precipitation_italy + daysstorm_italy,  
                    state.specification = ss, 
                    family = "gaussian",
                    niter = 1000)
bsts.model3 <- bsts(y ~
                      num_battles_Libya + num_battles_Libya_lag1 + num_battles_Libya_lag3 + num_battles_Libya_lag6 + num_battles_Libya_lag12 + 
                      num_battles_Somalia + num_battles_Somalia_lag1 + num_battles_Somalia_lag3 + num_battles_Somalia_lag6 + num_battles_Somalia_lag12 + 
                      syria_trend_google + syria_trend_google_lag1 + syria_trend_google_lag3 + syria_trend_google_lag6 + syria_trend_google_lag12 + 
                      oilprice_open_avg + oilprice_open_avg_lag1 + oilprice_open_avg_lag3 + oilprice_open_avg_lag6 + oilprice_open_avg_lag12 + 
                      unem_euro_area_all_lag1 + unem_euro_area_all_lag3 + unem_euro_area_all_lag6 + unem_euro_area_all_lag12 + 
                      LDY_to_EURO_price_avg + LDY_to_EURO_price_avg_lag1 + LDY_to_EURO_price_avg_lag3 + LDY_to_EURO_price_avg_lag6 + LDY_to_EURO_price_avg_lag12 + 
                      temperature_malta +  precipitation_malta + daysstorm_malta +  
                      temperature_italy + precipitation_italy + daysstorm_italy,  
                    state.specification = ss, 
                    family = "gaussian",
                    niter = 1000)
plot(bsts.model, "components")
plot(bsts.model, "coef")
plot(bsts.model)

CompareBstsModels(list("Model 1" = bsts.model1,
                       "Model 2" = bsts.model2,
                       "Model 3" = bsts.model3,
                       "Model full" = bsts.model),
                  colors = c("green", "red", "blue", "black"))

impact2 <- CausalImpact(bsts.model = bsts.model,
                        post.period.response = post.period.response)
plot(impact2)
summary(impact2)


errors <- bsts.prediction.errors(bsts.model, burn = 1000)
PlotDynamicDistribution(errors$in.sample)

pred.bsts <- predict(bsts.model, newdata=df_min[29:90,])
plot(pred.bsts$median)