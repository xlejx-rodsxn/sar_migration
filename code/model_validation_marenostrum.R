library(tidyverse)
library(caret)
library(bsts)
library(lubridate)
library(CausalImpact)
library(janitor)
library(countrycode)
library(tidyverse)
library(scales)
library(imputeTS)

#NOTE: sort observations by time

#http://sisifospage.tech/2017-10-30-forecasting-bsts.html
#https://topepo.github.io/caret/data-splitting.html#data-splitting-for-time-series
# https://en.wikipedia.org/wiki/Mean_absolute_scaled_error

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

df_min_A <- df_reduced_A %>% na.omit() %>% arrange(date)

#Mare Nostrum Oct 18, 2013 - Oct 31, 2014
pre.period_mare_nostrum <- ymd(min(df_min_A$date), "2013-09-01")
post.period_mare_nostrum <- ymd("2013-10-01", max(df_min_A$date))

set.seed(270488)

impact_marenostrum <- CausalImpact(df_min_A,
                                   pre.period = pre.period_mare_nostrum,
                                   post.period = post.period_mare_nostrum,
                                   alpha = 0.05,
                                   model.args = list(nseasons=NULL, dynamic.regression=F, standardize.data=T, max.flips=100, niter=2500))
saveRDS(impact_marenostrum, file="D:\\Postdoc\\DEZIM\\DeZIM\\SAR project\\manuscript\\impact_marenostrum.RDS")

###########################################
###########################################
###########################################
############ SAR Mare Nostrum #############
###########################################
###########################################
###########################################

# Cross validation 
horizon = 3
number_of_folds = 5 

# Variables and cross-validation set-up
nrow(df_min_A[which(df_min_A$date <= ymd("2013-09-01")),])

target <- df_min_A$crossings_CMR[1:26]
X_vars <- data.frame(lapply(df_min_A[1:26,-c(1,2,3)], function(x) scale(x)))
X_vars <- X_vars[,-which(colSums(is.na(X_vars))>0)]
covars_df <- as.matrix(X_vars)

sdy <- sd(target)

ss <- list()
sd.prior <- SdPrior(sigma.guess = 0.01 * sdy,
                    upper.limit = sdy)
#state.spec_seasonal12 <- AddSeasonal(ss, target, nseasons = 2)

state.spec_locallineartrend <- AddLocalLinearTrend(ss, target, sdy=sdy)
state.spec_semilocallineartrend <- AddSemilocalLinearTrend(ss, target, sdy=sdy)
state.spec_locallevel <- AddLocalLevel(ss, target, sigma.prior = sd.prior, sdy=sdy)

state.spec_combined1 <- AddLocalLinearTrend(ss, target, sdy=sdy)
state.spec_combined1 <- AddAutoAr(state.spec_combined1, target, sdy=sdy, lags=3)

state.spec_combined2 <- AddSemilocalLinearTrend(ss, target, sdy=sdy)
state.spec_combined2 <- AddAutoAr(state.spec_combined2, target, sdy=sdy, lags=3)

state.spec_combined3 <- AddLocalLevel(ss, target, sigma.prior = sd.prior, sdy=sdy)
state.spec_combined3 <- AddAutoAr(state.spec_combined3, target, sdy=sdy, lags=3)

state.spec_combined4 <- AddLocalLevel(ss, target, sigma.prior = sd.prior, sdy=sdy)
state.spec_combined4 <- AddSemilocalLinearTrend(state.spec_combined4, target, sdy=sdy)
state.spec_combined4 <- AddAutoAr(state.spec_combined4, target, sdy=sdy, lags=3)

state.spec_default <- impact_marenostrum$model$bsts.model$state.specification

state_specs <- list(state.spec_locallineartrend,
                    state.spec_semilocallineartrend,
                    state.spec_locallevel,
                    state.spec_combined1,
                    state.spec_combined2,
                    state.spec_combined3,
                    state.spec_combined4,
                    state.spec_default)

# With Covariates

# Cross validation loop
bsts.cv.loop_covar <- function(data,
                               covars_df,
                               horizon,
                               number_of_folds,
                               ss.function,
                               niter=1000,
                               seed=270488,
                               burn=250,
                               do.plot=TRUE,
                               verbose=TRUE,
                               debug=TRUE) {
  rmse_v <- c()
  mape_v <- c()
  mase_v <- c()
  for (fold in 1:number_of_folds) {
    # construct data_train/data_test
    l <- length(data) - fold*horizon
    if (debug) print(l)
    data_train <- data[1:l]
    data_test <- data[(l+1):(l+horizon)]
    covars_df_train <- covars_df[1:l]
    covars_df_test <- covars_df[(l+1):(l+horizon)]
    # fit model & predict
    model <- bsts(data_train ~ covars_df_train,
                  state.specification=ss.function,
                  niter=5000,
                  seed=270488,
                  ping=0)
    pred <- predict(model, newdata=covars_df_test, horizon=horizon, burn=burn)
    if (do.plot) {
      # plot
      plot(pred, plot.original = 36)
      lines((l+1):(l+horizon), data_test, col="red", type="l")
    }
    # evaluation
    errors <- data_test-pred$mean
    rmse <- sqrt(mean(errors^2,na.rm=T))
    rmse_v <- c(rmse_v, rmse)
    mape <- mean(abs(errors)/data_test,na.rm=T)*100
    mape_v <- c(mape_v, mape)
    naive_prediction_errors <- diff(data, 1)
    mase <- mean(abs(errors),na.rm=T) / mean(abs(naive_prediction_errors),na.rm=T)
    mase_v <- c(mase_v, mase)
    if (verbose) print(paste0("fold ", fold, ": mape ", mape, " / mase ", mase))
  }
  return(data.frame(rmse=rmse_v,mape=mape_v, mase=mase_v))
}

res_df_covar <- c()
for (ss in state_specs) {
  res <-
    bsts.cv.loop_covar(target,
                       covars_df,
                       horizon=horizon,
                       number_of_folds=number_of_folds,
                       ss.function=ss,
                       debug=FALSE,
                       verbose=FALSE)
  print(paste0(": mean(rmse) ", mean(res$rmse,na.rm=T),
               " / mean(mape) ", mean(res$mape,na.rm=T),
               " / mean(mase) ", mean(res$mase,na.rm=T)))
  res_row <- data.frame(mean_rmse=mean(res$rmse,na.rm=T),
                        mean_mape=mean(res$mape,na.rm=T),
                        mean_mase=mean(res$mase,na.rm=T))
  res_df_covar <- rbind(res_df_covar, res_row)
}

dev.off()
gc()

res_df_covar_ <- cbind(name = c("Local Linear Trend (LLT)",
                                "Semi Local Linear Trend (SLLT)",
                                "Local Level (LL)",
                                "LLT + AR",
                                "SLLT + AR",
                                "LL + AR",
                                "LL + SLT + AR",
                                "Default: LL"), 
                       round(res_df_covar,2))

table_marenostrum <- res_df_covar_ %>% arrange(mean_mase)
colnames(table_marenostrum) <- c("Models","RMSE","MAPE","MASE")
table_marenostrum
saveRDS(table_marenostrum, file="D:/Postdoc/DEZIM/DeZIM/SAR project/manuscript/table_marenostrum.RDS")

# Estimate the causal effect for all models

target_y <- df_min_A$crossings_CMR[order(df_min_A$date)]
post.period <- c(27, 113)
post.period.response <- target_y[post.period[1] : post.period[2]]

target_y[post.period[1] : post.period[2]] <- NA
sdy_y <- sd(target_y)

X_vars <- data.frame(lapply(df_min_A[,-c(1,2,3)], function(x) scale(x)))
X_vars <- X_vars[,-which(colSums(is.na(X_vars))>0)]
covars_df <- as.matrix(X_vars)

model_marenostrum_state.spec_locallineartrend <- bsts(target_y ~ as.matrix(covars_df),
                                                      state.specification=state.spec_locallineartrend,
                                                      niter=2500, max.flips=100,
                                                      seed=270488)
model_marenostrum_state.spec_locallevel <- bsts(target_y ~ as.matrix(covars_df),
                                                state.specification=state.spec_locallevel,
                                                niter=2500, max.flips=100,
                                                seed=270488)
model_marenostrum_state.state.spec_semilocallineartrend <- bsts(target_y ~ as.matrix(covars_df),
                                                                state.specification=state.spec_semilocallineartrend,
                                                                niter=2500, max.flips=100,
                                                                seed=270488)
model_marenostrum_state.spec_autoar <- bsts(target_y ~ as.matrix(covars_df),
                                            state.specification=state.spec_autoar,
                                            niter=2500, max.flips=100,
                                            seed=270488)

model_marenostrum_state.spec_combined1 <- bsts(target_y ~ as.matrix(covars_df),
                                               state.specification=state.spec_combined1,
                                               niter=2500, max.flips=100,
                                               seed=270488)
model_marenostrum_state.spec_combined2 <- bsts(target_y ~ as.matrix(covars_df),
                                               state.specification=state.spec_combined2,
                                               niter=2500, max.flips=100,
                                               seed=270488)
model_marenostrum_state.spec_combined3 <- bsts(target_y ~ as.matrix(covars_df),
                                               state.specification=state.spec_combined3,
                                               niter=2500, max.flips=100,
                                               seed=270488)
model_marenostrum_state.spec_combined4 <- bsts(target_y ~ as.matrix(covars_df),
                                               state.specification=state.spec_combined4,
                                               niter=2500, max.flips=100,
                                               seed=270488)

impact_marenostrum_state.spec_locallineartrend <- CausalImpact(bsts.model = model_marenostrum_state.spec_locallineartrend, post.period.response = post.period.response)
impact_marenostrum_state.spec_locallevel <- CausalImpact(bsts.model = model_marenostrum_state.spec_locallevel, post.period.response = post.period.response)
impact_marenostrum_state.state.spec_semilocallineartrend <- CausalImpact(bsts.model = model_marenostrum_state.state.spec_semilocallineartrend, post.period.response = post.period.response)

impact_marenostrum_state.spec_combined1 <- CausalImpact(bsts.model = model_marenostrum_state.spec_combined1, post.period.response = post.period.response)
impact_marenostrum_state.spec_combined2 <- CausalImpact(bsts.model = model_marenostrum_state.spec_combined2, post.period.response = post.period.response)
impact_marenostrum_state.spec_combined3 <- CausalImpact(bsts.model = model_marenostrum_state.spec_combined3, post.period.response = post.period.response)
impact_marenostrum_state.spec_combined4 <- CausalImpact(bsts.model = model_marenostrum_state.spec_combined4, post.period.response = post.period.response)

compare_mod <- as_tibble(CompareBstsModels(list("LLT" = impact_marenostrum_state.spec_locallineartrend$model$bsts.model,
                       "LL" = impact_marenostrum_state.spec_locallevel$model$bsts.model,
                       "SLLT" = impact_marenostrum_state.state.spec_semilocallineartrend$model$bsts.model,
                       "LLT+AR" = impact_marenostrum_state.spec_combined1$model$bsts.model,
                       "SLLT+AR" = impact_marenostrum_state.spec_combined2$model$bsts.model,
                       "LL+AR" = impact_marenostrum_state.spec_combined3$model$bsts.model,
                       "LL + SLLT + AR" = impact_marenostrum_state.spec_combined4$model$bsts.model,
                       "Default" = impact_marenostrum$model$bsts.model),
                  main = "Model Comparison: Mare Nostrum effects"))
colnames(compare_mod) <- df_min_A$date
compare_mod$Models <- c("LLT",
             "LL",
             "SLLT",
             "LLT+AR",
             "SLLT+AR",
             "LL+AR",
             "LL + SLLT + AR",
             "Default")

compare_mod_long_marenostrum <- compare_mod %>% 
  pivot_longer(1:113)

saveRDS(compare_mod_long_marenostrum, file="D:/Postdoc/DEZIM/DeZIM/SAR project/manuscript/compare_mod_long_marenostrum.RDS")

ggplot(compare_mod_long_marenostrum, aes(x=ymd(name), y=value, group=Models, color=Models)) + geom_line() +
  theme_classic() +
  ylab("Cumulative Absolute Error") + xlab("Date") +
  ggtitle("Model comparison for the Mare Nostrum intervention")

AcfDist(impact_marenostrum$model$posterior.samples)
qqdist(impact_marenostrum$model$posterior.samples)

plot(impact_marenostrum)

effs_marenostrum <- bind_rows(data.frame(model = "LLT", impact_marenostrum_state.spec_locallineartrend$summary)[1,],
          data.frame(model = "LL", impact_marenostrum_state.spec_locallevel$summary)[1,],
          data.frame(model = "SLT", impact_marenostrum_state.state.spec_semilocallineartrend$summary)[1,],
          data.frame(model = "LLT + AR", impact_marenostrum_state.spec_combined1$summary)[1,],
          data.frame(model = "SLT + AR", impact_marenostrum_state.spec_combined2$summary)[1,],
          data.frame(model = "LL + AR", impact_marenostrum_state.spec_combined3$summary)[1,],
          data.frame(model = "LL + SLT + AR", impact_marenostrum_state.spec_combined4$summary)[1,],
          data.frame(model = "Def.: LL", impact_marenostrum$summary)[1,])

saveRDS(effs_marenostrum, file="D:/Postdoc/DEZIM/DeZIM/SAR project/manuscript/effs_marenostrum.RDS")


ggplot(effs_marenostrum, aes(x=model, y=AbsEffect, ymin=AbsEffect.lower, ymax=AbsEffect.upper)) + 
  geom_point() + 
  geom_errorbar() +
  geom_hline(yintercept = 0, lty=2) +
  ggtitle("Effect sizes from different models") +
  theme_classic()
