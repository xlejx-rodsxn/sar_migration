# Weather data
#weather_lybia <- readRDS(file = "C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\weather\\weather_lybia.RDS")
weather_malta <- readRDS(file = "C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\weather\\weather_malta.RDS")
weather_italy <- readRDS(file = "C:\\Users\\sanchez\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\weather\\weather_italy.RDS")

weather_malta <- dplyr::select(weather_malta, c(1,2,7,11))
names(weather_malta) <-  c("date", "temperature_malta", "precipitation_malta", "daysstorm_malta")
weather_malta <- weather_malta %>% 
  mutate(date = my(date),
         temperature_malta = as.numeric(temperature_malta),
         precipitation_malta = as.numeric(precipitation_malta),
         daysstorm_malta = as.numeric(daysstorm_malta))

weather_italy <- dplyr::select(weather_italy, c(1,2,7,11))
names(weather_italy) <-  c("date", "temperature_italy", "precipitation_italy", "daysstorm_italy")
weather_italy <- weather_italy %>% 
  mutate(date = my(date),
         temperature_italy = as.numeric(temperature_italy),
         precipitation_italy = as.numeric(precipitation_italy),
         daysstorm_italy = as.numeric(daysstorm_italy))

weather_covars <- left_join(weather_malta, weather_italy, by="date")

weather_covars <- data.frame(date = weather_covars$date,
                             sapply(weather_covars[,-1], function(x) na_kalman(x)))

weather_covars <- na.omit(weather_covars)

lags <- c(1:12)
lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                   sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

df_weather_covars_lags <- weather_covars %>% 
  arrange(date) %>% 
  mutate_at(vars("temperature_malta","precipitation_malta","daysstorm_malta","temperature_italy","precipitation_italy","daysstorm_italy"), 
            funs_(lag_functions))

rm(weather_covars, weather_malta, weather_italy)