#IOM online available arrival data
'iom_arrivals <- read_excel(path = "IOM_monthly.xlsx")
iom_arrivals <- iom_arrivals %>% 
  mutate(date = ym(paste0(year,"-",month))) %>% 
  dplyr::select(c(date, land_arrivals, sea_arrivals, total_arrivals))
'
IOM_DTM_data1 <- read_excel(path = "DTM_Europe_CMR_Arrivals ITA MLT_2010-2021.xlsx",
                            sheet = "Nationality")

IOM_DTM_data2 <- read_excel(path = "DTM_Europe_CMR_Arrivals ITA MLT_2010-2021.xlsx",
                            sheet = "Arrivals")
IOM_DTM_data2_all <- IOM_DTM_data2 %>% 
  group_by(`Arrival Date`, `Country of<br/> Departure`) %>% 
  summarise(arrivals_IOM = sum(Arrivals)) %>% 
  mutate(date_ymd = ymd(`Arrival Date`)) %>% 
  filter(date_ymd >= "2016-01-01")

ggplot(data=IOM_DTM_data2_all, aes(x=date_ymd, y=arrivals_IOM)) + 
  geom_line() + 
  theme_classic() +
  facet_wrap(~`Country of<br/> Departure`)
