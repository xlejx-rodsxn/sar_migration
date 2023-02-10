#Exchange rate of currencies relevant to migration movements (only some countries easily available)
#2 Afghan Afghani  AFN
AFN_to_EUR <- historical_exchange_rates(from = "EUR", to = "AFN", start_date = "2008-01-01", end_date = "2021-12-31")
AFN_to_EUR <- AFN_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(AFN_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_AFN)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, AFN_to_EURO_price_avg))

#10 Azerbaijani Manat  AZN
AZN_to_EUR <- historical_exchange_rates(from = "EUR", to = "AZN", start_date = "2008-01-01", end_date = "2021-12-31")
AZN_to_EUR <- AZN_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(AZN_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_AZN)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, AZN_to_EURO_price_avg))

#16 Burundian Franc  BIF
BIF_to_EUR <- historical_exchange_rates(from = "EUR", to = "BIF", start_date = "2008-01-01", end_date = "2021-12-31")
BIF_to_EUR <- BIF_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(BIF_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_BIF)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, BIF_to_EURO_price_avg))

#24 Botswanan Pula  BWP
BWP_to_EUR <- historical_exchange_rates(from = "EUR", to = "BWP", start_date = "2008-01-01", end_date = "2021-12-31")
BWP_to_EUR <- BWP_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(BWP_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_BWP)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, BWP_to_EURO_price_avg))

#28 Congolese Franc  CDF
#CDF_to_EUR <- historical_exchange_rates(from = "EUR", to = "CDF", start_date = "2008-01-01", end_date = "2021-12-31")
#38 Cape Verdean Escudo  CVE
#CVE_to_EUR <- historical_exchange_rates(from = "EUR", to = "CVE", start_date = "2008-01-01", end_date = "2021-12-31")
#40 Djiboutian Franc  DJF
DJF_to_EUR <- historical_exchange_rates(from = "EUR", to = "DJF", start_date = "2008-01-01", end_date = "2021-12-31")
DJF_to_EUR <- DJF_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(DJF_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_DJF)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, DJF_to_EURO_price_avg))

#43 Algerian Dinar  DZD
DZD_to_EUR <- historical_exchange_rates(from = "EUR", to = "DZD", start_date = "2008-01-01", end_date = "2021-12-31")
DZD_to_EUR <- DZD_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(DZD_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_DZD)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, DZD_to_EURO_price_avg))

#44 Egyptian Pound  EGP
EGP_to_EUR <- historical_exchange_rates(from = "EUR", to = "EGP", start_date = "2008-01-01", end_date = "2021-12-31")
EGP_to_EUR <- EGP_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(EGP_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_EGP)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, EGP_to_EURO_price_avg))

#45 Eritrean Nakfa  ERN
#ERN_to_EUR <- historical_exchange_rates(from = "EUR", to = "ERN", start_date = "2008-01-01", end_date = "2021-12-31")
#46 Ethiopian Birr  ETB
ETB_to_EUR <- historical_exchange_rates(from = "EUR", to = "ETB", start_date = "2008-01-01", end_date = "2021-12-31")
ETB_to_EUR <- ETB_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(ETB_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_ETB)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, ETB_to_EURO_price_avg))

#53 Ghanaian Cedi  GHS
GHS_to_EUR <- historical_exchange_rates(from = "EUR", to = "GHS", start_date = "2008-01-01", end_date = "2021-12-31")
GHS_to_EUR <- GHS_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(GHS_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_GHS)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, GHS_to_EURO_price_avg))

#55 Gambian Dalasi  GMD
GMD_to_EUR <- historical_exchange_rates(from = "EUR", to = "GMD", start_date = "2008-01-01", end_date = "2021-12-31")
GMD_to_EUR <- GMD_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(GMD_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_GMD)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, GMD_to_EURO_price_avg))

#56 Guinean Franc  GNF
GNF_to_EUR <- historical_exchange_rates(from = "EUR", to = "GNF", start_date = "2008-01-01", end_date = "2021-12-31")
GNF_to_EUR <- GNF_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(GNF_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_GNF)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, GNF_to_EURO_price_avg))

#68 Iraqi Dinar  IQD
IQD_to_EUR <- historical_exchange_rates(from = "EUR", to = "IQD", start_date = "2008-01-01", end_date = "2021-12-31")
IQD_to_EUR <- IQD_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(IQD_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_IQD)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, IQD_to_EURO_price_avg))

#69 Iranian Rial  IRR
IRR_to_EUR <- historical_exchange_rates(from = "EUR", to = "IRR", start_date = "2008-01-01", end_date = "2021-12-31")
IRR_to_EUR <- IRR_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(IRR_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_IRR)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, IRR_to_EURO_price_avg))

#73 Jordanian Dinar  JOD
JOD_to_EUR <- historical_exchange_rates(from = "EUR", to = "JOD", start_date = "2008-01-01", end_date = "2021-12-31")
JOD_to_EUR <- JOD_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(JOD_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_JOD)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, JOD_to_EURO_price_avg))

#75 Kenyan Shilling  KES
KES_to_EUR <- historical_exchange_rates(from = "EUR", to = "KES", start_date = "2008-01-01", end_date = "2021-12-31")
KES_to_EUR <- KES_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(KES_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_KES)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, KES_to_EURO_price_avg))

#77 Cambodian Riel  KHR
KHR_to_EUR <- historical_exchange_rates(from = "EUR", to = "KHR", start_date = "2008-01-01", end_date = "2021-12-31")
KHR_to_EUR <- KHR_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(KHR_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_KHR)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, KHR_to_EURO_price_avg))

#83 Kazakhstani Tenge  KZT
KZT_to_EUR <- historical_exchange_rates(from = "EUR", to = "KZT", start_date = "2008-01-01", end_date = "2021-12-31")
KZT_to_EUR <- KZT_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(KZT_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_KZT)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, KZT_to_EURO_price_avg))

#85 Lebanese Pound  LBP
LBP_to_EUR <- historical_exchange_rates(from = "EUR", to = "LBP", start_date = "2008-01-01", end_date = "2021-12-31")
LBP_to_EUR <- LBP_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(LBP_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_LBP)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, LBP_to_EURO_price_avg))

#87 Liberian Dollar  LRD
#LRD_to_EUR <- historical_exchange_rates(from = "EUR", to = "LRD", start_date = "2008-01-01", end_date = "2021-12-31")
#88 Lesotho Loti  LSL
LSL_to_EUR <- historical_exchange_rates(from = "EUR", to = "LSL", start_date = "2008-01-01", end_date = "2021-12-31")
LSL_to_EUR <- LSL_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(LSL_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_LSL)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, LSL_to_EURO_price_avg))

#89 Libyan Dinar  LYD
LYD_to_EUR <- historical_exchange_rates(from = "EUR", to = "LYD", start_date = "2008-01-01", end_date = "2021-12-31")
LYD_to_EUR <- LYD_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(LYD_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_LYD)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, LYD_to_EURO_price_avg))

#90 Moroccan Dirham  MAD
MAD_to_EUR <- historical_exchange_rates(from = "EUR", to = "MAD", start_date = "2008-01-01", end_date = "2021-12-31")
MAD_to_EUR <- MAD_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(MAD_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_MAD)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, MAD_to_EURO_price_avg))

#97 Mauritanian Ouguiya (pre-2018)  MRO
MRO_to_EUR <- historical_exchange_rates(from = "EUR", to = "MRO", start_date = "2008-01-01", end_date = "2021-12-31")
MRO_to_EUR <- MRO_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(MRO_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_MRO)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, MRO_to_EURO_price_avg))

#98 Mauritanian Ouguiya  MRU
#MRU_to_EUR <- historical_exchange_rates(from = "EUR", to = "MRU", start_date = "2008-01-01", end_date = "2021-12-31")
#99 Mauritian Rupee  MUR
MUR_to_EUR <- historical_exchange_rates(from = "EUR", to = "MUR", start_date = "2008-01-01", end_date = "2021-12-31")
MUR_to_EUR <- MUR_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(MUR_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_MUR)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, MUR_to_EURO_price_avg))

#101 Malawian Kwacha  MWK
MWK_to_EUR <- historical_exchange_rates(from = "EUR", to = "MWK", start_date = "2008-01-01", end_date = "2021-12-31")
MWK_to_EUR <- MWK_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(MWK_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_MWK)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, MWK_to_EURO_price_avg))

#104 Mozambican Metical  MZN
#MZN_to_EUR <- historical_exchange_rates(from = "EUR", to = "MZN", start_date = "2008-01-01", end_date = "2021-12-31")
#105 Namibian Dollar  NAD
NAD_to_EUR <- historical_exchange_rates(from = "EUR", to = "NAD", start_date = "2008-01-01", end_date = "2021-12-31")
NAD_to_EUR <- NAD_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(NAD_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_NAD)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, NAD_to_EURO_price_avg))

#106 Nigerian Naira  NGN
NGN_to_EUR <- historical_exchange_rates(from = "EUR", to = "NGN", start_date = "2008-01-01", end_date = "2021-12-31")
NGN_to_EUR <- NGN_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(NGN_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_NGN)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, NGN_to_EURO_price_avg))

#116 Pakistani Rupee  PKR
#PKR_to_EUR <- historical_exchange_rates(from = "EUR", to = "PKR", start_date = "2008-01-01", end_date = "2021-12-31")
#123 Rwandan Franc  RWF
RWF_to_EUR <- historical_exchange_rates(from = "EUR", to = "RWF", start_date = "2008-01-01", end_date = "2021-12-31")
RWF_to_EUR <- RWF_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(RWF_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_RWF)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, RWF_to_EURO_price_avg))

#127 Sudanese Pound  SDG
#SDG_to_EUR <- historical_exchange_rates(from = "EUR", to = "SDG", start_date = "2008-01-01", end_date = "2021-12-31")
#131 Sierra Leonean Leone  SLL
#SLL_to_EUR <- historical_exchange_rates(from = "EUR", to = "SLL", start_date = "2008-01-01", end_date = "2021-12-31")
#132 Somali Shilling  SOS
SOS_to_EUR <- historical_exchange_rates(from = "EUR", to = "SOS", start_date = "2008-01-01", end_date = "2021-12-31")
SOS_to_EUR <- SOS_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(SOS_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_SOS)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, SOS_to_EURO_price_avg))

#134 South Sudanese Pound  SSP
#SSP_to_EUR <- historical_exchange_rates(from = "EUR", to = "SSP", start_date = "2008-01-01", end_date = "2021-12-31")
#138 Syrian Pound  SYP
SYP_to_EUR <- historical_exchange_rates(from = "EUR", to = "SYP", start_date = "2008-01-01", end_date = "2021-12-31")
SYP_to_EUR <- SYP_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(SYP_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_SYP)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, SYP_to_EURO_price_avg))

#139 Swazi Lilangeni  SZL
SZL_to_EUR <- historical_exchange_rates(from = "EUR", to = "SZL", start_date = "2008-01-01", end_date = "2021-12-31")
SZL_to_EUR <- SZL_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(SZL_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_SZL)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, SZL_to_EURO_price_avg))

#143 Tunisian Dinar  TND
TND_to_EUR <- historical_exchange_rates(from = "EUR", to = "TND", start_date = "2008-01-01", end_date = "2021-12-31")
TND_to_EUR <- TND_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(TND_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_TND)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, TND_to_EURO_price_avg))

#144 Tongan Pa anga  TOP
TOP_to_EUR <- historical_exchange_rates(from = "EUR", to = "TOP", start_date = "2008-01-01", end_date = "2021-12-31")
TOP_to_EUR <- TOP_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(TOP_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_TOP)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, TOP_to_EURO_price_avg))

#145 Turkish Lira  TRY
TRY_to_EUR <- historical_exchange_rates(from = "EUR", to = "TRY", start_date = "2008-01-01", end_date = "2021-12-31")
TRY_to_EUR <- TRY_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(TRY_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_TRY)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, TRY_to_EURO_price_avg))

#148 Tanzanian Shilling  TZS
TZS_to_EUR <- historical_exchange_rates(from = "EUR", to = "TZS", start_date = "2008-01-01", end_date = "2021-12-31")
TZS_to_EUR <- TZS_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(TZS_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_TZS)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, TZS_to_EURO_price_avg))

#150 Ugandan Shilling  UGX
UGX_to_EUR <- historical_exchange_rates(from = "EUR", to = "UGX", start_date = "2008-01-01", end_date = "2021-12-31")
UGX_to_EUR <- UGX_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(UGX_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_UGX)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, UGX_to_EURO_price_avg))

#153 Uzbekistan Som  UZS
UZS_to_EUR <- historical_exchange_rates(from = "EUR", to = "UZS", start_date = "2008-01-01", end_date = "2021-12-31")
UZS_to_EUR <- UZS_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(UZS_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_UZS)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, UZS_to_EURO_price_avg))

#159 CFA Franc BEAC  XAF
#XAF_to_EUR <- historical_exchange_rates(from = "EUR", to = "XAF", start_date = "2008-01-01", end_date = "2021-12-31")
#160 Silver Ounce  XAG
#XAG_to_EUR <- historical_exchange_rates(from = "EUR", to = "XAG", start_date = "2008-01-01", end_date = "2021-12-31")
#161 Gold Ounce  XAU
#XAU_to_EUR <- historical_exchange_rates(from = "EUR", to = "XAU", start_date = "2008-01-01", end_date = "2021-12-31")
#164 CFA Franc BCEAO  XOF
#XOF_to_EUR <- historical_exchange_rates(from = "EUR", to = "XOF", start_date = "2008-01-01", end_date = "2021-12-31")
#165 Palladium Ounce  XPD
#XPD_to_EUR <- historical_exchange_rates(from = "EUR", to = "XPD", start_date = "2008-01-01", end_date = "2021-12-31")
#166 CFP Franc  XPF
#XPF_to_EUR <- historical_exchange_rates(from = "EUR", to = "XPF", start_date = "2008-01-01", end_date = "2021-12-31")
#167 Platinum Ounce  XPT
#XPT_to_EUR <- historical_exchange_rates(from = "EUR", to = "XPT", start_date = "2008-01-01", end_date = "2021-12-31")
#168 Yemeni Rial  YER
#YER_to_EUR <- historical_exchange_rates(from = "EUR", to = "YER", start_date = "2008-01-01", end_date = "2021-12-31")
#169 South African Rand  ZAR
ZAR_to_EUR <- historical_exchange_rates(from = "EUR", to = "ZAR", start_date = "2008-01-01", end_date = "2021-12-31")
ZAR_to_EUR <- ZAR_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(ZAR_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_ZAR)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, ZAR_to_EURO_price_avg))

#170 Zambian Kwacha  ZMW
ZMW_to_EUR <- historical_exchange_rates(from = "EUR", to = "ZMW", start_date = "2008-01-01", end_date = "2021-12-31")
ZMW_to_EUR <- ZMW_to_EUR %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(date = ym(paste0(year,"-",month))) %>% 
  group_by(date) %>%
  summarise(ZMW_to_EURO_price_avg = mean(one_EUR_equivalent_to_x_ZMW)) %>% 
  ungroup() %>% 
  dplyr::select(c(date, ZMW_to_EURO_price_avg))

#171 Zimbabwean Dollar  ZWL
#ZWL_to_EUR <- historical_exchange_rates(from = "EUR", to = "ZWL", start_date = "2008-01-01", end_date = "2021-12-31")

df_currencies <- left_join(AFN_to_EUR, AZN_to_EUR, by="date")
df_currencies <- left_join(df_currencies, BIF_to_EUR, by="date")
df_currencies <- left_join(df_currencies, BWP_to_EUR, by="date")
df_currencies <- left_join(df_currencies, DJF_to_EUR, by="date")
df_currencies <- left_join(df_currencies, DZD_to_EUR, by="date")
df_currencies <- left_join(df_currencies, EGP_to_EUR, by="date")
df_currencies <- left_join(df_currencies, ETB_to_EUR, by="date")
df_currencies <- left_join(df_currencies, GHS_to_EUR, by="date")
df_currencies <- left_join(df_currencies, GMD_to_EUR, by="date")
df_currencies <- left_join(df_currencies, GNF_to_EUR, by="date")
df_currencies <- left_join(df_currencies, IQD_to_EUR, by="date")
df_currencies <- left_join(df_currencies, IRR_to_EUR, by="date")
df_currencies <- left_join(df_currencies, JOD_to_EUR, by="date")
df_currencies <- left_join(df_currencies, KES_to_EUR, by="date")
df_currencies <- left_join(df_currencies, KHR_to_EUR, by="date")
df_currencies <- left_join(df_currencies, KZT_to_EUR, by="date")
df_currencies <- left_join(df_currencies, LBP_to_EUR, by="date")
df_currencies <- left_join(df_currencies, LSL_to_EUR, by="date")
df_currencies <- left_join(df_currencies, LYD_to_EUR, by="date")
df_currencies <- left_join(df_currencies, MAD_to_EUR, by="date")
df_currencies <- left_join(df_currencies, MRO_to_EUR, by="date")
df_currencies <- left_join(df_currencies, MUR_to_EUR, by="date")
df_currencies <- left_join(df_currencies, MWK_to_EUR, by="date")
df_currencies <- left_join(df_currencies, NAD_to_EUR, by="date")
df_currencies <- left_join(df_currencies, NGN_to_EUR, by="date")
df_currencies <- left_join(df_currencies, RWF_to_EUR, by="date")
df_currencies <- left_join(df_currencies, SOS_to_EUR, by="date")
df_currencies <- left_join(df_currencies, SYP_to_EUR, by="date")
df_currencies <- left_join(df_currencies, SZL_to_EUR, by="date")
df_currencies <- left_join(df_currencies, TND_to_EUR, by="date")
df_currencies <- left_join(df_currencies, TOP_to_EUR, by="date")
df_currencies <- left_join(df_currencies, TRY_to_EUR, by="date")
df_currencies <- left_join(df_currencies, TZS_to_EUR, by="date")
df_currencies <- left_join(df_currencies, UGX_to_EUR, by="date")
df_currencies <- left_join(df_currencies, UZS_to_EUR, by="date")
df_currencies <- left_join(df_currencies, ZAR_to_EUR, by="date")
df_currencies <- left_join(df_currencies, ZMW_to_EUR, by="date")

lags <- c(1:24)
lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                   sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

df_currencies_lags <- df_currencies %>% 
  arrange(date) %>% 
  mutate_at(vars("AFN_to_EURO_price_avg", "AZN_to_EURO_price_avg", "BIF_to_EURO_price_avg",
                 "BWP_to_EURO_price_avg", "DJF_to_EURO_price_avg", "DZD_to_EURO_price_avg", "EGP_to_EURO_price_avg",
                 "ETB_to_EURO_price_avg", "GHS_to_EURO_price_avg", "GMD_to_EURO_price_avg", "GNF_to_EURO_price_avg",
                 "IQD_to_EURO_price_avg", "IRR_to_EURO_price_avg", "JOD_to_EURO_price_avg", "KES_to_EURO_price_avg",
                 "KHR_to_EURO_price_avg", "KZT_to_EURO_price_avg", "LBP_to_EURO_price_avg", "LSL_to_EURO_price_avg",
                 "LYD_to_EURO_price_avg", "MAD_to_EURO_price_avg", "MRO_to_EURO_price_avg", "MUR_to_EURO_price_avg",
                 "MWK_to_EURO_price_avg", "NAD_to_EURO_price_avg", "NGN_to_EURO_price_avg", "RWF_to_EURO_price_avg",
                 "SOS_to_EURO_price_avg", "SYP_to_EURO_price_avg", "SZL_to_EURO_price_avg", "TND_to_EURO_price_avg",
                 "TOP_to_EURO_price_avg", "TRY_to_EURO_price_avg", "TZS_to_EURO_price_avg", "UGX_to_EURO_price_avg",
                 "UZS_to_EURO_price_avg", "ZAR_to_EURO_price_avg", "ZMW_to_EURO_price_avg"), 
            funs_(lag_functions))

rm(df_currencies,
   AFN_to_EUR,AZN_to_EUR,BIF_to_EUR,BWP_to_EUR,DJF_to_EUR,DZD_to_EUR,EGP_to_EUR,ETB_to_EUR,GHS_to_EUR,GMD_to_EUR,
   GNF_to_EUR,IQD_to_EUR,IRR_to_EUR,JOD_to_EUR,KES_to_EUR,KHR_to_EUR,KZT_to_EUR,LBP_to_EUR,LSL_to_EUR,LYD_to_EUR,
   MAD_to_EUR,MRO_to_EUR,MUR_to_EUR,MWK_to_EUR,NAD_to_EUR,NGN_to_EUR,RWF_to_EUR,SOS_to_EUR,SYP_to_EUR,SZL_to_EUR,
   TND_to_EUR,TOP_to_EUR,TRY_to_EUR,TZS_to_EUR,UGX_to_EUR,UZS_to_EUR,ZAR_to_EUR,ZMW_to_EUR)
