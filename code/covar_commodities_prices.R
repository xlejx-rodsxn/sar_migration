# Commodity prices
df_imf <- read_xlsx(path = "C:\\Users\\sanchez\\OneDrive\\OneDrive - DeZIM-Institut e.V\\Dokumente\\DEZIM\\SAR project\\data\\IMF\\df_imf.xlsx",
                    skip=0)
df_imf <- df_imf[-c(1,2,3),]

df_imf <- df_imf %>% 
  mutate(year = str_split_fixed(df_imf$Commodity, "M", n=2)[,1],
         month = str_split_fixed(df_imf$Commodity, "M", n=2)[,2],
         date = ym(paste0(year,"-",month)))

df_commodityprice <- dplyr::select(df_imf, c("date", "PALLFNF","PEXGALL","PNFUEL","PFANDB","PFOOD",
                                  "PBEVE","PINDU","PAGRI","PRAWM","PALLMETA","PMETA",
                                  "PPMETA","PEXGMETA","PFERT","PNRG","POILAPSP...17","PNGAS",
                                  "PCOAL","PALUM","PBANSOP","PBARL","PBEEF","PCOALAU",
                                  "PCOALSA_USD","PCOCO","PCOFFOTM","PCOFFROB","PROIL","PCOPP",
                                  "PCOTTIND","PFSHMEAL","PGNUTS","PHIDE","PIORECR","PLAMB",
                                  "PLEAD","PLOGORE","PLOGSK","PMAIZMT","PNGASEU","PNGASJP",
                                  "PNGASUS","PNICK","POILAPSP...45","POILBRE","POILDUB","POILWTI",
                                  "POLVOIL","PORANG","PPOIL","PPORK","PPOULT","PRICENPQ",
                                  "PRUBB","PSALM","PSAWMAL","PSAWORE","PSHRI","PSMEA",
                                  "PSOIL","PSOYB","PSUGAISA","PSUGAUSA","PSUNO","PTEA",
                                  "PTIN","PURAN","PWHEAMT","POATS","PSORG","PWOOLC",
                                  "PWOOLF","PZINC","PLMMODY","PCOBA","PGOLD","PSILVER",
                                  "PPALLA","PPLAT","PPROPANE","PUREA","PPOTASH","PDAP",
                                  "PTOMATO","PMILK","PCHANA","PAPPLE")) %>% 
  rename(POILAPSP45 = `POILAPSP...45`,
         POILAPSP17 = `POILAPSP...17`) %>% 
  filter(date >= "2008-01-01") %>% 
  dplyr::select(-c("PLMMODY"))

df_commodityprice <- data.frame(date = df_commodityprice$date,
                                sapply(df_commodityprice[,-1], function(x) as.numeric(as.character(x))))

lags <- c(1:24)
lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                   sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

# Apply this to all the functions we have created  
df_commodityprice_lags <- df_commodityprice %>% 
  arrange(date) %>% 
  mutate_at(vars("PALLFNF","PEXGALL","PNFUEL","PFANDB","PFOOD",
                 "PBEVE","PINDU","PAGRI","PRAWM","PALLMETA","PMETA",
                 "PPMETA","PEXGMETA","PFERT","PNRG","POILAPSP17","PNGAS",
                 "PCOAL","PALUM","PBANSOP","PBARL","PBEEF","PCOALAU",
                 "PCOALSA_USD","PCOCO","PCOFFOTM","PCOFFROB","PROIL","PCOPP",
                 "PCOTTIND","PFSHMEAL","PGNUTS","PHIDE","PIORECR","PLAMB",
                 "PLEAD","PLOGORE","PLOGSK","PMAIZMT","PNGASEU","PNGASJP",
                 "PNGASUS","PNICK","POILAPSP45","POILBRE","POILDUB","POILWTI",
                 "POLVOIL","PORANG","PPOIL","PPORK","PPOULT","PRICENPQ",
                 "PRUBB","PSALM","PSAWMAL","PSAWORE","PSHRI","PSMEA",
                 "PSOIL","PSOYB","PSUGAISA","PSUGAUSA","PSUNO","PTEA",
                 "PTIN","PURAN","PWHEAMT","POATS","PSORG","PWOOLC",
                 "PWOOLF","PZINC","PCOBA","PGOLD","PSILVER",
                 "PPALLA","PPLAT","PPROPANE","PUREA","PPOTASH","PDAP",
                 "PTOMATO","PMILK","PCHANA","PAPPLE"), funs_(lag_functions))

rm(df_commodityprice, df_imf)
