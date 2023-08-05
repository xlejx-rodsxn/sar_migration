# Conflict indicators ACLED
acled_df <- read.csv(file="acled_someregions_00_20.csv")

#selecting africa
acled_africa_df <- filter(acled_df, region %in% c('Eastern Africa','Middle Africa','Northern Africa', 'Southern Africa', 'Western Africa'))

acled_africa_df <- acled_africa_df %>% 
  mutate(date_day = dmy(event_date),
         date = ym(paste0(year(date_day),"-",month(date_day))))

#computing number of events per month date
#battles
acled_africa_df_battles <- acled_africa_df %>% 
  filter(event_type == "Battles") %>% 
  group_by(country, date) %>% 
  summarise(num_battles = n())

acled_africa_df_battles_wide <- acled_africa_df_battles %>% 
  pivot_wider(id_cols = "date",
              names_from = c("country"),
              names_prefix = "num_battles_",
              values_from = "num_battles")
acled_africa_df_battles_wide <- acled_africa_df_battles_wide %>% replace(is.na(.), 0)

#explosions and remote violence
acled_africa_df_expvio <- acled_africa_df %>% 
  filter(event_type == "Explosions/Remote violence") %>% 
  group_by(country, date) %>% 
  summarise(num_expvio = n())

acled_africa_df_expvio_wide <- acled_africa_df_expvio %>% 
  pivot_wider(id_cols = "date",
              names_from = c("country"),
              names_prefix = "num_expvio_",
              values_from = "num_expvio")
acled_africa_df_expvio_wide <- acled_africa_df_expvio_wide %>% replace(is.na(.), 0)

#protests
acled_africa_df_protest <- acled_africa_df %>% 
  filter(event_type == "Protests") %>% 
  group_by(country, date) %>% 
  summarise(num_protest = n())

acled_africa_df_protest_wide <- acled_africa_df_protest %>% 
  pivot_wider(id_cols = "date",
              names_from = c("country"),
              names_prefix = "num_protest_",
              values_from = "num_protest")
acled_africa_df_protest_wide <- acled_africa_df_protest_wide %>% replace(is.na(.), 0)

#riots
acled_africa_df_riots <- acled_africa_df %>% 
  filter(event_type == "Riots") %>% 
  group_by(country, date) %>% 
  summarise(num_riots = n())

acled_africa_df_riots_wide <- acled_africa_df_riots %>% 
  pivot_wider(id_cols = "date",
              names_from = c("country"),
              names_prefix = "num_riots_",
              values_from = "num_riots")
acled_africa_df_riots_wide <- acled_africa_df_riots_wide %>% replace(is.na(.), 0)

#strategic developments
acled_africa_df_strdev <- acled_africa_df %>% 
  filter(event_type == "Strategic developments") %>% 
  group_by(country, date) %>% 
  summarise(num_strdev= n())

acled_africa_df_strdev_wide <- acled_africa_df_strdev %>% 
  pivot_wider(id_cols = "date",
              names_from = c("country"),
              names_prefix = "num_strdev_",
              values_from = "num_strdev")
acled_africa_df_strdev_wide <- acled_africa_df_strdev_wide %>% replace(is.na(.), 0)

#violence against civilians
acled_africa_df_violciv <- acled_africa_df %>% 
  filter(event_type == "Violence against civilians") %>% 
  group_by(country, date) %>% 
  summarise(num_violciv = n())

acled_africa_df_violciv_wide <- acled_africa_df_violciv %>% 
  pivot_wider(id_cols = "date",
              names_from = c("country"),
              names_prefix = "num_violciv_",
              values_from = "num_violciv")
acled_africa_df_violciv_wide <- acled_africa_df_violciv_wide %>% replace(is.na(.), 0)

acled_africa_df_all <- left_join(acled_africa_df_battles_wide, acled_africa_df_expvio_wide, by="date")
acled_africa_df_all <- left_join(acled_africa_df_all, acled_africa_df_protest_wide, by="date")
acled_africa_df_all <- left_join(acled_africa_df_all, acled_africa_df_riots_wide, by="date")
acled_africa_df_all <- left_join(acled_africa_df_all, acled_africa_df_strdev_wide, by="date")
acled_africa_df_all <- left_join(acled_africa_df_all, acled_africa_df_violciv_wide, by="date")

df_acled_africa_df_all_lags <- acled_africa_df_all %>% 
  mutate_at(vars("num_battles_Algeria",
                 "num_battles_Angola","num_battles_Benin",                       
                 "num_battles_Botswana","num_battles_Burkina Faso",                
                 "num_battles_Burundi","num_battles_Cameroon",                    
                 "num_battles_Central African Republic","num_battles_Chad",                        
                 "num_battles_Democratic Republic of Congo","num_battles_Djibouti",                    
                 "num_battles_Egypt","num_battles_Equatorial Guinea",           
                 "num_battles_Eritrea","num_battles_eSwatini",                    
                 "num_battles_Ethiopia","num_battles_Gabon",                       
                 "num_battles_Gambia","num_battles_Ghana",                       
                 "num_battles_Guinea","num_battles_Guinea-Bissau",               
                 "num_battles_Ivory Coast","num_battles_Kenya",                       
                 "num_battles_Lesotho","num_battles_Liberia",                    
                 "num_battles_Libya","num_battles_Madagascar",                  
                 "num_battles_Malawi","num_battles_Mali",                        
                 "num_battles_Mauritania","num_battles_Morocco",                     
                 "num_battles_Mozambique","num_battles_Namibia",                     
                 "num_battles_Niger","num_battles_Nigeria",                     
                 "num_battles_Republic of Congo","num_battles_Rwanda",                      
                 "num_battles_Senegal","num_battles_Sierra Leone",                
                 "num_battles_Somalia","num_battles_South Africa",                
                 "num_battles_South Sudan","num_battles_Sudan",                       
                 "num_battles_Tanzania","num_battles_Togo",                        
                 "num_battles_Tunisia","num_battles_Uganda",                      
                 "num_battles_Zambia","num_battles_Zimbabwe",                    
                 "num_expvio_Algeria","num_expvio_Angola",                       
                 "num_expvio_Burkina Faso","num_expvio_Burundi",                      
                 "num_expvio_Cameroon","num_expvio_Central African Republic",     
                 "num_expvio_Chad","num_expvio_Democratic Republic of Congo", 
                 "num_expvio_Djibouti","num_expvio_Egypt",                        
                 "num_expvio_Eritrea","num_expvio_eSwatini",                     
                 "num_expvio_Ethiopia","num_expvio_Gambia",
                 "num_expvio_Guinea","num_expvio_Guinea-Bissau",                
                 "num_expvio_Ivory Coast","num_expvio_Kenya",                        
                 "num_expvio_Liberia","num_expvio_Libya",                        
                 "num_expvio_Madagascar","num_expvio_Malawi",                       
                 "num_expvio_Mali","num_expvio_Mauritania",                   
                 "num_expvio_Morocco","num_expvio_Mozambique",                   
                 "num_expvio_Namibia","num_expvio_Niger",                        
                 "num_expvio_Nigeria","num_expvio_Republic of Congo",            
                 "num_expvio_Rwanda","num_expvio_Senegal",                      
                 "num_expvio_Sierra Leone","num_expvio_Somalia",                      
                 "num_expvio_South Africa","num_expvio_South Sudan",                  
                 "num_expvio_Sudan","num_expvio_Tanzania",                     
                 "num_expvio_Tunisia","num_expvio_Uganda",                       
                 "num_expvio_Zambia","num_expvio_Zimbabwe",                     
                 "num_protest_Algeria","num_protest_Angola",                      
                 "num_protest_Benin","num_protest_Botswana",                    
                 "num_protest_Burkina Faso","num_protest_Burundi",                     
                 "num_protest_Cameroon","num_protest_Central African Republic",    
                 "num_protest_Chad","num_protest_Democratic Republic of Congo",
                 "num_protest_Djibouti","num_protest_Egypt",                       
                 "num_protest_Equatorial Guinea","num_protest_Eritrea",                     
                 "num_protest_eSwatini","num_protest_Ethiopia",                    
                 "num_protest_Gabon","num_protest_Gambia",                     
                 "num_protest_Ghana","num_protest_Guinea",                      
                 "num_protest_Guinea-Bissau","num_protest_Ivory Coast",                 
                 "num_protest_Kenya","num_protest_Lesotho",                     
                 "num_protest_Liberia","num_protest_Libya",                       
                 "num_protest_Madagascar","num_protest_Malawi",                      
                 "num_protest_Mali","num_protest_Mauritania",                  
                 "num_protest_Morocco","num_protest_Mozambique",                  
                 "num_protest_Namibia","num_protest_Niger",                       
                 "num_protest_Nigeria","num_protest_Republic of Congo",           
                 "num_protest_Rwanda","num_protest_Senegal",                     
                 "num_protest_Sierra Leone","num_protest_Somalia",                     
                 "num_protest_South Africa","num_protest_South Sudan",                 
                 "num_protest_Sudan","num_protest_Tanzania",                    
                 "num_protest_Togo","num_protest_Tunisia",                     
                 "num_protest_Uganda","num_protest_Zambia",                      
                 "num_protest_Zimbabwe","num_riots_Algeria",                       
                 "num_riots_Angola","num_riots_Benin",                         
                 "num_riots_Botswana","num_riots_Burkina Faso",                  
                 "num_riots_Burundi","num_riots_Cameroon",                      
                 "num_riots_Central African Republic","num_riots_Chad",                          
                 "num_riots_Democratic Republic of Congo","num_riots_Djibouti",                      
                 "num_riots_Egypt","num_riots_Equatorial Guinea",             
                 "num_riots_Eritrea","num_riots_eSwatini",                      
                 "num_riots_Ethiopia","num_riots_Gabon",                         
                 "num_riots_Gambia","num_riots_Ghana",                         
                 "num_riots_Guinea","num_riots_Guinea-Bissau",                 
                 "num_riots_Ivory Coast","num_riots_Kenya",                         
                 "num_riots_Lesotho","num_riots_Liberia",                       
                 "num_riots_Libya","num_riots_Madagascar",                    
                 "num_riots_Malawi","num_riots_Mali",                          
                 "num_riots_Mauritania","num_riots_Morocco",                       
                 "num_riots_Mozambique","num_riots_Namibia",                       
                 "num_riots_Niger","num_riots_Nigeria",                       
                 "num_riots_Republic of Congo","num_riots_Rwanda",                        
                 "num_riots_Senegal","num_riots_Sierra Leone",                  
                 "num_riots_Somalia","num_riots_South Africa",                  
                 "num_riots_South Sudan","num_riots_Sudan",                         
                 "num_riots_Tanzania","num_riots_Togo",                          
                 "num_riots_Tunisia","num_riots_Uganda",                        
                 "num_riots_Zambia","num_riots_Zimbabwe",                      
                 "num_strdev_Algeria","num_strdev_Angola",                       
                 "num_strdev_Benin","num_strdev_Botswana",                     
                 "num_strdev_Burkina Faso","num_strdev_Burundi",                      
                 "num_strdev_Cameroon","num_strdev_Central African Republic",     
                 "num_strdev_Chad","num_strdev_Democratic Republic of Congo", 
                 "num_strdev_Djibouti","num_strdev_Egypt",                        
                 "num_strdev_Equatorial Guinea","num_strdev_Eritrea",                      
                 "num_strdev_eSwatini","num_strdev_Ethiopia",                     
                 "num_strdev_Gabon","num_strdev_Gambia",                       
                 "num_strdev_Ghana","num_strdev_Guinea",                       
                 "num_strdev_Guinea-Bissau","num_strdev_Ivory Coast",                  
                 "num_strdev_Kenya","num_strdev_Lesotho",                      
                 "num_strdev_Liberia","num_strdev_Libya",                        
                 "num_strdev_Madagascar","num_strdev_Malawi",                       
                 "num_strdev_Mali","num_strdev_Mauritania",                   
                 "num_strdev_Morocco","num_strdev_Mozambique",                   
                 "num_strdev_Namibia","num_strdev_Niger",                        
                 "num_strdev_Nigeria","num_strdev_Republic of Congo",            
                 "num_strdev_Rwanda","num_strdev_Senegal",                      
                 "num_strdev_Sierra Leone","num_strdev_Somalia",                      
                 "num_strdev_South Africa","num_strdev_South Sudan",                  
                 "num_strdev_Sudan","num_strdev_Tanzania",                     
                 "num_strdev_Togo","num_strdev_Tunisia",                      
                 "num_strdev_Uganda","num_strdev_Zambia",                       
                 "num_strdev_Zimbabwe","num_violciv_Algeria",                     
                 "num_violciv_Angola","num_violciv_Benin",                       
                 "num_violciv_Botswana","num_violciv_Burkina Faso",                
                 "num_violciv_Burundi","num_violciv_Cameroon",                    
                 "num_violciv_Central African Republic","num_violciv_Chad",                        
                 "num_violciv_Democratic Republic of Congo","num_violciv_Djibouti",                    
                 "num_violciv_Egypt","num_violciv_Equatorial Guinea",           
                 "num_violciv_Eritrea","num_violciv_eSwatini",                    
                 "num_violciv_Ethiopia","num_violciv_Gabon",                       
                 "num_violciv_Gambia","num_violciv_Ghana",                       
                 "num_violciv_Guinea","num_violciv_Guinea-Bissau",               
                 "num_violciv_Ivory Coast","num_violciv_Kenya",                       
                 "num_violciv_Lesotho","num_violciv_Liberia",                     
                 "num_violciv_Libya","num_violciv_Madagascar",                  
                 "num_violciv_Malawi","num_violciv_Mali",                        
                 "num_violciv_Mauritania","num_violciv_Morocco",                     
                 "num_violciv_Mozambique","num_violciv_Namibia",                     
                 "num_violciv_Niger","num_violciv_Nigeria",                     
                 "num_violciv_Republic of Congo","num_violciv_Rwanda",                      
                 "num_violciv_Senegal","num_violciv_Sierra Leone",                
                 "num_violciv_Somalia","num_violciv_South Africa",                
                 "num_violciv_South Sudan","num_violciv_Sudan",                       
                 "num_violciv_Tanzania","num_violciv_Togo",                       
                 "num_violciv_Tunisia","num_violciv_Uganda",                      
                 "num_violciv_Zambia","num_violciv_Zimbabwe"), funs_(lag_functions))

rm(acled_df,
   acled_africa_df,acled_africa_df_all,
   acled_africa_df_battles,acled_africa_df_battles_wide,
   acled_africa_df_expvio,acled_africa_df_expvio_wide,
   acled_africa_df_protest,acled_africa_df_protest_wide,
   acled_africa_df_riots,acled_africa_df_riots_wide,
   acled_africa_df_strdev,acled_africa_df_strdev_wide,
   acled_africa_df_violciv,acled_africa_df_violciv_wide)
