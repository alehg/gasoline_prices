limpieza <- function(producto,df){
  
  # This function remove the stations that did not report any price,
  # also removes the ones that did not report a price change for more than 90 days
  # 
  # Inputs.- Price dataframe.
  # Outputs.- Price dataframe without non-reporting stations.
  #
  # ----------------------------------------------------
  # Alejandro Hernandez Gonzalez 2020
  #-----------------------------------------------------
  
  
  # First I will  drop the stations with 808 missing values,
  # which means they did not report any prices
  df %<>% filter(product == producto)
  df_mv <- df %>% filter(is.na(price_end))
  mv_by_station <- df_mv %>% # missing values by station
    group_by(code) %>% summarise(nas = n()) %>% ungroup()
  missing_stations <- mv_by_station %>% # non-existing stations
    filter(nas == 808) %>% pull(code)
  df_clean0 <- df %>% filter(!(code %in% missing_stations)) #no non-existing stations
  
  # Now I want to se which stations did not report a price change for more than 100 days (approx 3 months)
  non_reporting <- function(station,df,threshold){
    
    df1 <- df %>% filter(code == station,!is.na(price_end)) %>% 
      arrange(date) %>%  mutate(rlid = data.table::rleid(price_end)) %>% 
      group_by(rlid) %>% #create a group each time a price changes
      mutate(non_reporting_days = 1:n()) %>% ungroup() #count the number of days wihtout a change
    
    if(max(df1$non_reporting_days) > threshold){
      return(station)
    }else{
      return('')
    }
  }
  
  # threshold set to 100 days
  nr_code <- map_chr(df_clean0 %>% pull(code) %>% unique(),non_reporting,df_clean0,100) %>% 
    setdiff('')
  
  df_clean <- df_clean0 %>% filter(!(code %in% nr_code)) #no non-reporting stations
  
  
}
