outdate<-function(data,days){
  # This function deletes the gas stations that did not report 
  # prices for more than a specified nuber of days
  
  da<-data
  da <- da %>%  mutate(aux = as.Date(cutoff_datetime)-date) 
  stations <- unique(filter(da,aux > days)$code)
  da <- filter(da,!(code %in% stations)) %>% select(-aux)
  return(da)
}