limpieza <- function(pricedf,gasdf){
# This function removes outliers and joins prices dataframe with stations dataframe
  
  dataf <- bchange(pricedf) # remove outliers
  gasdf <- select(gasdf,-c('latitude','longitude'))
  dataf <- left_join(dataf,gasdf,by = c('code')) #merge
  
  return(dataf)
  
}