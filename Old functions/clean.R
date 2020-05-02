clean<-function(listdf,gasdf){
  #
  # This function applies several functions to each of the price databases stored in
  # the input list, first it filters observations for Torreon and Gomez Palacio
  # municipalities, then it removes gas stations that did not report for more than 30 days (outdate)
  # then it removes outliers(bchange1). Finally, it merges price data with gas station data.
  #
  # INPUT
  # listdf = list with 3 price databases corresponding to the three products analyzed: regular,
  # premium and diesel
  # gasdf = dataframe of gas stations with characteristics
  #
  # OUTPUT
  # l = list with 3 dataframes
  
  limpieza <- function(pricedf,gasdf){
    
    dataf <- bchange(dataf) # remove outliers
    gasdf <- select(gasdf,-c('latitude','longitude'))
    dataf <- merge(dataf,gasdf,by = c('code','muni_name')) #merge
    
    return(dataf)
    
  }
  
  l <- map(listdf,limpieza,gasdf)
  
  return(l)
  
}