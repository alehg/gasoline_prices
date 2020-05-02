varianza <- function(priced,v){
  
  # Given a gas stations data set and its corresponding price data set , this function for each station
  # at a given time computes the mean and variance of the station's relevant market (stations within a
  # 2km. radius)
  
  funcionc <- function(i,pdf,ne){
    n <- pluck(ne,i,2) #get neighbors from neighbors list
    c <- pluck(ne,i,1) #code
    if(is.null(n)){ #if there are no neighbors 
      dataf <- filter(pdf,code  == c) %>% 
        select(code,cutoff_datetime,mean_price = price,mean_uhat = uhat) %>% #mean_price = price
        mutate(psd = 0)
      
    }else{
      aux <- pdf %>% 
        filter(code %in% n) %>%  #filter neighbors
        group_by(cutoff_datetime) %>% #for each date
        summarise(mean_price = mean(price), #calculate avg. price
                  mean_uhat = mean(uhat), #calculate avg. residuals
                  psd = sd(price)) %>%
        ungroup() 
      dataf <- data.frame(code = c,aux) #data frame with code, date, and 
      #average price and residuals of neighbors
    }
    
    return(dataf)
  }
  auxdf <- map_dfr(1:length(v),funcionc,priced,v)
  df <- left_join(priced,auxdf,by = c('code','cutoff_datetime'))
  return(df)
}