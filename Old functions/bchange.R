bchange <- function(df){
  
  # given a dataset this function computes for each
  # day the deviation of prices from the mean, if it is 
  # greater than 0.8 it sets the price to be equal to that
  # price minus one, if it is lower than -0.8, it sets 
  # price = price + 1
  
   
  df %<>%    #computes each day's mean price and then filters by deviation
    group_by(date,product) %>% 
    mutate(meanPrice = mean(price_end),dev = price_end - meanPrice)  %>% 
    ungroup()
  
   aux <- df %>%  filter(dev > 0.8) 
  
  while(dim(aux)[1]>0){#while there are deviations higher than 0.8
  
    df %<>%  # sets price = price -1
      mutate(price_end = ifelse(dev > 0.8,price_end - 1,price_end))

    #computes each day's mean price and then filters by deviation
    df %<>% 
      group_by(date,product) %>% 
      mutate(meanPrice = mean(price_end),dev = price_end - meanPrice)  %>% 
      ungroup()
     aux <- df %>%  filter(dev > 0.8) 
  
  }
  
  aux <- df %>% filter(dev < -0.8) 
  
  while(dim(aux)[1]>0){ #while there are deviations lower than -0.8
  
    df <- df %>%  #set price = price + 1
      mutate(price_end = ifelse(dev < -0.8,price_end + 1,price_end))
  
    df %<>%   #computes mean price each day and filters by deviations
      group_by(date,product) %>% 
      mutate(meanPrice = mean(price_end),dev = price_end - meanPrice) %>% 
      ungroup()
    
    aux <- df %>% filter(dev < -0.8) 
  
  }

  df %<>% select(-c('dev','meanPrice'))
  return(df)
  
}
