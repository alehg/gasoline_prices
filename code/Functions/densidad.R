densidad <- function(producto,df,dist_df,fechas_list, radius = 2000){
  
  # Given the price df, distance between stations, and new_stations list,
  # this function computes the number of competitors within a specific market (product) for
  # every station on every date
  #
  # Inputs.- Product, Price dataframe, Distance dataframe, entry_dates list for each product
  # Output.- Price dataframe with number of competitors for each code, date (density variable)
  # and the number of competitors with the same brand
  
  aux_df <- df %>% filter(product == producto) 
  # entry dates, for each of these I will recompute the number of competitors
  fechas <- pluck(fechas_list,producto,2) %>% 
    union(c(as.character.Date(min(aux_df$date)),as.character.Date(max(aux_df$date) + days(1)))) %>% 
    sort()
  
  density_product <- function(i,df,dist_df,fechas,radius){
    # create a partition of the dataframe to join with the number of competitors on each period
    aux <- df %>% filter(date >= fechas[i], date < fechas[i+1])
    # stations selling at that time
    aux_codes <- aux %>% filter(!is.na(price_end)) %>% pull(code) %>% unique()
    # we just want to keep pairs of stations that sell 
    dist_df %<>% filter(code1 %in% aux_codes, code2 %in% aux_codes)
    
    distancias <- function(codigo,aux,dist_df){
      # For each station I will keep all pairs where it appears and filter
      # so that the distance between stations is less than the radius
      code_df <- dist_df %>% filter(code1 == codigo | code2 == codigo, dist < radius)
      # filter if they have the same brand
      brand_df <- code_df %>% filter(brand1 == brand2)
      den <- nrow(code_df) #number of competitors within radius
      same <- nrow(brand_df) # number of competitors with same brand
      mdist <- ifelse(den == 0,NA,min(code_df$dist)) # distance to closest competitor
      tibble(code = codigo,density = den, same_brand = same, min_dist = mdist)
    }
    codes_df <- map_dfr(aux_codes,distancias,aux,dist_df)
    # join partitioned datagrame with 
    aux1 <- left_join(aux,codes_df,by = 'code')
    
  }
  final_df <- map_dfr(1:(length(fechas)-1),density_product,aux_df,dist_df,fechas,radius)

  
}