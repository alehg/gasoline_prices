densidad <- function(producto, df, dist_df, fechas_list, radius = 2000){
  # Computes competitor density for each station-date combination within a product market.
  #
  # Given the price dataframe, distance between stations, and entry dates list,
  # this function computes the number of competitors within a specified radius
  # for every station on every date. Density is recalculated whenever a new 
  # station enters the market.
  #
  # Args:
  #   producto: Product type (regular, premium, diesel)
  #   df: Price dataframe with columns: product, code, date, price_end
  #   dist_df: Dataframe with distances between all station pairs (code1, code2, dist, brand1, brand2)
  #   fechas_list: Named list of entry dates for each product
  #   radius: Radius in meters for competitor density calculation (default: 2000)
  #
  # Returns:
  #   Dataframe with original price data plus:
  #     - density: Number of competitors within radius
  #     - same_brand: Number of competitors with same brand
  #     - min_dist: Distance to nearest competitor (NA if no competitors)
  
  # Filter to specific product
  aux_df <- df %>% filter(product == producto) 
  
  # Get entry dates for this product and add start/end dates to create time periods
  # Each period is between entry dates, so density can be recalculated when new stations enter
  fechas <- pluck(fechas_list, producto, 2) %>% 
    union(c(
      as.character.Date(min(aux_df$date)),
      as.character.Date(max(aux_df$date) + days(1))
    )) %>% 
    sort()
  
  # Function to compute density for a specific time period
  density_product <- function(i, df, dist_df, fechas, radius){
    # Create a partition of the dataframe for the time period [fechas[i], fechas[i+1])
    aux <- df %>% filter(date >= fechas[i], date < fechas[i+1])
    
    # Get list of stations that were selling during this period
    aux_codes <- aux %>% 
      filter(!is.na(price_end)) %>% 
      pull(code) %>% 
      unique()
    
    # Filter distance dataframe to only include pairs of stations that were both selling
    # This ensures we only count active competitors
    dist_df_filtered <- dist_df %>% 
      filter(code1 %in% aux_codes, code2 %in% aux_codes)
    
    # Function to compute density metrics for a single station
    distancias <- function(codigo, aux, dist_df_filtered){
      # Find all station pairs where this station appears and distance is within radius
      code_df <- dist_df_filtered %>% 
        filter((code1 == codigo | code2 == codigo), dist < radius)
      
      # Count competitors with same brand
      brand_df <- code_df %>% filter(brand1 == brand2)
      
      # Calculate metrics
      den <- nrow(code_df)  # Total number of competitors within radius
      same <- nrow(brand_df)  # Number of competitors with same brand
      # Distance to closest competitor (NA if no competitors)
      mdist <- ifelse(den == 0, NA, min(code_df$dist))
      
      tibble(code = codigo, density = den, same_brand = same, min_dist = mdist)
    }
    
    # Compute density for all stations in this period
    codes_df <- map_dfr(aux_codes, distancias, aux, dist_df_filtered)
    
    # Join density metrics back to price data for this period
    aux1 <- left_join(aux, codes_df, by = 'code')
    
    return(aux1)
  }
  
  # Apply density calculation to each time period and combine results
  final_df <- map_dfr(1:(length(fechas) - 1), density_product, aux_df, dist_df, fechas, radius)
  
  # Return the final dataframe with density metrics
  return(final_df)
}