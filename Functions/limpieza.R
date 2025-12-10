limpieza <- function(producto, df, total_days = 808, non_reporting_threshold = 100){
  # Removes non-reporting stations from the price dataframe.
  #
  # This function removes stations that:
  #   1. Never reported any prices (all missing values)
  #   2. Did not report a price change for more than non_reporting_threshold days
  #
  # Args:
  #   producto: Product type to filter (regular, premium, diesel)
  #   df: Price dataframe with columns: product, code, date, price_end
  #   total_days: Expected total number of days in dataset (default: 808)
  #   non_reporting_threshold: Maximum days without price change before considering
  #                            station non-reporting (default: 100, ~3 months)
  #
  # Returns:
  #   Cleaned price dataframe without non-reporting stations
  #
  # Author: Alejandro Hernandez Gonzalez 2020
  
  # Filter to specific product
  df %<>% filter(product == producto)
  
  # ============================================================================
  # STEP 1: Remove stations that never reported any prices
  # ============================================================================
  # Count missing values per station
  df_mv <- df %>% filter(is.na(price_end))
  mv_by_station <- df_mv %>% 
    group_by(code) %>% 
    summarise(nas = n(), .groups = 'drop')
  
  # Identify stations with all missing values (never reported)
  missing_stations <- mv_by_station %>% 
    filter(nas == total_days) %>% 
    pull(code)
  
  if(length(missing_stations) > 0) {
    cat("Removing", length(missing_stations), "stations with no reported prices for", producto, "\n")
  }
  
  # Remove non-existing stations
  df_clean0 <- df %>% filter(!(code %in% missing_stations))
  
  # ============================================================================
  # STEP 2: Remove stations with extended periods without price changes
  # ============================================================================
  # A station that doesn't change prices for a long time may have stopped
  # reporting or may have closed. We identify these by finding the longest
  # consecutive period without a price change.
  
  non_reporting <- function(station, df, threshold){
    # Identifies if a station had an extended period without price changes.
    #
    # Args:
    #   station: Station code
    #   df: Price dataframe for this station
    #   threshold: Maximum days without price change
    
    # Filter to this station and remove missing prices
    df1 <- df %>% 
      filter(code == station, !is.na(price_end)) %>% 
      arrange(date)
    
    # If no reported prices, return station code (should be caught in step 1)
    if(nrow(df1) == 0) {
      return(station)
    }
    
    # Create run-length encoding groups: each group represents a period with constant price
    df1 <- df1 %>% 
      mutate(rlid = data.table::rleid(price_end)) %>% 
      group_by(rlid) %>% 
      mutate(non_reporting_days = n()) %>%  # Count days in each constant-price period
      ungroup()
    
    # Check if longest constant-price period exceeds threshold
    max_days <- max(df1$non_reporting_days, na.rm = TRUE)
    if(max_days > threshold) {
      return(station)
    } else {
      return('')
    }
  }
  
  # Apply non-reporting check to all stations
  unique_stations <- df_clean0 %>% pull(code) %>% unique()
  nr_code <- map_chr(unique_stations, non_reporting, df_clean0, non_reporting_threshold) %>% 
    setdiff('')  # Remove empty strings
  
  if(length(nr_code) > 0) {
    cat("Removing", length(nr_code), "stations with extended periods without price changes for", producto, "\n")
  }
  
  # Remove non-reporting stations
  df_clean <- df_clean0 %>% filter(!(code %in% nr_code))
  
  return(df_clean)
}
