# This code will be used to clean and build the main dataframe for analysis
# Alejandro Hernandez 2019
#-----------------------------

# Clear workspace
rm(list=ls())

# ============================================================================
# CONSTANTS
# ============================================================================
DEREGULATION_DATE <- as.Date('2017-06-15')  # Date when gasoline market was deregulated
TOTAL_DAYS <- 808  # Total number of days in the dataset
NON_REPORTING_THRESHOLD <- 100  # Days without price change to consider station non-reporting
COMPETITOR_RADIUS <- 2000  # Radius in meters for competitor density calculation

# ============================================================================
# LOAD REQUIRED PACKAGES
# ============================================================================
required_packages <- c('tidyverse','openxlsx','magrittr','lubridate','geosphere')
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) {
  stop(paste("Missing required packages:", paste(missing_packages, collapse=", ")))
}
lapply(required_packages, library, character.only = TRUE)

# ============================================================================
# LOAD CUSTOM FUNCTIONS
# ============================================================================
# Use file.path for cross-platform compatibility
functions_dir <- file.path(getwd(), 'Functions')
if(!dir.exists(functions_dir)) {
  stop(paste("Functions directory not found at:", functions_dir))
}
function_files <- list.files(functions_dir, pattern = "\\.R$", full.names = TRUE)
if(length(function_files) == 0) {
  stop("No R function files found in Functions directory")
}
lapply(function_files, source)

# ============================================================================
# LOAD RAW DATA
# ============================================================================
data_dir <- file.path(getwd(), 'data')
prices_file <- file.path(data_dir, 'laguna_prices.rds')
if(!file.exists(prices_file)) {
  stop(paste("Price data file not found at:", prices_file))
}
prices_raw <- readRDS(prices_file)

# ============================================================================
# FILTER DATA TO POST-DEREGULATION PERIOD
# ============================================================================
# We are only interested in prices after deregulation (June 15, 2017)
# This ensures we analyze the competitive market period
prices0 <- prices_raw %>% 
  filter(date >= DEREGULATION_DATE)

# Verify we have the expected number of days
if(n_distinct(prices0$date) != TOTAL_DAYS) {
  warning(paste("Expected", TOTAL_DAYS, "days but found", n_distinct(prices0$date)))
}

# ============================================================================
# REMOVE NON-REPORTING STATIONS
# ============================================================================
# Apply cleaning function to each product type separately
# This removes stations that never reported prices or had extended periods without price changes
prices1 <- map_dfr(unique(prices0$product), limpieza, prices0)

# ============================================================================
# IDENTIFY STATION ENTRY DATES
# ============================================================================
# After a station reported a price, there are no missing values afterwards.
# If there are missing values before the first reported price, we assume 
# the station did not sell that product at that time (i.e., entered later).
# 
# We need entry dates to recalculate competitor density when new stations enter,
# as the competitive environment changes with each new entrant.

entry <- function(producto, df){
  # Identifies stations that entered the market after deregulation and their entry dates.
  #
  # Args:
  #   producto: Product type (regular, premium, diesel)
  #   df: Price dataframe
  #
  # Returns:
  #   List with two elements:
  #     - new_stations: Vector of station codes that entered after deregulation
  #     - fechas: Vector of entry dates (as character strings)
  df_clean <- df %>% filter(product == producto)
  
  # Find stations with missing prices (likely entered after deregulation)
  new_stations <- df_clean %>% 
    filter(is.na(price_end)) %>% 
    pull(code) %>% 
    unique()
  
  # Extract the first date each new station reported a price
  first_date <- function(station, df){
    # Extracts the date when a station first entered the market (first non-NA price).
    nona_df <- df %>% 
      filter(code == station, !is.na(price_end))
    
    if(nrow(nona_df) == 0) {
      warning(paste("Station", station, "has no reported prices"))
      return(NA_character_)
    }
    
    fecha <- nona_df %>% 
      filter(date == min(date)) %>% 
      pull(date) %>% 
      unique() %>% 
      as.character.Date()
    
    return(fecha)
  }
  
  # Get entry dates for all new stations
  fechas <- map_chr(new_stations, first_date, df_clean) %>% 
    unique() %>% 
    na.omit()  # Remove any NA entries
  
  return(list(new_stations, fechas))
}

# Calculate entry dates for each product type
entry_dates <- map(unique(prices1$product), entry, prices1)
names(entry_dates) <- unique(prices1$product)

# ============================================================================
# LOAD STATION CHARACTERISTICS DATA
# ============================================================================
# Load station characteristics from Excel files (originally from Google Maps)
# These files contain: brand, location (lat/lon), road type, amenities, etc.

gasot_file <- file.path(data_dir, 'gast.xlsx')
gasogp_file <- file.path(data_dir, 'gasgp.xlsx')

if(!file.exists(gasot_file)) {
  stop(paste("Torreon station data not found at:", gasot_file))
}
if(!file.exists(gasogp_file)) {
  stop(paste("Gomez Palacio station data not found at:", gasogp_file))
}

# Load and combine station data from both municipalities
gasot <- read.xlsx(gasot_file, 1) %>% 
  mutate(muni_name = 'Torreon') 

gasogp <- read.xlsx(gasogp_file, 1) %>% 
  mutate(muni_name = 'Gomez Palacio') 

# Combine and standardize station characteristics
# - Standardize brand names (Mi Gasolina special case)
# - Standardize road types (periferico -> highway)
# - Ensure proper data types for coordinates and codes
gasocl <- bind_rows(gasot, gasogp) %>% 
  mutate(
    # Handle special case: Mi Gasolina brand indicator
    Brand = ifelse(Migasolina == 1, 'migasolina', trimws(Brand)),
    # Standardize road type: periferico (ring road) is treated as highway
    Road.Type = ifelse(Road.Type == 'periferico', 'highway', as.character(Road.Type)),
    # Ensure coordinates are numeric (may come as character from Excel)
    latitude = as.numeric(as.character(latitude)),
    longitude = as.numeric(as.character(longitude)),
    # Ensure station code is character for consistent joining
    code = as.character(code)
  ) %>% 
  # Only keep stations that appear in price data
  filter(code %in% prices1$code)

# Clean up temporary dataframes
rm(gasot, gasogp)

# ============================================================================
# COMPUTE DISTANCES BETWEEN ALL STATION PAIRS
# ============================================================================
# Create a dataframe with all unique pairs of stations and calculate
# the geographic distance between them. This is needed to compute
# competitor density within a specified radius.

# Generate all unique pairs of stations (n choose 2)
pairs_raw <- t(combn(gasocl$code, 2)) %>% 
  as_tibble() %>% 
  rename(code1 = V1, code2 = V2)

# Join station characteristics (coordinates and brand) for first station in pair
pairs0 <- left_join(
  pairs_raw %>% select(code1 = code1), 
  gasocl %>% select(code, latitude, longitude, Brand), 
  by = c('code1' = 'code')
) %>% 
  rename(lat1 = latitude, lon1 = longitude, brand1 = Brand)

# Join station characteristics for second station in pair
pairs1 <- left_join(
  pairs0,
  gasocl %>% select(code, latitude, longitude, Brand),
  by = c('code2' = 'code')
) %>% 
  rename(lat2 = latitude, lon2 = longitude, brand2 = Brand)

# Calculate great-circle distance between station pairs using Haversine formula
# distGeo returns distance in meters
distancias <- pairs1 %>% 
  rowwise() %>% 
  mutate(dist = distGeo(c(lon1, lat1), c(lon2, lat2))) %>% 
  ungroup()

# Clean up intermediate dataframes
rm(pairs_raw, pairs0, pairs1)

# ============================================================================
# COMPUTE COMPETITOR DENSITY
# ============================================================================
# For each product type and each station-date combination, compute:
# - density: Number of competitors within COMPETITOR_RADIUS meters
# - same_brand: Number of competitors with the same brand
# - min_dist: Distance to nearest competitor
#
# Density is recalculated whenever a new station enters the market,
# as this changes the competitive environment.

prices2 <- map_dfr(
  unique(prices1$product),
  densidad,
  prices1,
  distancias,
  entry_dates,
  radius = COMPETITOR_RADIUS
)

# ============================================================================
# OUTLIER CHECK (COMMENTED OUT - FOR REFERENCE)
# ============================================================================
# Outlier detection code (commented out but kept for reference):
# - Computes mean price per day-product combination
# - Identifies prices deviating more than 0.6 pesos from mean
# - Found ~2500 outliers out of 240,000 observations (1%)
# - No deviations exceeded 1 peso, so outliers were kept in analysis
#
# prices_check <- prices2 %>% 
#   group_by(date, product) %>% 
#   mutate(meanPrice = mean(price_end, na.rm = TRUE),
#          dev = price_end - meanPrice) %>% 
#   ungroup() 
# outliers <- prices_check %>% 
#   filter(abs(dev) > 0.6)

# ============================================================================
# FINAL DATA CLEANING AND VARIABLE CREATION
# ============================================================================
clean_prices <- prices2 %>% 
  # Remove observations with missing prices (stations not selling product)
  filter(!is.na(price_end)) %>% 
  # Merge with station characteristics
  left_join(gasocl %>% select(-c('latitude', 'longitude')), by = 'code') %>% 
  # Create additional variables:
  # - same_brand_share: Proportion of competitors with same brand (0 if no competitors)
  # - year: Extract year from date for time-period analysis
  # - sd: Standard deviation of prices across stations for each product-date
  mutate(
    same_brand_share = if_else(density != 0, same_brand / density, 0),
    year = year(date)
  ) %>% 
  # Calculate cross-sectional price dispersion (standard deviation) for each product-date
  group_by(product, date) %>% 
  mutate(sd = sd(price_end, na.rm = TRUE)) %>% 
  ungroup()

# ============================================================================
# SAVE CLEANED DATA
# ============================================================================
output_file <- file.path(data_dir, 'clean_prices.RData')
save(clean_prices, prices_raw, distancias, gasocl, entry_dates, file = output_file)
cat("Cleaned data saved to:", output_file, "\n") 

