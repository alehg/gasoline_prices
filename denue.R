# DENUE Data Enrichment Script
# This script enriches station data with neighborhood characteristics from INEGI DENUE
# (National Directory of Economic Units) using the INEGI API

# ============================================================================
# LOAD REQUIRED PACKAGES
# ============================================================================
if(!require(inegiR)) {
  stop("Package 'inegiR' is required. Install with: install.packages('inegiR')")
}
library(inegiR)

if(!require(tidyverse)) {
  stop("Package 'tidyverse' is required")
}
library(tidyverse)

if(!require(openxlsx)) {
  stop("Package 'openxlsx' is required")
}
library(openxlsx)

# ============================================================================
# API CONFIGURATION
# ============================================================================
# IMPORTANT: Store your INEGI API token as an environment variable for security
# Set it before running: Sys.setenv(INEGI_TOKEN = "your-token-here")
# Or create a .Renviron file with: INEGI_TOKEN=your-token-here

token <- Sys.getenv("INEGI_TOKEN")
if(token == "") {
  # Fallback: Check for hardcoded token (NOT RECOMMENDED for production)
  # Remove this fallback and use environment variable instead
  warning("INEGI_TOKEN environment variable not set. Using fallback token (not recommended).")
  token <- "90ba6ba7-5d92-43e9-bfca-bad73c65d4e1"
}

if(is.null(token) || token == "") {
  stop("INEGI API token is required. Set INEGI_TOKEN environment variable.")
}

# ============================================================================
# LOAD STATION DATA
# ============================================================================
data_dir <- file.path(getwd(), 'data')
gasot_file <- file.path(data_dir, 'gast.xlsx')
gasogp_file <- file.path(data_dir, 'gasgp.xlsx')

if(!file.exists(gasot_file)) {
  stop(paste("Torreon station data not found at:", gasot_file))
}
if(!file.exists(gasogp_file)) {
  stop(paste("Gomez Palacio station data not found at:", gasogp_file))
}

# Load station data from Excel files (originally from Google Maps)
gasot <- read.xlsx(gasot_file, 1) %>% 
  mutate(muni_name = 'Torreon') 

gasogp <- read.xlsx(gasogp_file, 1) %>% 
  mutate(muni_name = 'Gomez Palacio') 

# Combine and standardize station characteristics
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
  )

# Note: prices0 should be loaded from previous script or data file
# If not available, filter will be skipped
if(exists("prices0")) {
  gasocl <- gasocl %>% filter(code %in% prices0$code)
} else {
  warning("prices0 not found. Proceeding with all stations in gasocl.")
}

# Clean up temporary dataframes
rm(gasot, gasogp)

# ============================================================================
# FETCH DENUE DATA FROM INEGI API
# ============================================================================
# DENUE provides information about establishments (businesses) in a given area
# We query for establishments within 2000 meters of each gas station

DENUE_RADIUS <- 2000  # Radius in meters for DENUE queries

itera_denue <- function(latitud, longitud){
  # Queries INEGI DENUE API for establishments near a given location.
  #
  # Args:
  #   latitud: Latitude of gas station
  #   longitud: Longitude of gas station
  #
  # Returns:
  #   Dataframe with establishment data (may be empty if no establishments found)
  
  tryCatch({
    inegi_denue(latitud, longitud, token, meters = DENUE_RADIUS)
  }, error = function(e) {
    warning(paste("Error querying DENUE for lat:", latitud, "lon:", longitud, "-", e$message))
    return(data.frame())  # Return empty dataframe on error
  })
}

cat("Querying INEGI DENUE API for", nrow(gasocl), "stations...\n")
cat("This may take several minutes...\n")

# Query DENUE for each station (this can be slow - consider parallelization for large datasets)
denue_est <- map2(gasocl$latitude, gasocl$longitude, itera_denue)

# Extract number of establishments and most common postal code for each station
denue_num <- map(denue_est, nrow)
denue_zip <- map(denue_est, function(x) {
  if(nrow(x) > 0 && 'postal_code' %in% names(x)) {
    getmode(pull(x, 'postal_code'))
  } else {
    NA_character_
  }
})

# Combine DENUE data with station data
denue_col <- tibble(
  num_est = unlist(denue_num),  # Number of establishments within radius
  zip_code = unlist(denue_zip)  # Most common postal code in area
)

gasocl1 <- gasocl %>% bind_cols(denue_col)

# Merge DENUE data with price data (if prices0 exists)
if(exists("prices0")) {
  prices1 <- prices0 %>% 
    left_join(gasocl1 %>% select(code, num_est, zip_code), by = 'code')
} else {
  warning("prices0 not found. prices1 will not be created.")
  prices1 <- NULL
}

# ============================================================================
# SAVE RESULTS
# ============================================================================
output_file <- file.path(data_dir, 'denue.RData')
save(denue_est, gasocl1, prices1, file = output_file)
cat("DENUE data saved to:", output_file, "\n")
cat("Summary: Average establishments per station:", round(mean(denue_col$num_est, na.rm = TRUE), 2), "\n")
