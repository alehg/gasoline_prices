## Code to perform regression analysis 
# This script performs econometric analysis of gasoline prices:
# 1. Price level regressions (panel random effects)
# 2. Price dispersion regressions (Lewis method with PCSE)

rm(list=ls())

# ============================================================================
# CONSTANTS
# ============================================================================
ANALYSIS_YEARS <- 2017:2019  # Years for period-specific regressions
PRODUCTS <- c('regular', 'premium', 'diesel')  # Product types analyzed

# ============================================================================
# LOAD REQUIRED PACKAGES
# ============================================================================
required_packages <- c('tidyverse','openxlsx','geosphere','magrittr','plm',
                       'stats','utils','lubridate','corrplot','pcse', 'panelAR')
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) {
  stop(paste("Missing required packages:", paste(missing_packages, collapse=", ")))
}
lapply(required_packages, library, character.only = TRUE)

# ============================================================================
# LOAD CUSTOM FUNCTIONS
# ============================================================================
functions_dir <- file.path(getwd(), 'Functions')
if(!dir.exists(functions_dir)) {
  stop(paste("Functions directory not found at:", functions_dir))
}
function_files <- list.files(functions_dir, pattern = "\\.R$", full.names = TRUE)
lapply(function_files, source)

# ============================================================================
# LOAD DATA
# ============================================================================
# Load data with auxiliary regression residuals (from descriptive statistics script)
data_dir <- file.path(getwd(), 'data')
denue_file <- file.path(data_dir, 'denue.RData')
if(!file.exists(denue_file)) {
  stop(paste("DENUE data file not found at:", denue_file))
}
load(denue_file)

# Standardize road type: 'drive' should be 'avenue'
prices1 <- prices1 %>% 
  mutate(Road.Type = if_else(Road.Type == 'drive', 'avenue', Road.Type))
# ============================================================================
# DESCRIPTIVE: DISPERSION BY YEAR
# ============================================================================
# Quick summary of price dispersion (standard deviation of residuals) by product and year
dispersion_summary <- prices1 %>% 
  group_by(product, year) %>% 
  summarise(disp = sd(uhat, na.rm = TRUE), .groups = 'drop') %>% 
  pivot_wider(id_cols = product, 
              names_from = year, 
              values_from = disp)
print("Price Dispersion Summary:")
print(dispersion_summary)

# ============================================================================
# PRICE LEVEL REGRESSIONS
# ============================================================================
# Regression equation: Retail price as function of wholesale price, competition,
# station characteristics, and location factors
ecuacion <- price_end ~ price_term + log(density + 1) + 
  Brand + Convenience.Store + ATM + Car.Wash + zip_code + muni_name +
  Road.Type + same_brand_share + min_dist

# Function to run price level regressions for a product
pricereg <- function(producto, df){
  # Runs panel random effects regressions for price levels.
  #
  # Args:
  #   producto: Product type (regular, premium, diesel)
  #   df: Price dataframe with all required variables
  #
  # Returns:
  #   List containing:
  #     - Full period random effects model
  #     - Breusch-Godfrey serial correlation test
  #     - List of period-specific models (2017, 2018, 2019)
  
  df <- df %>% filter(product == producto)
  
  # Full period random effects model
  random <- plm(ecuacion,
                df,
                index = c('code', 'date'),
                model = 'random')
  
  # Serial Correlation test (Breusch-Godfrey)
  # Tests for autocorrelation in residuals, which would violate panel model assumptions
  bg_test <- pbgtest(random)
  
  # Period-specific regressions (2017, 2018, 2019)
  periodsreg <- function(periodo){
    # periodo: 1 = 2017, 2 = 2018, 3 = 2019
    year_filter <- ANALYSIS_YEARS[periodo]
    df_period <- df %>% filter(year == year_filter)
    reg <- plm(ecuacion, 
               df_period, 
               index = c('code', 'date'), 
               model = 'random')
    return(reg)
  }
  periodos <- map(1:length(ANALYSIS_YEARS), periodsreg)
  
  return(list(random, bg_test, periodos))
}

# Run regressions for all products
gasr <- unique(prices1$product)
cat("Running price level regressions...\n")
ptm <- proc.time()
reg_precios <- map(gasr, pricereg, prices1)
proc.time() - ptm
names(reg_precios) <- gasr

# Example: View summary with robust standard errors (SCC = Driscoll-Kraay)
# These account for cross-sectional and temporal correlation
cat("\nExample regression summary (Regular, Full Period, Robust SE):\n")
pluck(reg_precios, 'regular', 1) %>% summary(vcov = plm::vcovSCC(.))

# Save regression results
output_file <- file.path(data_dir, 'reg_precios.RDS')
saveRDS(reg_precios, file = output_file)
cat("Price level regression results saved to:", output_file, "\n")




# ============================================================================
# PRICE DISPERSION REGRESSIONS (LEWIS METHOD)
# ============================================================================
# The Lewis method analyzes determinants of price dispersion (variance) rather than price levels.
# Uses Prais-Winsten estimator with panel-corrected standard errors (PCSE) to account for
# panel structure and autocorrelation.

# Regression equation for dispersion analysis
# Note: ecuacion3 is defined but not used (kept for reference)
ecuacion3 <- uhat2 ~ log(density + 1) + Brand + Convenience.Store + 
  same_brand_share + Road.Type + ATM + Car.Wash + zip_code +
  muni_name + min_dist

# Main dispersion equation: dispersion as function of wholesale price and station characteristics
ecuacion4 <- disp ~ price_term + log(density + 1) + Brand + Convenience.Store + 
  same_brand_share + Road.Type + ATM + Car.Wash + zip_code +
  muni_name + min_dist

# ============================================================================
# PREPARE DATASETS FOR DISPERSION REGRESSIONS
# ============================================================================
# Prepare data for each product-year combination
# Note: prepare_datasets function exists but is not used (prepare_datasets2 is used instead)
# prepare_datasets would create uhat2 residuals, but we use disp directly

prepare_datasets2 <- function(producto){
  # Prepares datasets for dispersion regressions by product and year.
  #
  # Args:
  #   producto: Product type (regular, premium, diesel)
  #
  # Returns:
  #   List of dataframes, one for each year (2017, 2018, 2019)
  
  df <- prices1 %>% filter(product == producto)
  
  prepare_years <- function(anio, df){
    # Filter to specific year and prepare variables
    df <- df %>% 
      filter(year == anio) %>%
      # Convert date to integer for panelAR time variable
      mutate(date2 = as.integer(date)) %>% 
      # Sort by station and date for panel structure
      arrange(code, date)
    return(df)
  }
  
  dataset_years <- map(ANALYSIS_YEARS, prepare_years, df)
  return(dataset_years)
}

# Prepare datasets for all products
datasets <- map(PRODUCTS, prepare_datasets2)

# ============================================================================
# RUN DISPERSION REGRESSIONS
# ============================================================================
# Function to apply panel AR(1) model with PCSE
apply_pcse <- function(dataf){
  # Runs panel AR(1) regression with panel-corrected standard errors.
  #
  # Args:
  #   dataf: Dataframe for a specific product-year combination
  #
  # Returns:
  #   panelAR model object
  
  panelAR(formula = ecuacion4,
          data = as.data.frame(dataf), 
          panelVar = 'code',      # Panel identifier
          timeVar = 'date2',      # Time identifier
          panelCorrMethod = "pcse",  # Panel-corrected standard errors
          autoCorr = "ar1",        # AR(1) autocorrelation structure
          complete.case = FALSE)   # Keep incomplete cases
}

# Run regressions for all product-year combinations
# Structure: product_year (e.g., regular_2017)
cat("Running dispersion regressions...\n")
regular_2017 <- apply_pcse(pluck(datasets, 1, 1))
regular_2018 <- apply_pcse(pluck(datasets, 1, 2))
regular_2019 <- apply_pcse(pluck(datasets, 1, 3))

premium_2017 <- apply_pcse(pluck(datasets, 2, 1))
premium_2018 <- apply_pcse(pluck(datasets, 2, 2))
premium_2019 <- apply_pcse(pluck(datasets, 2, 3))

diesel_2017 <- apply_pcse(pluck(datasets, 3, 1))
diesel_2018 <- apply_pcse(pluck(datasets, 3, 2))
diesel_2019 <- apply_pcse(pluck(datasets, 3, 3))


# ============================================================================
# SAVE DISPERSION REGRESSION RESULTS
# ============================================================================
dispersion_output_file <- file.path(data_dir, 'regression_results_dispersion.RData')
save(regular_2017, regular_2018, regular_2019,
     premium_2017, premium_2018, premium_2019,
     diesel_2017, diesel_2018, diesel_2019, 
     file = dispersion_output_file)
cat("Dispersion regression results saved to:", dispersion_output_file, "\n")
