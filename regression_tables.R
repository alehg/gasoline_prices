# Regression Tables Generation Script
# This script generates publication-ready regression tables in LaTeX format
# for both price level and price dispersion regressions

####### Initialize ########
rm(list = ls())

# ============================================================================
# LOAD REQUIRED PACKAGES
# ============================================================================
required_packages <- c('tidyverse','openxlsx','magrittr', 'stargazer', 'texreg')
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
# HELPER FUNCTION
# ============================================================================
loadRData <- function(fileName){
  # Loads an RData file and returns the first object.
  # Note: This function assumes the RData file contains a single object.
  # For files with multiple objects, use load() directly.
  #
  # Args:
  #   fileName: Path to RData file
  #
  # Returns:
  #   The first object from the RData file
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# ============================================================================
# PRICE LEVEL REGRESSION TABLES
# ============================================================================
data_dir <- file.path(getwd(), 'data')
reg_precios_file <- file.path(data_dir, 'reg_precios.RDS')
if(!file.exists(reg_precios_file)) {
  stop(paste("Regression results file not found at:", reg_precios_file))
}
reg_precios <- readRDS(reg_precios_file)

# Function to extract robust standard errors (Driscoll-Kraay SCC)
get_robust_se <- function(i, producto){
  # Extracts robust standard errors for a specific product-period regression.
  #
  # Args:
  #   i: Period index (1 = 2017, 2 = 2018, 3 = 2019)
  #   producto: Product type (regular, premium, diesel)
  #
  # Returns:
  #   Vector of robust standard errors
  pluck(reg_precios, producto, 3, i) %>% 
    plm::vcovSCC() %>%  # Driscoll-Kraay standard errors
    diag() %>% 
    sqrt()
}

# Extract robust standard errors for all product-period combinations
price_regular_robust_se <- map(1:3, get_robust_se, 'regular')
price_premium_robust_se <- map(1:3, get_robust_se, 'premium')
price_diesel_robust_se <- map(1:3, get_robust_se, 'diesel')

tabla_regresion_precios <- function(producto, lista_se){
  # Generates LaTeX table for price level regressions.
  #
  # Args:
  #   producto: Product type (regular, premium, diesel)
  #   lista_se: List of robust standard error vectors for each period
  #
  # Returns:
  #   LaTeX table saved to file
  
  # Define covariate labels for each product (brands differ by product)
  if (producto == 'premium'){
    labs = c('Wholesale Price', 'log of Seller Density',
             'Distance to Nearest Competitor', 
             'Share of Sellers with the Same Brand in Relevant Market', 
             'Cargo Gas', 'Conser de la Laguna', 'Hidrosina', 
             'Mi Gasolina', 'OXXO Gas', 'Pemex',
             'Petro 7', 'Progas', 'Punto Gas',
             'Convenience Store', 'ATM', 'Car Wash',
             'Boulevard', 'Highway', 'Street')
  } else if(producto == "diesel"){
    # Diesel doesn't have Cargo Gas or Hidrosina brands
    labs = c('Wholesale Price', 'log of Seller Density',
             'Distance to Nearest Competitor', 
             'Share of Sellers with the Same Brand in Relevant Market',
             'Conser de la Laguna',  
             'Mi Gasolina', 'OXXO Gas', 'Pemex',
             'Petro 7', 'Progas', 'Punto Gas',
             'Convenience Store', 'ATM', 'Car Wash',
             'Boulevard', 'Highway', 'Street')
  } else{
    # Regular gasoline
    labs = c('Wholesale Price', 'log of Seller Density',
             'Distance to Nearest Competitor', 
             'Share of Sellers with the Same Brand in Relevant Market',
             'Conser de la Laguna', 'Hidrosina', 
             'Mi Gasolina', 'OXXO Gas', 'Pemex',
             'Petro 7', 'Progas', 'Punto Gas',
             'Convenience Store', 'ATM', 'Car Wash',
             'Boulevard', 'Highway', 'Street')
  }
  # Generate LaTeX table using stargazer
  output_file <- file.path(data_dir, str_c('price_', producto, '.tex'))
  
  stargazer(pluck(reg_precios, producto, 3, 1),  # 2017 model
            pluck(reg_precios, producto, 3, 2),  # 2018 model
            pluck(reg_precios, producto, 3, 3),  # 2019 model
            se = lista_se,  # Robust standard errors
            title = str_c('Regression Results (', str_to_title(producto), ' Gasoline)'), 
            style = 'qje',  # Quarterly Journal of Economics style
            column.labels = c('2017', '2018', '2019'),
            dep.var.labels = 'Retail Price $p_{it}$',
            align = FALSE,
            single.row = TRUE,  # Coefficients and SEs on same row
            out = output_file,
            covariate.labels = labs,
            omit = c('zip', 'Tor'),  # Omit zip code and Torreon municipality dummies
            order = c('term', 'density', 'min_dist', 'same_brand_share'),  # Reorder variables
            digits = 3,
            label = str_c('preg_', producto),
            model.names = FALSE,
            no.space = TRUE,
            notes = 'Robust standard errors in parenthesis',
            omit.stat = c('f'))  # Omit F-statistic
  
  cat("Price regression table saved to:", output_file, "\n")
}

# Generate tables for all products (example: diesel)
tabla_regresion_precios('diesel', price_diesel_robust_se)

# Example: View summary of diesel 2017 regression
cat("\nExample: Diesel 2017 Regression Summary\n")
summary(pluck(reg_precios, 'diesel', 3, 1))

# ============================================================================
# DISPERSION REGRESSION TABLES
# ============================================================================
dispersion_file <- file.path(data_dir, 'regression_results_dispersion.RData')
if(!file.exists(dispersion_file)) {
  stop(paste("Dispersion regression results file not found at:", dispersion_file))
}
load(dispersion_file)

# Create output directory for LaTeX tables if it doesn't exist
tex_dir <- file.path(data_dir, 'tex')
if(!dir.exists(tex_dir)) {
  dir.create(tex_dir, recursive = TRUE)
  cat("Created output directory:", tex_dir, "\n")
}

# Generate dispersion regression table for diesel (example)
# Note: Similar tables can be generated for regular and premium
texreg(l = list(diesel_2017, diesel_2018, diesel_2019),
       file = file.path(tex_dir, 'dispersion_diesel.tex'),
       single.row = TRUE,
       custom.model.names = c('2017', '2018', '2019'),
       custom.coef.map = list( 'price_term' = 'Wholesale Price', 
                               'log(density + 1)' = 'log of Seller Density',
                               'min_dist' = 'Distance to Nearest Competitor',
                               'same_brand_share' = 'Share of Sellers with Same Brand within Relevant Market',
                               'Brandconser de la laguna' = 'Conser de la Laguna',
                               'Brandhidrosina' = 'Hidrosina',
                               'Brandmigasolina' = 'Mi Gasolina',
                               'Brandoxxogas' = 'OXXO Gas',
                               'Brandpemex' = 'Pemex',
                               'Brandpetro 7' = 'Petro 7',
                               'Brandprogas' = 'Pro Gas',
                               'Brandpuntogas' = 'Punto Gas',
                               'Convenience.Store' = 'Convenience Store',
                               'ATM' = NA,
                               'Car.Wash' = 'Car Wash',
                               'Road.Typeboulevard' = 'Boulevard',
                               'Road.Typehighway' = 'Highway',
                               'Road.Typestreet' = 'Street',
                               '(Intercept)' = 'Constant') ,
       center = TRUE,
       caption = "Regression Results (Diesel Gasoline)",
       caption.above = TRUE,
       label = 'disp_results_diesel')
