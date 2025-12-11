# Descriptive Statistics and Visualization Script
# This script generates exploratory data analysis plots and descriptive statistics
# for the gasoline price analysis

rm(list=ls())

# ============================================================================
# LOAD REQUIRED PACKAGES
# ============================================================================
required_packages <- c('tidyverse','openxlsx','geosphere',
                       'magrittr','scales','readxl','extrafont',
                       'fastDummies','treemapify','lubridate',
                       'corrplot','stats')
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
data_dir <- file.path(getwd(), 'data')
clean_prices_file <- file.path(data_dir, 'clean_prices.RData')
if(!file.exists(clean_prices_file)) {
  stop(paste("Clean prices file not found at:", clean_prices_file))
}
load(clean_prices_file)

# ============================================================================
# SET UP PLOTTING THEME AND COLORS
# ============================================================================
# Set locale for proper date formatting
Sys.setlocale(locale = 'English')

# Define custom theme for thesis/publication-quality plots
# Note: font_install('font_cm') may be needed if Arial Unicode MS is not available
theme_tesis <- theme_minimal() +
  theme(text = element_text(family = 'Arial Unicode MS'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 17, hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        axis.text = element_text(size = 13),
        legend.position = 'top',
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

theme_set(theme_tesis)

# Color palette for plots
azules <- c("#023876", "#045DC3", "#318DF6")  # Blues from dark to light
naranja <- c("#C56403")  # Orange
verdes <- c('#044A26', '#006D34')  # Greens
gris <- c('#858585')  # Gray

# Note: Example multiplot call (commented out as plots don't exist yet)
# multiplot(pluck(quant_plots, 'diesel') + ggtitle('(A)'), 
#           pluck(sd_plots, 'diesel') + ggtitle('(B)'))
# Recommended aspect ratio: 1102 x 1235 pixels

# ============================================================================
# AUXILIARY REGRESSION FOR PRICE DISPERSION
# ============================================================================
# Run fixed effects regression (station and date fixed effects) to extract
# residuals. These residuals represent price deviations from station and
# time averages, which are then used to measure price dispersion.
#
# The dispersion measure is log(uhat^2), where uhat are the residuals from
# the fixed effects regression.

aux_reg <- function(producto, price_df){
  # Runs auxiliary fixed effects regression for a product.
  #
  # Args:
  #   producto: Product type (regular, premium, diesel)
  #   price_df: Price dataframe
  #
  # Returns:
  #   List containing:
  #     - Dataframe with residuals (uhat) and dispersion measure (disp)
  #     - Regression model object
  
  df <- price_df %>% filter(product == producto)
  
  # Fixed effects regression: price ~ station FE + date FE
  # This removes station-specific and time-specific effects
  modelito <- lm(price_end ~ factor(code) + factor(date), data = df)
  
  # Extract residuals (deviations from station and time averages)
  u_hat <- resid(modelito)
  
  # Create dispersion measure: log of squared residuals
  # This measures the variance of price deviations
  df <- df %>% 
    mutate(uhat = u_hat, disp = log(uhat^2))
  
  return(list(df, modelito))
}

# Run auxiliary regression for each product
auxreg_list <- map(unique(clean_prices$product), aux_reg, clean_prices)

# Combine results from all products into single dataframe
final_prices <- map_dfr(1:length(auxreg_list), 
                       function(i, lista) pluck(lista, i, 1), 
                       auxreg_list)

# Save final prices with dispersion measures
final_prices_file <- file.path(data_dir, 'final_prices.RData')
save(final_prices, file = final_prices_file)
cat("Final prices with dispersion measures saved to:", final_prices_file, "\n")

# Save auxiliary regression results
auxreg_file <- file.path(data_dir, 'auxreg_results.RData')
save(auxreg_list, file = auxreg_file)

# ============================================================================
# GENERATE PLOTS
# ============================================================================

# Standard Deviation Plots: Show evolution of price dispersion over time
sd_plots <- map(unique(final_prices$product), sdplot, final_prices)
names(sd_plots) <- unique(final_prices$product)

# Quantile Plots: Show distribution of prices across stations over time
quant_plots <- map(unique(final_prices$product), quantplot, final_prices)
names(quant_plots) <- unique(final_prices$product)

# Terminal (Wholesale) Price Plots: Show wholesale price evolution
# Note: Analysis could be extended to determine if volatility and price
# decline in 2019 is due to fiscal policies
term_plots <- map(unique(final_prices$product), terminalplot, final_prices)
names(term_plots) <- unique(final_prices$product)

# Research question: Why do terminal prices differ so much between products?

# Reset locale
Sys.setlocale(locale = "")


# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================

# Create summary statistics table for station characteristics
# Computes mean, sd, min, max for density, brand shares, amenities, etc.
tabla <- lst(avg = mean, sd = sd, min = min, max = max) %>% 
  map(~ final_prices %>% 
    # Create dummy variables for Road.Type and Brand for summary statistics
    dummy_cols(select_columns = c('Road.Type', 'Brand')) %>% 
    # Compute statistics for key variables
    summarise_at(vars(density, same_brand, same_brand_share, min_dist,
                      Convenience.Store, ATM, 
                      contains('Road.Type_'), contains('Brand_')), 
                 .x, na.rm = TRUE) %>% 
    ungroup() %>% 
    # Reshape to long format for joining
    gather('var', 'value')) %>% 
  # Join all statistics into single table
  reduce(inner_join, by = 'var') %>% 
  rename(mean = value.x, sd = value.y, min = value.x.x, max = value.y.y)
  
# How many stations per municipalities?
# mode function
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
# final_prices %>% 
#   group_by(code) %>% summarise(muni = getmode(muni_name)) %>% 
#   group_by(muni) %>% summarise(stations = n()) %>% ungroup() 

# 97 in Torreon (68.3%)
# 45 in Gomez Palacio (31.7%)

# Distribution of road type
road_type <- tabla %>% filter(var %>% str_detect('Road')) %>% 
  mutate(var = str_replace(var,'Road.Type_','') %>% str_to_title()) %>% 
  select(var,mean) %>% 
  ggplot(aes(area = mean,fill = var)) + 
  geom_treemap(stat = 'identity') +
  geom_treemap_text(aes(label = var),place = 'centre')+
  scale_fill_manual(values = c(azules[2:3],verdes[2],gris,naranja))+
  theme(legend.position = 'none') +
  labs(title = 'Distribution of stations by Road Type')
  
# Distribution of brands
brand <- tabla %>% filter(var %>% str_detect('Brand')) %>% 
  mutate(var = str_replace(var,'Brand_','') %>% str_to_title()) %>% 
  select(var,mean) %>% 
  arrange(desc(mean)) %>% 
  ggplot(aes(x = reorder(var,mean),y = mean)) + 
  geom_col()+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.title.y = element_blank()) + 
  labs(title = 'Distribution of Stations by Brand')

# Distribution of densities 
density_hist <-final_prices %>% group_by(code) %>% 
  summarise(density = round(mean(density))) %>%  
  group_by(density) %>% summarise(n = n()) %>% 
  ggplot(aes(x = density,y = n)) + geom_col() +
  labs(title = 'Histogram of Density')


graficas_densidad <- function(prices_df){
  # Density by muni
  muni <- prices_df %>% 
    ggplot(aes(x = muni_name,y = density,fill = muni_name)) + 
    geom_boxplot(show.legend = F)
  
  # Boxplot: Road Type vs Density
  road_type <- prices_df %>% 
    ggplot(aes(x = Road.Type,y = density,fill = Road.Type)) + 
    geom_boxplot(show.legend = F)
  
  # Boxplot: Convenience vs Density
  conv <- prices_df %>% 
    ggplot(aes(x = as.character(Convenience.Store),y = density,fill = as.character(Convenience.Store))) + 
    geom_boxplot(show.legend = F)
  
  # Boxplot: Brand vs Density
  brand <- prices_df %>% 
    ggplot(aes(x = Brand,y = density,fill = Brand)) + 
    geom_boxplot(show.legend = F)
  
  return(list(muni = muni, road_type = road_type, convenience = conv,brand = brand))
}
density_plots <- graficas_densidad(final_prices)

graficas_precios <- function(producto,price_df){
  # Boxplot: Price vs. Density 
  den <- price_df %>% 
    mutate(den_cat = if_else(density <= 14,'low','high')) %>% 
    filter(product == producto) %>% 
    ggplot(aes(x = as.character(year),y = price_end,fill = den_cat)) + 
    geom_boxplot()
  
  
  # Boxplot: Price vs Convenience Store 
  conv <- price_df %>% 
    filter(product == producto) %>% 
    ggplot(aes(x = as.character(year),y = price_end,fill = as.character(Convenience.Store))) + 
    geom_boxplot()
  
  # Boxplot: Price vs Road.Type 
  road <- price_df %>% 
    filter(product == producto) %>% 
    ggplot(aes(x = as.character(year),y = price_end,fill = Road.Type)) + 
    geom_boxplot()
  
  
  # Boxplot: Price vs. Brand 
  brand <- price_df %>% 
    filter(product == producto) %>% 
    ggplot(aes(x = as.character(year),y = price_end,fill = Brand)) + 
    geom_boxplot()
  
  return(list(density = den,convenience = conv, road = road,brand = brand))
}
price_plots <- map(unique(final_prices$product),graficas_precios,final_prices)
names(price_plots) <- unique(final_prices$product)

graficas_correlaciones <- function(producto,price_df){
 
  first <- price_df %>% 
    filter(date <= '2017-12-31',product == producto) %>% 
    select(price_end,price_term,sd,disp,density,min_dist,Convenience.Store,same_brand_share) %>% 
    cor(use = "pairwise.complete.obs") %>% corrplot(method = 'color', addCoef.col = 'black')
  
  second <- price_df %>% 
    filter(date > '2017-12-31',date <= '2018-12-31',product == producto) %>% 
    select(price_end,price_term,sd,disp,density,min_dist,Convenience.Store,same_brand_share) %>% 
    cor(use = "pairwise.complete.obs") %>% corrplot(method = 'color',addCoef.col = 'black')
  
  third <- price_df %>% 
    filter(date > '2018-12-31',product == producto) %>% 
    select(price_end,price_term,sd,disp,density,min_dist,Convenience.Store,same_brand_share) %>% 
    cor(use = "pairwise.complete.obs") %>% corrplot(method = 'color', addCoef.col = 'black')
  return(list(first = first, second = second, third = third))
}
correlation_plots <- map(unique(final_prices$product),graficas_correlaciones,final_prices)
names(correlation_plots) <- unique(final_prices$product)

grafica_dispersion <- function(producto,price_df){
  
  price_df %>% 
    filter(product == producto) %>% 
    group_by(date) %>% 
    summarise_at(vars(disp),mean) %>% 
    ungroup() %>% 
    ggplot(aes(x = date, y = disp)) +
    geom_line(size = 1) + 
    geom_vline(xintercept = as.numeric(as.Date(c('2018-01-01','2019-01-01')))) + 
    scale_x_date(breaks = seq.Date(as.Date('2017-07-01'),as.Date('2019-07-01'), by = '3 months'),
                 date_labels = '%b %y')
  
}

dispersion_plots <- map(unique(final_prices$product),grafica_dispersion,final_prices)
names(dispersion_plots) <- unique(final_prices$product)

# ============================================================================
# PRICE SUMMARY TABLE
# ============================================================================
# Create summary table of average and standard deviation of prices by year and product
# Note: This uses clean_prices (not prices1) since prices1 may not be loaded
# If prices1 is needed, it should be loaded from denue.RData first

tabla_precios <- clean_prices %>% 
  select(date, year, product, price_end, price_term) %>% 
  # Reshape to long format: price_end and price_term become 'tipo' variable
  pivot_longer(cols = c('price_end', 'price_term'),
               names_to = 'tipo',
               values_to = 'value') %>% 
  # Compute summary statistics by year, product, and price type
  group_by(year, product, tipo) %>% 
  summarise(avg_price = mean(value, na.rm = TRUE),
            sd_price = sd(value, na.rm = TRUE),
            .groups = 'drop')

# Reshape to wide format for presentation
# Note: The nested pivot operations create a complex structure
tabla_precios2 <- tabla_precios %>% 
  pivot_longer(cols = contains('price'), names_to = 'stat_name', values_to = 'value') %>% 
  pivot_wider(id_cols = c('tipo', 'product'), 
              names_from = c(year, stat_name), 
              values_from = value)

# ============================================================================
# SAVE ALL PLOTS AND TABLES
# ============================================================================
plots_file <- file.path(data_dir, 'plots.RData')
save(sd_plots, quant_plots, term_plots, tabla, road_type, brand,
     density_hist, density_plots, price_plots,
     correlation_plots, dispersion_plots, tabla_precios,
     file = plots_file)
cat("All plots and tables saved to:", plots_file, "\n")




