# This code will be used to clean and build the main dataframe for analysis
# Alejandro Hernandez 2019
#-----------------------------

rm(list=ls())

x <- c('tidyverse','openxlsx','magrittr','lubridate',
       'geosphere')
lapply(x, library, character.only = TRUE)

dir <- '/Functions/'
funs<-list.files(paste0(getwd(),dir))
lapply(str_c(getwd(),dir,funs),source)

prices_raw <- readRDS('data/laguna_prices.rds')

# We are only intrested in prices after deregulation 
prices0 <- prices_raw %>% 
  filter(date >= '2017-06-15')

# In this dataframe we have 808 days and 154 different stations
# apply function that removes non-reporting stations
prices1 <- map_dfr(unique(prices0$product), limpieza, prices0)

# After a station reported a price, there are no missing values afterwards so if
# there are any missing values I will assume the station did not sell that product at that time

# Before I remove missing values in the price column,
# I will get the codes for stations that entered after deregulation and their entry dates

entry <- function(producto,df){
  
  df_clean <- df %>% filter(product == producto)
  new_stations <- df_clean %>% filter(is.na(price_end)) %>% pull(code) %>% unique()
  
  first_date <- function(station,df){
    # this functions extracts the date in which the new stations entered the market
    nona_df <-  df %>% filter(code == station,!is.na(price_end))
    fecha <- nona_df %>% filter(date == min(date)) %>% 
      pull(date) %>% unique() %>% as.character.Date()
  }
  
  fechas <- map_chr(new_stations,first_date,df_clean) %>% unique() #entry dates
  return(list(new_stations,fechas))
}

entry_dates <- map(unique(prices1$product),entry,prices1)
names(entry_dates) <- unique(prices1$product)

#################  Gasolineras (Torreon y Gomez Palacio)  ######################

gasot <- read.xlsx('data/gast.xlsx',1) %>%  #load data from google maps
  mutate(muni_name = 'Torreon') 

gasogp <- read.xlsx('data/gasgp.xlsx',1) %>%  #gomez palacio
  mutate(muni_name = 'Gomez Palacio') 

gasocl <- rbind(gasot,gasogp) %>% #gasolineras comarca lagunera
  mutate(Brand = ifelse(Migasolina == 1,'migasolina',trimws(Brand)),
         #periferico  as highway
         Road.Type = ifelse(Road.Type == 'periferico','highway',as.character(Road.Type)),
         latitude = as.numeric(as.character(latitude)),
         longitude = as.numeric(as.character(longitude)),
         code = as.character(code)) %>% 
  filter(code %in% prices1$code)

rm(gasot,gasogp) #remove databases

# Now I want to have a dataframe with every pair of stations and their respective distance 
pairs_raw <- t(combn(gasocl$code,2)) %>% as_tibble() #all pairs of stations

# bind the lat and lon variables for each code to compute distances
pairs0 <- left_join(pairs_raw %>% rename(code = V1),gasocl, by = 'code') %>% 
  select(code1 = code,code = V2,lat1 = latitude,lon1 = longitude, brand1 = Brand)
pairs1 <- left_join(pairs0,gasocl, by = 'code') %>% 
  select(code1,code2 = code,lat1,lon1,lat2 = latitude,lon2 = longitude, brand1, brand2 = Brand)

distancias <- pairs1 %>% rowwise() %>% 
  mutate(dist = distGeo(c(lon1,lat1),c(lon2,lat2))) # distance between stations in meters
rm(pairs_raw,pairs0,pairs1)

# For each type of product and for each station I will compute the number of competitors
# every time a new station enters the market

prices2 <- map_dfr(unique(prices1$product),densidad,prices1,distancias,entry_dates,radius = 2000)

 # Here I compute the average price for each day and then calculate the deviation 
 # prices_check <- prices2 %>% 
 #   group_by(date,product) %>% 
 #   mutate(meanPrice = mean(price_end),
 #          dev = price_end - meanPrice) %>% 
 #   ungroup() 
 # outliers <- prices_check %>% 
 #   filter(abs(dev) > 0.6)
 # there are a few outliers (around 2500 out of 240000, 1%) but none of them have a deviation higher than one peso
 
clean_prices <- prices2 %>% 
   filter(!is.na(price_end)) %>% 
   left_join(gasocl, by = 'code') %>% 
   select(-c('latitude','longitude')) %>% 
   mutate(same_brand_share = if_else(density != 0, same_brand/density, 0),
          year = year(date)) %>% 
   group_by(product,date) %>% 
   mutate(sd = sd(price_end,na.rm=T)) %>% ungroup()

 
 
save(clean_prices,prices_raw,distancias,gasocl,entry_dates,file = 'data/clean_prices.RData') 

