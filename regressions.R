## Code to perform regression analysis 

rm(list=ls())
# ------ Initialize ---------------------------
# load clean prices with auxiliar regression residuals
# load("data/final_prices.RData")

#install.packages('pacman')
x <- c('tidyverse','openxlsx','geosphere','magrittr','plm',
       'stats','utils','lubridate','corrplot','pcse', 'panelAR')
#pacman::p_load(char=x,character.only=T)
lapply(x, library, character.only = TRUE)

dir<-'/Functions/'
funs<-list.files(paste0(getwd(),dir))
lapply(paste0(getwd(),dir,funs),source)

load('data/denue.RData')
prices1 <- prices1 %>% 
  mutate(Road.Type = if_else(Road.Type == 'drive', 'avenue', Road.Type))
ageb_data <- readRDS('data/ageb_final.RDS') %>% 
  select(code, pobtot, graproes, pea, vph_autom)
prices1 <- left_join(prices1, ageb_data, by='code')
#------- Level of Dispersion ----------
prices1 %>% 
  group_by(product, year) %>% 
  summarise(disp = sd(uhat)) %>% 
  pivot_wider(id_cols = product, 
              names_from = year, 
              values_from = disp)
#----------------- Price Level regression -------------------------
ecuacion <- price_end ~ price_term + log(density + 1) + 
  Brand + Convenience.Store + ATM + Car.Wash + zip_code + muni_name +
  Road.Type +  same_brand_share + min_dist


pricereg <- function(producto,df, equation){
  df <- df %>% filter(product == producto)
  random  <- plm(equation,
                 df,
                 index = c('code','date'),
                 model = 'random')
  # Serial Correlation test (Breusch-Godfrey)
  bg_test <- pbgtest(random)
  
  periodsreg <- function(periodo){
    reg <- plm(equation,filter(df,period == periodo), index = c('code','date'), model = 'random')
  }
  periodos <- map(1:3,periodsreg)
  return(list(random,bg_test,periodos))
}
gasr <- unique(prices1$product)
ptm <- proc.time()
reg_precios <- map(gasr,pricereg,prices1)
proc.time() - ptm


pluck(reg_precios,'regular',1) %>% summary(vcov = plm::vcovSCC(.))


saveRDS(reg_precios, file = 'data/reg_precios.RDS')
# serial correlation test
pbgtest(prueba_reg, order = 2)
summary(prueba_reg)
summary(prueba_reg, vcov = plm::vcov(prueba_reg,  cluster = 'group'))


# -------- Suggestions ------
# 1. log(price)
# 2. remove min_dist variable
# 3. Use fixed effects

# Regression using log price
ecuacion_aux <- log(price_end) ~ log(price_term) + log(density + 1) + 
  Brand + Convenience.Store + ATM + Car.Wash  +
  Road.Type +  same_brand_share + log(graproes+1) + log(vph_autom+1) 
gasr <- unique(prices1$product)

ptm <- proc.time()
aux_reg_precios <- map(gasr,pricereg,prices1, ecuacion_aux)
proc.time() - ptm
names(aux_reg_precios) <- gasr

pluck(aux_reg_precios, 'regular', 3, 1) %>% summary(vcov = plm::vcovSCC(.))

# Before adding AGEB vars check their correlation with each other and with seller density

#-----------Lewis Method-------------------
# Prais-Winsten estimator
ecuacion3 <- uhat2 ~ log(density + 1) + Brand + Convenience.Store + 
  same_brand_share + Road.Type + ATM + Car.Wash + zip_code +
  muni_name + min_dist
ecuacion4 <- disp ~ price_term + log(density + 1) + Brand + Convenience.Store + 
  same_brand_share + Road.Type + ATM + Car.Wash + zip_code +
  muni_name + min_dist

productos <- c('regular', 'premium', 'diesel')
prepare_datasets <- function(producto){
  df <- prices1 %>% filter(product == producto)
  prepare_years <- function(anio, df){
    df <- df %>% filter(year == anio)
    areg <- lm(disp ~ price_term, data = df)
    df <- df %>% 
      bind_cols(uhat2 = resid(areg)) %>% 
      mutate(date2 = as.integer(date)) %>% 
      arrange(code, date)
    return(df)
  }
  dataset_years <- map(2017:2019, prepare_years, df)
  return(dataset_years)
}
prepare_datasets2 <- function(producto){
  df <- prices1 %>% filter(product == producto)
  prepare_years <- function(anio, df){
    df <- df %>% 
      filter(year == anio) %>%
      mutate(date2 = as.integer(date)) %>% 
      arrange(code, date)
    return(df)
  }
  dataset_years <- map(2017:2019, prepare_years, df)
  return(dataset_years)
}
datasets <- map(productos, prepare_datasets2)
apply_pcse <- function(dataf){
  panelAR(formula = ecuacion4,
          data = as.data.frame(dataf), 
          panelVar = 'code', 
          timeVar = 'date2',
          panelCorrMethod = "pcse", 
          autoCorr = "ar1", 
          complete.case = F) 
  }

regular_2017 <- apply_pcse(pluck(datasets,1,1))
regular_2018 <- apply_pcse(pluck(datasets,1,2))
regular_2019 <- apply_pcse(pluck(datasets,1,3))

premium_2017 <- apply_pcse(pluck(datasets,2,1))
premium_2018 <- apply_pcse(pluck(datasets,2,2))
premium_2019 <- apply_pcse(pluck(datasets,2,3))

diesel_2017 <- apply_pcse(pluck(datasets,3,1))
diesel_2018 <- apply_pcse(pluck(datasets,3,2))
diesel_2019 <- apply_pcse(pluck(datasets,3,3))


save(regular_2017,
     regular_2018,
     regular_2019,
     premium_2017,
     premium_2018,
     premium_2019,
     diesel_2017,
     diesel_2018,
     diesel_2019, 
     file = 'data/regression_results_dispersion.RData')
