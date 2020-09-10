## Code to perform regression analysis 

rm(list=ls())

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


# Done only for La Laguna region, regular and premium


# prices0  <- final_prices %>%
#   mutate(period = case_when(
#     date < '2018-01-01' ~ 1,
#     date < '2019-01-01' & date >= '2018-01-01' ~ 2,
#     date >= '2019-01-01' ~ 3),
#     min_dist=min_dist/1000)

load('data/denue.RData')
prices1 <- prices1 %>% 
  mutate(Road.Type = if_else(Road.Type == 'drive', 'avenue', Road.Type))
#----------------- Price Level regression -------------------------
ecuacion <- price_end ~ price_term + density + 
  Brand + Convenience.Store + 
  Road.Type +  same_brand_share 

pricereg <- function(producto,df){
  df <- df %>% filter(product == producto)
  # random vs pooled ols
  #between <- plm(price_end ~ price_term + density,df, index = c('code','date'), model = 'between')
  fixed <- plm(price_end ~ price_term + density,
               df, 
               index = c('code','date'), 
               model = 'within')
  
  random  <- plm(ecuacion,
                 df,
                 index = c('code','date'),
                 model = 'random')
  #pool <- plm(ecuacion,df,index = c('code','date'),model='pooling')
  
  #null: zero variance of individual effect is zero
  # if rejected, heterogeneity among individuals is significant
  #effects_test <- plmtest(pool, type = 'bp') 
  
  # Hausman endogeneity test
  # null: individual random effects are exogenous
  hausman_test <- phtest(fixed,random) #null rejected, random model is consistent
  
  # Serial Correlation test (Breusch-Godfrey)
  bg_test <- pbgtest(random)
  
  periodsreg <- function(periodo){
    reg <- plm(ecuacion,filter(df,period == periodo), index = c('code','date'), model = 'random')
  }
  periodos <- map(1:3,periodsreg)
  return(list(fixed,random,hausman_test,bg_test,periodos))
}
gasr <- unique(prices1$product)
ptm <- proc.time()
reg_precios <- map(gasr,pricereg,prices1)
proc.time() - ptm
names(reg_precios) <- gasr



ecuacion_prueba <- price_end ~ price_term + log(density + 1) + 
  Brand + Convenience.Store +  min_dist + 
  Road.Type +  same_brand_share + zip_code + muni_name  

prueba_reg  <- plm(ecuacion_prueba,
                   data = prices1 %>% 
                     filter(product == 'premium'),
                   index = c('code','date'),
                   model = 'random')

# serial correlation test
pbgtest(prueba_reg, order = 2)
summary(prueba_reg)
summary(prueba_reg, vcov = plm::vcovBK(prueba_reg,  cluster = 'group'))

# ------------------- Dispersion Regression -------------------------
ecuacion2 <- "disp ~ log(density + 1) + Brand + Convenience.Store + 
  same_brand_share + Road.Type + ATM + Car.Wash + zip_code + 
  muni_name + min_dist + log(num_est + 1)"

dispersion_reg <- function(producto,df){
  df <- df %>% filter(product == producto)
  # random vs pooled ols
  ecuacion2 <- formula(ecuacion2)
  fe_reg <- plm(ecuacion2,
                data = df,
                index = c('code','date'),
                model = 'within',
                effect = 'time')
  # re_reg <- plm(ecuacion2,
  #               data = df,
  #               index = c('code','date'),
  #               model = 'random')
  
  periodsreg <- function(periodo){
    aux <- df %>% filter(period == periodo)
    plm(ecuacion2,
        data = aux, 
        index = c('code','date'),
        model = 'within',
        effect = 'time')
  }
  periodos <- map(1:3,periodsreg)
  return(list(fe_reg,periodos))
}
ptm <- proc.time()
reg_disp <- map(gasr,dispersion_reg,prices1) 
names(reg_disp) <- gasr
proc.time() - ptm
summary(pluck(reg_disp,'premium',1))
summary(pluck(reg_disp,'regular',2,3),
        vcov = vcovBK(pluck(reg_disp,'regular',2,3),
                      cluster = 'group',
                      type = 'HC0'))


#-----------Lewis Method-------------------
# Prais-Winsten estimator
ecuacion3 <- uhat2 ~ log(density + 1) + Brand + Convenience.Store + 
  same_brand_share + Road.Type + ATM + Car.Wash + zip_code +
  muni_name + min_dist + log(num_est + 1)


lewis_reg <- function(producto, df){
  df <- df %>% filter(product == producto)
  periods_reg <- function(year,df){
    df <- df %>% filter(year==year)
    areg <- lm(disp ~ price_term, data = df)
    df <- df %>% 
      bind_cols(uhat2 = resid(areg)) %>% 
      mutate(date2 = as.integer(date)) %>% 
      arrange(code, date)
    out <- panelAR(formula = ecuacion3, 
                   data = as.data.frame(df), 
                   panelVar = 'code', 
                   timeVar = 'date2',
                   panelCorrMethod = "pcse", 
                   autoCorr = "ar1", 
                   complete.case = F)
  }
  periodos <- map(2017:2019,periods_reg, df)
}

regular_results <- lewis_reg('regular', prices1)


diesel <- prices1 %>% filter(product == 'diesel', year == 2019)
areg <- lm(disp ~ price_term, data = diesel)
diesel <- diesel %>% bind_cols(uhat2 = resid(areg))
diesel <- diesel %>% mutate(date2 = as.integer(date)) %>% arrange(code,date)



diesel_2019 <- panelAR(formula = ecuacion3, 
               data = as.data.frame(diesel), 
               panelVar = 'code', 
               timeVar = 'date2',
               panelCorrMethod = "pcse", 
               autoCorr = "ar1", 
               complete.case = F)  
  
summary(diesel_2019) 
save(diesel_2019, file='data/diesel_2019.RData')

# anova <- list(anova(pluck(reg_disp,1,1)),anova(pluck(reg_disp,2,1))) #anova

# ---------------Local Variance Regression  ------------------
# ecuacion3 <- log(vhat^2 + .0001) ~ log(density + 1) + Brand + Convenience.Store + 
#   Car.Wash + Road.Type + min_dist + same_brand_share
# 
# ecuacion4 <- pdev ~ log(density + 1) + Brand + Convenience.Store + Car.Wash + 
#   Road.Type + min_dist + same_brand_share
# 
# v <- vecinos(gasocl) #list of neighbors
# 
# localreg <- function(df){
#   df <- varianza(df,v)
#   df %<>% mutate(pdev = price - mean_price,
#                  dhat = uhat - mean_uhat)
#   aux <- lm(uhat ~ mean_uhat -1, df)
#   vhat <- resid(aux)
#   df <- data.frame(df,vhat = vhat)
#   vreg <- regresiontw(df,ecuacion3) #variance
#   preg <- regresiontw(df,ecuacion4) #mean
#   return(list(vreg,preg))
# }
# 
# reg_local <- map(gasr,localreg)


save(gasr,reg_precios,reg_disp, file = 'data/regression_results.RData')
