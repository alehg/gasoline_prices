## Code to perform regression analysis 

rm(list=ls())

# load clean prices with auxiliar regression residuals
load("data/final_prices.RData")

#install.packages('pacman')
x <- c('tidyverse','openxlsx','geosphere','magrittr','plm',
       'stats','utils','lubridate','corrplot','pcse')
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
  fixed <- plm(price_end ~ price_term + density,df, index = c('code','date'), model = 'within')
  random  <- plm(ecuacion,df,index = c('code','date'),model = 'random')
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
  Brand + Convenience.Store + ATM + Car.Wash + min_dist + 
  Road.Type +  same_brand_share + zip_code + muni_name  + 
  log(num_est + 1)

prueba_reg  <- plm(ecuacion_prueba,
                   data = prices1 %>% filter(product == 'premium'),
                   index = c('code','date'),
                   model = 'random')
summary(prueba_reg)
summary(prueba_reg, vcov = vcovBK(prueba_reg, cluster = 'group', type = 'HC0'))

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
