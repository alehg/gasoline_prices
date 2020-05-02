## Code to perform regression analysis 

rm(list=ls())

# load clean prices with auxiliar regression residuals
load("data/final_prices.RData")

#install.packages('pacman')
x <- c('tidyverse','openxlsx','geosphere','magrittr','plm',
       'stats','utils','lubridate','corrplot')
#pacman::p_load(char=x,character.only=T)
lapply(x, library, character.only = TRUE)

dir<-'/Functions/'
funs<-list.files(paste0(getwd(),dir))
lapply(paste0(getwd(),dir,funs),source)


# Done only for La Laguna region, regular and premium


prices0  <- final_prices %>% 
  mutate(period = case_when(
    date < '2018-01-01' ~ 1,
    date < '2019-01-01' & date >= '2018-01-01' ~ 2,
    date >= '2019-01-01' ~ 3),
    min_dist=min_dist/1000)



#----------------- Price Level regression -------------------------
ecuacion <- price_end ~ price_term + density + 
  Brand + Convenience.Store + 
  Road.Type +  same_brand_share 

pricereg <- function(producto,df){
  df <- df %>% filter(product == producto)
  # random vs pooled ols
  between <- plm(price_end ~ price_term + density,df, index = c('code','date'), model = 'between')
  fixed <- plm(price_end ~ price_term + density,df, index = c('code','date'), model = 'within')
  random  <- plm(ecuacion,df,index = c('code','date'),model = 'random')
  pool <- plm(ecuacion,df,index = c('code','date'),model='pooling')
  
  #null: zero variance of individual effect is zero
  # if rejected, heterogeneity among individuals is significant
  effects_test <- plmtest(pool, type = 'bp') 
  
  # Hausman endogeneity test
  # null: individual random effects are exogenous
  hausman_test <- phtest(fixed,random) #null rejected, random model is consistent
  
  periodsreg <- function(periodo){
    reg <- plm(ecuacion,filter(df,period == periodo), index = c('code','date'), model = 'random')
  }
  periodos <- map(1:3,periodsreg)
  return(list(between,fixed,random,pool,effects_test,hausman_test,periodos))
}
gasr <- unique(prices0$product)
ptm <- proc.time()
reg_precios <- map(gasr,pricereg,prices0)
proc.time() - ptm
names(reg_precios) <- gasr

# ------------------- Dispersion Regression -------------------------
ecuacion2 <- disp ~ price_term + density + Brand + Convenience.Store + 
  same_brand_share + Road.Type  

dispersion_reg <- function(producto,df){
  df <- df %>% filter(product == producto)
  # random vs pooled ols
  fixed <- plm(ecuacion2,df, index = c('code','date'), model = 'within', effect='time')
  random  <- plm(ecuacion2,df,index = c('code','date'),model = 'random')
  pool <- plm(ecuacion2,df,index = c('code','date'),model='pooling') 
  
  #null: zero variance of individual effect is zero
  # if rejected, heterogeneity among individuals is significant
  effects_test <- plmtest(pool, type = 'bp') 
  
  # Hausman endogeneity test
  # null: individual random effects are exogenous
  hausman_test <- phtest(fixed,random) #null rejected, random model is consistent
  
  periodsreg <- function(periodo){
    reg <- plm(ecuacion2,
               filter(df,period == periodo), 
               index = c('code','date'), 
               model = 'random',
               effect = 'time')
  }
  periodos <- map(1:3,periodsreg)
  return(list(fixed,random,pool,effects_test,hausman_test))
}

reg_disp <- map(gasr,dispersion_reg,prices0) 
names(reg_disp) <- gasr

pluck(reg_disp,'premium',1) %>% summary()


anova <- list(anova(pluck(reg_disp,1,1)),anova(pluck(reg_disp,2,1))) #anova

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


save(gasr,reg_precios,reg_disp,anova, file = 'regression_results.RData')
