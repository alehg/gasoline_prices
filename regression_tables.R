rm(list = ls())

#install.packages('pacman')
x <- c('tidyverse','openxlsx','geosphere','magrittr','plm',
       'stats','utils','lubridate','corrplot','pcse', 'panelAR', 'stargazer')
#pacman::p_load(char=x,character.only=T)
lapply(x, library, character.only = TRUE)

dir<-'/Functions/'
funs<-list.files(paste0(getwd(),dir))
lapply(paste0(getwd(),dir,funs),source)

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

###### Price Regressions ######
reg_precios <- readRDS('data/reg_precios.RDS')
stargazer(reg_precios %>% pluck('regular',1))
