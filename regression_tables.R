####### Inititalize ########
rm(list = ls())

#install.packages('pacman')
x <- c('tidyverse','openxlsx','magrittr', 
       'stargazer', 'texreg')
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
get_robust_se <- function(i, producto){
  pluck(reg_precios, producto, 3, i) %>% 
  plm::vcovSCC() %>% 
  diag() %>% 
  sqrt()
}
price_regular_robust_se <- map(1:3, get_robust_se, 'regular')
price_premium_robust_se <- map(1:3, get_robust_se, 'premium')
price_diesel_robust_se <- map(1:3, get_robust_se, 'diesel')

tabla_regresion_precios <- function(producto, lista_se){
  if (producto == 'premium'){
    labs = c('Wholesale Price', 'log of Seller Density',
             'Distance to Nearest Competitor', 
             'Share of Sellers with the Same Brand in Relevant Market', 'Cargo Gas',
             'Conser de la Laguna', 'Hidrosina', 
             'Mi Gasolina', 'OXXO Gas', 'Pemex',
             'Petro 7', 'Progas', 'Punto Gas',
             'Convenience Store', 'ATM', 'Car Wash',
             'Boulevard', 'Highway', 'Street')
  }else{
    if(producto == "diesel"){
      labs = c('Wholesale Price', 'log of Seller Density',
               'Distance to Nearest Competitor', 
               'Share of Sellers with the Same Brand in Relevant Market',
               'Conser de la Laguna',  
               'Mi Gasolina', 'OXXO Gas', 'Pemex',
               'Petro 7', 'Progas', 'Punto Gas',
               'Convenience Store', 'ATM', 'Car Wash',
               'Boulevard', 'Highway', 'Street')
    }else{
      labs = c('Wholesale Price', 'log of Seller Density',
               'Distance to Nearest Competitor', 
               'Share of Sellers with the Same Brand in Relevant Market',
               'Conser de la Laguna', 'Hidrosina', 
               'Mi Gasolina', 'OXXO Gas', 'Pemex',
               'Petro 7', 'Progas', 'Punto Gas',
               'Convenience Store', 'ATM', 'Car Wash',
               'Boulevard', 'Highway', 'Street')
    }
    
  }
  stargazer(pluck(reg_precios, producto, 3, 1), 
            pluck(reg_precios, producto, 3, 2),
            pluck(reg_precios, producto, 3, 3),
            se = lista_se,
            title = str_c('Regression Results (',str_to_title(producto), ' Gasoline)'), 
            style = 'qje',
            column.labels = c('2017', '2018', '2019'),
            dep.var.labels = 'Retail Price $p_{it}$',
            align = FALSE,
            single.row = TRUE,
            out = str_c('data/price_',producto,'.tex'),
            covariate.labels = labs,
            omit = c('zip', 'Tor'),
            order = c('term', 'density', 'min_dist', 'same_brand_share'),
            digits = 3,
            label = str_c('preg_',producto),
            model.names = FALSE,
            no.space = TRUE,
            notes = 'Robust standard errors in parenthesis',
            omit.stat = c('f'))
}

tabla_regresion_precios('diesel', price_diesel_robust_se)
summary(pluck(reg_precios, 'diesel',3,1))

####### Dispersion Regressions ###########
load('data/regression_results_dispersion.RData')

texreg(l = list(diesel_2017, diesel_2018, diesel_2019),
       file = 'data/tex/dispersion_diesel.tex',
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
