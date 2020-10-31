
library(stargazer)


get_robust_se <- function(i, producto){
  pluck(aux_reg_precios, producto, 3, i) %>% 
    plm::vcovSCC() %>% 
    diag() %>% 
    sqrt()
}
price_regular_robust_se <- map(1:3, get_robust_se, 'regular')
price_premium_robust_se <- map(1:3, get_robust_se, 'premium')
price_diesel_robust_se <- map(1:3, get_robust_se, 'diesel')

tabla <- function(producto, lista_se){
  stargazer(pluck(aux_reg_precios, producto, 3, 1), 
            pluck(aux_reg_precios, producto, 3, 2),
            pluck(aux_reg_precios, producto, 3, 3),
            se = lista_se,
            title = str_c('Regression Results (',str_to_title(producto), ' Gasoline)'), 
            style = 'qje',
            column.labels = c('2017', '2018', '2019'),
            dep.var.labels = 'Retail Price $p_{it}$',
            align = FALSE,
            single.row = TRUE,
            out = str_c('data/price_',producto,'.tex'),
            omit = c('zip', 'Tor'),
            order = c('term', 'density', 'min_dist', 'same_brand_share'),
            digits = 3,
            label = str_c('preg_',producto),
            model.names = FALSE,
            no.space = TRUE,
            notes = 'Robust standard errors in parenthesis',
            omit.stat = c('f'))
}
tabla('regular', price_regular_robust_se)
