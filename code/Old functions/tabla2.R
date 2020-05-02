tabla2 <- function(reg, list_reg, main_title){
  #this function provides a regression table in latex 
  # INPUT 
  # reg. list_reg = two regression models with 3 periods each
  # main_title
  # 
  # OUTPUT
  # regression table in latex
  
  
  cov1 <- vcovHC(reg,method = 'arellano')
  cov2 <- vcovHC(pluck(list_reg,1),method = 'arellano')
  cov3 <- vcovHC(pluck(list_reg,2),method = 'arellano')
  cov4 <- vcovHC(pluck(list_reg,3),method = 'arellano')
  
  rse1 <- sqrt(diag(cov1))
  rse2 <- sqrt(diag(cov2))
  rse3 <- sqrt(diag(cov3))
  rse4 <- sqrt(diag(cov4))
  
  stargazer(reg,pluck(list_reg,1),pluck(list_reg,2),pluck(list_reg,3),
            title = main_title, allign = T,
            se = list(rse1,rse2,rse3,rse4),
            column.labels = c('All Periods','t = 1','t = 2','t = 3'),
            dep.var.labels = 'Citywide Dispersion',
            order = c(1,10,11),
            covariate.labels = c('Density', 'Convenience Store','Same Brand Share',
                                 'Conser de la Laguna', 'Hidrosina', 'Migasolina', 
                                 'OxxoGas', 'Pemex', 'Petro 7', 'Progas','Puntogas'),
            omit = c('Road.Type','zipc'), omit.stat = c('LL','ser','f'),no.space = T,
            single.row = TRUE)
}
