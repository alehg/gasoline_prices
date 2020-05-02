tabla <- function(reg1,reg2,maintitle){
  #this function provides a regression table in latex 
  # INPUT 
  # reg1,reg2 = two regression models with 3 periods each
  # maintitle
  # title1,title2 = corresponding titles of regression models
  # OUTPUT
  # regression table in latex
  
  d1 <- 'log(density + 1)'
  d2 <- 'log(Density)'
  if(str_detect(maintitle,'Price')){
    title1 <- 'Price'
    title2 <- 'Local Deviation'
  }else{
    title1 <- 'Citywide Dispersion'
    title2 <- 'Local Dispersion'
  }
 
    cov1 <- vcovHC(reg1[[1]],method = 'white1')
    cov2 <- vcovHC(reg1[[2]],method = 'white1')
    cov3 <- vcovHC(reg1[[3]],method = 'white1')
    cov4 <- vcovHC(reg2[[1]],method = 'white1')
    cov5 <- vcovHC(reg2[[2]],method = 'white1')
    cov6 <- vcovHC(reg2[[3]],method = 'white1')
    rse1 <- sqrt(diag(cov1))
    rse2 <- sqrt(diag(cov2))
    rse3 <- sqrt(diag(cov3))
    rse4 <- sqrt(diag(cov4))
    rse5 <- sqrt(diag(cov5))
    rse6 <- sqrt(diag(cov6))
    stargazer(reg1[[1]],reg1[[2]],reg1[[3]], reg2[[1]],reg2[[2]],reg2[[3]],
              title = maintitle, allign = T,
              se = list(rse1,rse2,rse3,rse4,rse5,rse6),
              column.labels = c('t = 1', 't = 2', 't = 3','t = 1','t = 2','t = 3'),
              dep.var.labels = c(title1,title2),
              order = c(d1,'Convenience.Store','Car.Wash','min_dist','same_brand_share'),
              covariate.labels = c(d2, 'Convenience Store', 'Car Wash', 
                                   'Min. Distance to Competitor','Same Brand Share',
                                   'Conser de la Laguna', 'Hidrosina', 'Migasolina', 
                                   'OxxoGas', 'Pemex', 'Petro 7', 'Progas','Puntogas'),
              omit = 'Road.Type', omit.stat = c('LL','ser','f'),no.space = T)
  
    cov1 <- vcovHC(reg1[[4]],method='white1')
    cov2 <- vcovHC(reg2[[4]], method = 'white1')
    rse1 <- sqrt(diag(cov1))
    rse2 <- sqrt(diag(cov2))
    stargazer(reg1[[4]], reg2[[4]], title = maintitle, allign = T,
              se = list(rse1,rse2), dep.var.labels = c(title1,title2),
              order = c(d1,'Convenience.Store','Car.Wash','min_dist','same_brand_share'),
              covariate.labels = c(d2, 'Convenience Store', 'Car Wash', 
                                   'Min. Distance to Competitor','Same Brand Share',
                                   'Conser de la Laguna', 'Hidrosina', 'Migasolina', 
                                   'OxxoGas', 'Pemex', 'Petro 7', 'Progas','Puntogas'),
              omit = 'Road.Type', omit.stat = c('LL','ser','f'),no.space = T)
  
  
}