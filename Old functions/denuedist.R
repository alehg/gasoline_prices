denuedist <- function(gas, den){
  
  #Given a data set of gas stations and a data set of retail establishments this function
  #computes the number of retail establishments within a 2km. radius for each gas station
  
  m<-dim(gas)[1]
  n<-dim(den)[1]
  gas$est<-0
  
  for (i in 1:m) {
    for(j in 1:n){
      aux<-distGeo(c(gas$longitude[i],gas$latitude[i]), c(den$longitud[j],den$latitud[j]))
      if (aux<=2000){
        gas$est[i]=gas$est[i] + 1
      }
      
    }
  }
  return(gas)
  
  
}