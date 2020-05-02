fdistance<- function(ds){
  
  # Given a data set of gas stations with its coordinates, this function computes:
  # - Number of stations within 2 km.
  # - Distance to nearest station. (meters)
  # - Number of stations of the same Brand within 2 km. 
  
  ds$density<- 0
  ds$min_dist <- 100000
  ds$same_brand <- 0
  m <- dim(ds)[1]
  for (i in 1:(m-1)) {
    for(j in (i+1):m){
      aux<-distGeo(c(ds$longitude[i],ds$latitude[i]), c(ds$longitude[j],ds$latitude[j]))
      if (aux<=2000){
        ds$density[c(i,j)]= ds$density[c(i,j)] + c(1,1)
      }
      if(ds$Brand[i] == ds$Brand[j] & aux <= 2000){
        ds$same_brand[c(i,j)]= ds$same_brand[c(i,j)] + c(1,1)
      }
      if (aux< ds$min_dist[i]){
        ds$min_dist[i]<-aux
      }
      if (aux<ds$min_dist[j]){
        ds$min_dist[j]<-aux
      }
    }
  }
  return(ds)
} 