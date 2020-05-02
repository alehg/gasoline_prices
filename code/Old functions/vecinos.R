vecinos <- function(gasdf){
  
  #Given  a gas station data set this function returns a list 
  # with the code of the neighbors of each station (station within a 2km radius)
  
  codes <- unique(gasdf$code)
  x<-t(combn(codes,2)) %>% #compute all code pairs and match with their coordinates
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    rename(code = V1) %>% 
    left_join(gasdf,by='code') %>% 
    select(code,V2,latitude,longitude) %>% 
    rename(code1=code,lat1 = latitude,lon1 = longitude,code=V2) %>% 
    left_join(gasdf,by='code') %>% 
    select(code1,lon1,lat1,code,longitude,latitude) %>% 
    rename(code2=code,lon2 = longitude,lat2 = latitude)

  l <- list(x$lon1,x$lat1,x$lon2,x$lat2) #list coordinates
  
  distancia<- function(lon1,lat1,lon2,lat2){ #function to compute distance for each pair of coordinates
    data.frame(d=distGeo(c(lon1,lat1),c(lon2,lat2)))
  }

  dist <- pmap_dfr(l,distancia) #data frame with distance variable
  x <- cbind(x,dist)
  x %<>% filter(d<=2000) #we filter to get only neighbors

  vec <- function(c,x){ #function to get the codes of the neighbors for each code c
    x %<>% filter(code1 == c | code2 == c)
    v <- setdiff(c(x$code1,x$code2),c) 
    l <- list(c,v)
    return(l)
  }
  
  da <- map(codes,vec,x) #compute all neighbors
  
  return(da)
}

