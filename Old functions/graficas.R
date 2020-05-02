graficas <-function(listdf){
  
  # For each price data frame in the list, this function creates the quantile plot and 
  # the standard deviation plot for prices and returns a list of lists of two elements.
  
  simpleCap <- function(x) {#function that returns the first letter capitalized 
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  } 
  
  grafs <- function(pricedf){
    
    qp <- quantplot(pricedf) + #quantile plot
      ggtitle(str_c(' Price Quantiles (La Laguna: ',simpleCap(unique(pricedf$product)),')'))
    
    sdp <- sdplot(pricedf) + #standard deviation plot
      ggtitle(str_c('Standard Deviation (La Laguna: ',simpleCap(unique(pricedf$product)),')'))
    
    l <- list(qp,sdp)
    return(l)
  }
  
  lista <- map(listdf,grafs)
  return(lista)
}