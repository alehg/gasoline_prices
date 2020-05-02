regresiontw <- function(data,formulita){
  # This function runs a two way fixed effects regression given a data frame
  # with specific id and time indeces, namely "code" and "cutoff_datetime"
  #Returns a list with 4 objects, the regression over the whole period of study, and
  # the regression of the 3 subperiods.
  
  reglist <- list()
  for(i in 1:3){
    reglist[[i]]<- plm(formulita, filter(data,period==i), effect = 'time', model = 'within', 
                      index = c('code','date'))
  }
  reglist[[4]]<- plm(formulita, data, effect = 'time', model = 'within', 
                   index = c('code','date'))
  return(reglist)
}