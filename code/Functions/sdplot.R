sdplot<- function(producto,price_df){
  
  # This function returns an object that represents the standard deviation plot of prices 
  # from a given database da and a type of product
  
  da <- price_df %>% 
    filter(product == producto) %>% 
    group_by(date) %>% 
    summarise(ed = mean(sd))
  
  # da %<>% mutate(cutoffd = as.Date(substr(cutoff_datetime,1,10)))
  
  g<-ggplot(data=da, aes(x=date, y=ed)) +
    geom_line(size = 0.8)+ 
    geom_vline(xintercept = as.numeric(as.Date(c('2018-01-01','2019-01-01')))) + 
    labs(y = 'Pesos per Liter') + 
    scale_x_date(breaks = seq.Date(as.Date('2017-07-01'),
                                   as.Date('2019-07-01'), 
                                   by = '3 months'),
                 date_labels = '%b. %y') +
    scale_y_continuous(labels = dollar_format(prefix = "$", accuracy = 0.05))
  
  return(g)
  
}