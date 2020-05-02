quantplot<- function(producto,price_df){
  # This function returns an object that represents the quartile plot of prices 
  # from a given database da
  
  probas <- c(0.05, .25,.5,.75,.95)

    da <- price_df %>% 
      filter(product == producto) %>% 
      group_by(date) %>% 
      summarise(q0 = quantile(price_end,probas)[1],
                q1 = quantile(price_end,probas)[2],
                q2 = quantile(price_end,probas)[3],
                q3 = quantile(price_end,probas)[4],
                q4 = quantile(price_end,probas)[5]) %>% 
      mutate_if(is.factor, as.character) %>% 
      gather(key = 'variable',value = 'value',q0:q4) 
  
  
  
  
  
  g<-ggplot(data=da, aes(x=date, y=value,color = variable)) +
    geom_line(aes(size = variable, linetype = variable)) +
    geom_vline(xintercept = as.numeric(as.Date(c('2018-01-01','2019-01-01')))) +
    labs(y = 'Pesos per Liter') +
    scale_size_manual(name = 'Price quantiles', 
                          labels = c('5%','25%','50%','75%','95%'), 
                          values = c(0.8,0.8,1.2,0.8,0.8)) +
    scale_color_manual(name = 'Price quantiles', 
                       labels = c('5%','25%','50%','75%','95%'),
                       values = c('#434141','#989797','#000000','#989797','#434141')) + 
    scale_linetype_manual(name = 'Price quantiles', 
                          labels = c('5%','25%','50%','75%','95%'),
                          values = c('solid','dashed','solid','dashed','solid')) +
    scale_x_date(breaks = seq.Date(as.Date('2017-07-01'),as.Date('2019-07-01'), by = '3 months'),
                 date_labels = '%b %y') +
    scale_y_continuous(labels = dollar_format(prefix = "$", accuracy = 0.5))
  
  
  return(g)
  
}