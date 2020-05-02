terminalplot <- function(producto,price_df){
  
  # This function given a price dataframe 
  # and a product, returns a plot of the terminal prices over time
  
  da <- price_df %>% 
    filter(product == producto) %>% 
    group_by(date) %>% 
    summarise(p = mean(price_term)) %>% 
    ungroup()
  
  g <- ggplot(da, aes(x = date, y = p)) +
    geom_line(size = 1) +
    geom_vline(xintercept = as.numeric(as.Date(c('2018-01-01','2019-01-01')))) +
    labs(y = 'Pesos per Liter') +
    scale_x_date(breaks = seq.Date(as.Date('2017-07-01'),
                                   as.Date('2019-07-01'), 
                                   by = '3 months'),
                 date_labels = '%b %y') +
    scale_y_continuous(labels = dollar_format(prefix = "$", accuracy = 0.5))
  
}