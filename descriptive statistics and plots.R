rm(list=ls())
# Here I will make some quantile and standard deviation plots 

x <- c('tidyverse','openxlsx','geosphere',
       'magrittr','scales','readxl','extrafont',
       'fastDummies','treemapify','lubridate',
       'corrplot','stats')
lapply(x, library, character.only = TRUE)

dir <- '/Functions/'
funs <- list.files(paste0(getwd(),dir))
lapply(str_c(getwd(),dir,funs),source)

load('data/clean_prices.RData')

Sys.setlocale(locale = 'English')
 # font_install('font_cm')
theme_tesis <- theme_minimal() +
  theme(text = element_text(family = 'Arial Unicode MS'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y =  element_text(size = 12),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 13,hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        axis.text =element_text(size=10),
        legend.position = 'top',
        legend.text = element_text(size = 10))

theme_set(theme_tesis)

azules <- c("#023876","#045DC3","#318DF6") #azules de oscuro a claro
naranja <- c("#C56403")
verdes <-c('#044A26','#006D34')
gris <- c('#858585')

aux_reg <- function(producto,price_df){
  df <- price_df %>% filter(product == producto)
  modelito <- lm(price_end ~ factor(code) + factor(date),data = df)
  u_hat <- resid(modelito)
  df <- df %>% mutate(uhat = u_hat,disp = log(uhat^2))
  return(list(df,modelito))
}
auxreg_list <- map(unique(clean_prices$product),aux_reg,clean_prices)
final_prices <- map_dfr(1:3,function(i,lista) pluck(lista,i,1),auxreg_list)
save(final_prices,file='data/final_prices.RData')
save(auxreg_list,file='data/auxreg_results.RData')

#Standard Deviation Plots 
sd_plots <- map(unique(final_prices$product),sdplot,final_prices)
names(sd_plots) <- unique(final_prices$product)

# Quantile Plots
quant_plots <- map(unique(final_prices$product),quantplot,final_prices)
names(quant_plots) <- unique(final_prices$product)

# dividir en anios, hacer graficas para precios de terminal, determinar si la volatilidad y
# la caida en precios en 2019 se debe a politicas fiscales 

# Terminal Prices
term_plots <- map(unique(final_prices$product),terminalplot,final_prices)
names(term_plots) <- unique(final_prices$product)

# Why do terminal prices differ that much between products?

Sys.setlocale(locale="")


######## Descriptive statistics #######
# Stations 
tabla <- lst(avg = mean,sd = sd,min = min,max = max) %>% 
  map(~ final_prices %>% 
  dummy_cols(select_columns = c('Road.Type','Brand')) %>% 
  summarise_at(vars(density,same_brand,same_brand_share,min_dist,
                    Convenience.Store,ATM,contains('Road.Type_'),contains('Brand_')),.x,na.rm = T) %>% 
  ungroup() %>% gather('var','value')) %>% 
  reduce(inner_join,by = 'var') %>% 
  rename(mean = value.x,sd = value.y,min = value.x.x, max = value.y.y)
  
# How many stations per municipalities?
# mode function
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
# final_prices %>% 
#   group_by(code) %>% summarise(muni = getmode(muni_name)) %>% 
#   group_by(muni) %>% summarise(stations = n()) %>% ungroup() 

# 97 in Torreon (68.3%)
# 45 in Gomez Palacio (31.7%)

# Distribution of road type
road_type <- tabla %>% filter(var %>% str_detect('Road')) %>% 
  mutate(var = str_replace(var,'Road.Type_','') %>% str_to_title()) %>% 
  select(var,mean) %>% 
  ggplot(aes(area = mean,fill = var)) + 
  geom_treemap(stat = 'identity') +
  geom_treemap_text(aes(label = var),place = 'centre')+
  scale_fill_manual(values = c(azules[2:3],verdes[2],gris,naranja))+
  theme(legend.position = 'none') +
  labs(title = 'Distribution of stations by Road Type')
  
# Distribution of brands
brand <- tabla %>% filter(var %>% str_detect('Brand')) %>% 
  mutate(var = str_replace(var,'Brand_','') %>% str_to_title()) %>% 
  select(var,mean) %>% 
  arrange(desc(mean)) %>% 
  ggplot(aes(x = reorder(var,mean),y = mean)) + 
  geom_col()+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.title.y = element_blank()) + 
  labs(title = 'Distribution of Stations by Brand')

# Distribution of densities 
density_hist <-final_prices %>% group_by(code) %>% 
  summarise(density = round(mean(density))) %>%  
  group_by(density) %>% summarise(n = n()) %>% 
  ggplot(aes(x = density,y = n)) + geom_col() +
  labs(title = 'Histogram of Density')


graficas_densidad <- function(prices_df){
  # Density by muni
  muni <- prices_df %>% 
    ggplot(aes(x = muni_name,y = density,fill = muni_name)) + 
    geom_boxplot(show.legend = F)
  
  # Boxplot: Road Type vs Density
  road_type <- prices_df %>% 
    ggplot(aes(x = Road.Type,y = density,fill = Road.Type)) + 
    geom_boxplot(show.legend = F)
  
  # Boxplot: Convenience vs Density
  conv <- prices_df %>% 
    ggplot(aes(x = as.character(Convenience.Store),y = density,fill = as.character(Convenience.Store))) + 
    geom_boxplot(show.legend = F)
  
  # Boxplot: Brand vs Density
  brand <- prices_df %>% 
    ggplot(aes(x = Brand,y = density,fill = Brand)) + 
    geom_boxplot(show.legend = F)
  
  return(list(muni = muni, road_type = road_type, convenience = conv,brand = brand))
}
density_plots <- graficas_densidad(final_prices)

graficas_precios <- function(producto,price_df){
  # Boxplot: Price vs. Density 
  den <- price_df %>% 
    mutate(den_cat = if_else(density <= 14,'low','high')) %>% 
    filter(product == producto) %>% 
    ggplot(aes(x = as.character(year),y = price_end,fill = den_cat)) + 
    geom_boxplot()
  
  
  # Boxplot: Price vs Convenience Store 
  conv <- price_df %>% 
    filter(product == producto) %>% 
    ggplot(aes(x = as.character(year),y = price_end,fill = as.character(Convenience.Store))) + 
    geom_boxplot()
  
  # Boxplot: Price vs Road.Type 
  road <- price_df %>% 
    filter(product == producto) %>% 
    ggplot(aes(x = as.character(year),y = price_end,fill = Road.Type)) + 
    geom_boxplot()
  
  
  # Boxplot: Price vs. Brand 
  brand <- price_df %>% 
    filter(product == producto) %>% 
    ggplot(aes(x = as.character(year),y = price_end,fill = Brand)) + 
    geom_boxplot()
  
  return(list(density = den,convenience = conv, road = road,brand = brand))
}
price_plots <- map(unique(final_prices$product),graficas_precios,final_prices)
names(price_plots) <- unique(final_prices$product)

graficas_correlaciones <- function(producto,price_df){
 
  first <- price_df %>% 
    filter(date <= '2017-12-31',product == producto) %>% 
    select(price_end,price_term,sd,disp,density,min_dist,Convenience.Store,same_brand_share) %>% 
    cor(use = "pairwise.complete.obs") %>% corrplot(method = 'color', addCoef.col = 'black')
  
  second <- price_df %>% 
    filter(date > '2017-12-31',date <= '2018-12-31',product == producto) %>% 
    select(price_end,price_term,sd,disp,density,min_dist,Convenience.Store,same_brand_share) %>% 
    cor(use = "pairwise.complete.obs") %>% corrplot(method = 'color',addCoef.col = 'black')
  
  third <- price_df %>% 
    filter(date > '2018-12-31',product == producto) %>% 
    select(price_end,price_term,sd,disp,density,min_dist,Convenience.Store,same_brand_share) %>% 
    cor(use = "pairwise.complete.obs") %>% corrplot(method = 'color', addCoef.col = 'black')
  return(list(first = first, second = second, third = third))
}
correlation_plots <- map(unique(final_prices$product),graficas_correlaciones,final_prices)
names(correlation_plots) <- unique(final_prices$product)

grafica_dispersion <- function(producto,price_df){
  
  price_df %>% 
    filter(product == producto) %>% 
    group_by(date) %>% 
    summarise_at(vars(disp),mean) %>% 
    ungroup() %>% 
    ggplot(aes(x = date, y = disp)) +
    geom_line(size = 1) + 
    geom_vline(xintercept = as.numeric(as.Date(c('2018-01-01','2019-01-01')))) + 
    scale_x_date(breaks = seq.Date(as.Date('2017-07-01'),as.Date('2019-07-01'), by = '3 months'),
                 date_labels = '%b %y')
  
}

dispersion_plots <- map(unique(final_prices$product),grafica_dispersion,final_prices)
names(dispersion_plots) <- unique(final_prices$product)

# Tabla precios
# 2017
tabla_precios <- final_prices %>% 
  select(date,year,product,price_end,price_term) %>% 
  pivot_longer(cols = c('price_end','price_term'),
               names_to = c('tipo'),
               values_to = 'value') %>% 
  group_by(year,product,tipo) %>% 
  summarise(avg_price = mean(value),
            sd_price = sd(value)) %>% 
  ungroup()

save(sd_plots,quant_plots,term_plots,tabla,road_type,brand,
     density_hist,density_plots,price_plots,
     correlation_plots,dispersion_plots, tabla_precios,file='data/plots.RData')




