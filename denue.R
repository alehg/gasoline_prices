library(inegiR)
token <- "90ba6ba7-5d92-43e9-bfca-bad73c65d4e1"

gasot <- read.xlsx('data/gast.xlsx',1) %>%  #load data from google maps
  mutate(muni_name = 'Torreon') 

gasogp <- read.xlsx('data/gasgp.xlsx',1) %>%  #gomez palacio
  mutate(muni_name = 'Gomez Palacio') 

gasocl <- rbind(gasot,gasogp) %>% #gasolineras comarca lagunera
  mutate(Brand = ifelse(Migasolina == 1,'migasolina',trimws(Brand)),
         #periferico  as highway
         Road.Type = ifelse(Road.Type == 'periferico','highway',as.character(Road.Type)),
         latitude = as.numeric(as.character(latitude)),
         longitude = as.numeric(as.character(longitude)),
         code = as.character(code)) %>% 
  filter(code %in% prices0$code)

rm(gasot,gasogp) 

itera_denue <- function(latitud, longitud){
  inegi_denue(latitud, longitud, token, meters=2000)
}

denue_est <- map2(gasocl$latitude, gasocl$longitude, itera_denue) 
denue_num <- map(denue_est,nrow)

denue_col <- data.frame(num_est = matrix(unlist(denue_num), nrow=length(denue_num), byrow=T))
gasocl1 <- gasocl %>% bind_cols(denue_col)
prices1 <- prices0 %>% left_join(gasocl1 %>% select(code, num_est), by = 'code')
