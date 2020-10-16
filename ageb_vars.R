rm(list=ls())
library(tidyverse)
library(openxlsx)
library(readxl)

load('data/denue.RData')
str_c(gasocl1$latitude, gasocl1$longitude, sep=', ')

# extraer estado y ageb, descartar agebs rurales y hacer merge con selección
# de variables del censo. Población, escolaridad, PEA, personas con carro.
agebs <- read.xlsx('data/ageb_by_station.xlsx') %>% 
  mutate(rural = if_else(nchar(ageb) < 10, 1, 0),
         estado = if_else(str_extract(ageb, "^.{2}") %in% c('50', '05'), '05', '10'),
         ageb = ifelse(rural ==1, NA, str_sub(ageb,-4)))


# Data del censo 2010
coahuila_ageb_data <- read_excel('data/AGEB/RESAGEBURB_05XLS10.xls')
durango_ageb_data <- read_excel('data/AGEB/RESAGEBURB_10XLS10.xls')
ageb_data <- bind_rows(coahuila_ageb_data, durango_ageb_data) %>% 
  select(estado = ENTIDAD, AGEB, POBTOT, GRAPROES, PEA, VPH_AUTOM) 
names(ageb_data) <- tolower(names(ageb_data))
ageb_data_grouped <- ageb_data %>% 
  mutate(graproes = ifelse(graproes == '*', NA, graproes),
         pea = ifelse(pea == '*', NA, pea),
         vph_autom = ifelse(vph_autom == '*', NA, vph_autom)) %>% 
  mutate_at(vars(pobtot, graproes, pea, vph_autom), as.numeric) %>% 
  group_by(estado, ageb) %>%
  summarise(graproes = weighted.mean(graproes, pobtot, na.rm = TRUE), 
            pobtot = sum(pobtot, na.rm = TRUE),
            pea = sum(pea, na.rm = TRUE),
            vph_autom = sum(vph_autom, na.rm = TRUE))
ageb_final <- agebs %>% left_join(ageb_data_grouped, by=c('estado', 'ageb'))  
saveRDS(ageb_final, file='data/ageb_final.RDS')
