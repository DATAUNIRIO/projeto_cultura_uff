

library(geojsonio)
bairros_niteroi <- geojson_read("C:/Users/Meu Computador/Documents/GitHub/projeto_cultura_uff/mapas/Limite_de_Bairros.geojson",  what = "sp")

bairros_niteroi2 = bairros_niteroi@data
names(bairros_niteroi)

library(readxl)
library(janitor)
AP <- read_excel("~/GitHub/projeto_cultura_uff/mapas/AP.xlsx") %>% clean_names()
names(AP)


library(dplyr)
AP = AP %>% rename(tx_nome=bairro)

bairros_niteroi2 = bairros_niteroi2 %>% left_join(AP)


bairros_niteroi = bairros_niteroi %>% left_join(bairros_niteroi2)

bairros_niteroi@data = bairros_niteroi@data %>% left_join(bairros_niteroi2)






