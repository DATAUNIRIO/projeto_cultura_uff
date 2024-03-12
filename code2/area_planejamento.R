library(janitor)
library(readxl)
AP <- read_excel("mapas/AP.xlsx") %>% clean_names()
head(AP)

visuais = visuais %>% rename(bairro=qual_seu_bairro)
table(visuais$bairro)

visuais = visuais %>% left_join(AP)

table(visuais$regiao)
table(AP$regiao)

AP %>% filter(regiao=='Região Leste') %>% print()


table(visuais$regiao)
