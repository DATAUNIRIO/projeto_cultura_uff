library(stringr)

table(visuais$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_15)
table(visuais$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)

visuais$area_de_atuacao_cultura = tolower(visuais$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)
data.frame(table(visuais$area_de_atuacao_cultura))


d = data.frame(table(visuais$area_de_atuacao_cultura))
d$percentual = (d$Freq/sum(d$Freq))*100
d



library(ggplot2)
ggplot(d) +
  aes(x = reorder(Var1 , percentual), y = percentual) +
  geom_col(fill='#b02c57') +
  labs(x = "Área de Atuação", y = "Percentual", 
       subtitle = "Artes Visuais: Qual a sua principal área de atuação no campo da cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  coord_flip() + 
  ecoar_theme2()

ggsave("graficos_v4/entrega1_visual_area_atuacao.png",width = 20, height = 14, units = "cm")

remove(d)
#-----------------------------------------------------------------------------------------

table(espetaculo$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_15)
table(espetaculo$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)

espetaculo$area_de_atuacao = tolower(espetaculo$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)
table(espetaculo$area_de_atuacao)

d = data.frame(table(espetaculo$area_de_atuacao))
d$percentual = (d$Freq/sum(d$Freq))*100

d$percentual = round(d$percentual,2)
d

library(ggplot2)
ggplot(d) +
  aes(x = reorder(Var1 , percentual), y = percentual) +
  geom_col(fill='#b02c57') +
  labs(x = "Área de Atuação", y = "Percentual", 
       subtitle = "Espetáculo: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  coord_flip()+
  theme_minimal() +
  ecoar_theme2()

ggsave("graficos_v4/entrega1_espetaculo_area_atuacao.png",width = 20, height = 14, units = "cm")

remove(d) 













