
table(espetaculo$trabalho_semanal_NAO_area_cultural,espetaculo$trabalho_semanal_na_area_cultural)

espetaculo %>% select(trabalho_semanal_NAO_area_cultural,trabalho_semanal_na_area_cultural,voce_trabalha_exclusivamente_na_area_cultural) %>% table()

d = espetaculo %>% select(trabalho_semanal_NAO_area_cultural,trabalho_semanal_na_area_cultural)

d = d %>% group_by(trabalho_semanal_na_area_cultural,trabalho_semanal_NAO_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
d

is.na(d$trabalho_semanal_NAO_area_cultural)
d$trabalho_semanal_NAO_area_cultural = sub("NA", 'Trabalho exclusivo na área cultural', paste(d$trabalho_semanal_NAO_area_cultural))

d
d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = trabalho_semanal_na_area_cultural, y = freq, fill = trabalho_semanal_NAO_area_cultural) +
  geom_col() +
  scale_fill_manual(values = c("#25b887","#327fc7","#b02c57"),name = "Carga de trabalho semanal") +
  labs(x = "Carga de trabalho semanal na área cultural", y = "Percentual", 
       subtitle = "Artes do Espetáculo: Carga de trabalho semanal na área cultural e em \noutras áreas", 
       caption = "Fonte: ECOA", 
       fill =  "Carga de trabalho semanal em outra área") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ecoar_theme2()

g1

ggsave("graficos_v7/entrega2_espetaculo_carga_horaria.png",width = 28, height = 16, units = "cm")
remove(d,g1) 

#------------------------------------------------------------------------



table(visuais$trabalho_semanal_NAO_area_cultural,visuais$trabalho_semanal_na_area_cultural)

visuais %>% select(trabalho_semanal_NAO_area_cultural,trabalho_semanal_na_area_cultural,voce_trabalha_exclusivamente_na_area_cultural) %>% table()

e = visuais %>% select(trabalho_semanal_NAO_area_cultural,trabalho_semanal_na_area_cultural)

e = e %>% group_by(trabalho_semanal_na_area_cultural,trabalho_semanal_NAO_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
e

is.na(e$trabalho_semanal_NAO_area_cultural)
e$trabalho_semanal_NAO_area_cultural = sub("NA", 'Trabalho exclusivo na área cultural', paste(e$trabalho_semanal_NAO_area_cultural))

e$freq = e$freq*100


g2 = ggplot(e) +
  aes(x = trabalho_semanal_na_area_cultural, y = freq, fill = trabalho_semanal_NAO_area_cultural) +
  geom_col() +
  scale_fill_manual(values = c("#25b887","#327fc7","#b02c57"),name = "Carga de trabalho semanal em outra área") +
  labs(x = "Carga de trabalho semanal na área cultural", y = "Percentual", 
       subtitle = "Artes Visuais: Carga de trabalho semanal na área cultural \ne em outras áreas", 
       caption = "Fonte: ECOA", 
       fill =  "Carga de trabalho semanal em outra área") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ecoar_theme2()

g2

ggsave("graficos_v7/entrega2_visuais_carga_horaria.png",width = 28, height = 16, units = "cm")
remove(e,g2) 

