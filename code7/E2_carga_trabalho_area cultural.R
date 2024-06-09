library(ggplot2)
table(espetaculo$carga_de_trab_area_cultural)

d = espetaculo %>% select(carga_de_trab_area_cultural,como_voce_se_identifica_em_relacao_a_raca)
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')

d = d %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,carga_de_trab_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = carga_de_trab_area_cultural) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Qual é a sua carga de trabalho semanal na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v7/entrega2_espetaculo_carga_trab_cor.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------
table(visuais$carga_de_trab_area_cultural,visuais$como_voce_se_identifica_em_relacao_a_raca)


e = visuais %>% select(carga_de_trab_area_cultural,como_voce_se_identifica_em_relacao_a_raca)
e = e %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
e = e %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')

e = e %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,carga_de_trab_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = carga_de_trab_area_cultural) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Qual é a sua carga de trabalho semanal na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v7/entrega2_visuais_carga_trab_cor.png",width = 20, height = 14, units = "cm")

library(ggpubr)
figure = ggarrange(g1, g2, 
                   labels = c("Espetáculo", "Artes Visuais"),
                   font.label = list(size = 12, color = "#b02c57", face = "bold"),
                   ncol = 1, nrow = 2,
                   common.legend = TRUE, legend = "bottom")
#figure+ guides(fill=guide_legend(nrow = 2,byrow = TRUE))

annotate_figure(figure,
                top = text_grob("Qual é a sua carga de trabalho semanal na área cultural?", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

ggsave("graficos_v7/entrega2_carga_traab_cor.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)

#----------------------------------------------------------------------------------------------
# Genero
library(ggplot2)

d = espetaculo %>% select(carga_de_trab_area_cultural,como_voce_se_identifica_em_relacao_ao_genero)
table(d$como_voce_se_identifica_em_relacao_ao_genero)

d = d %>%   group_by(como_voce_se_identifica_em_relacao_ao_genero,carga_de_trab_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_ao_genero, y = freq, fill = carga_de_trab_area_cultural) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Qual é a sua carga de trabalho semanal na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v7/entrega2_espetaculo_carga_trab_genero.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------
table(visuais$carga_de_trab_area_cultural,visuais$como_voce_se_identifica_em_relacao_ao_genero)


e = visuais %>% select(carga_de_trab_area_cultural,como_voce_se_identifica_em_relacao_ao_genero)
e = e %>%   group_by(como_voce_se_identifica_em_relacao_ao_genero,carga_de_trab_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100
e = e %>% filter(!is.na(como_voce_se_identifica_em_relacao_ao_genero))


g2 = ggplot(e) +
  aes(x = como_voce_se_identifica_em_relacao_ao_genero, y = freq, fill = carga_de_trab_area_cultural) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Qual é a sua carga de trabalho semanal na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v7/entrega2_visuais_carga_trab_genero.png",width = 20, height = 14, units = "cm")

library(ggpubr)
figure = ggarrange(g1, g2, 
                   labels = c("Espetáculo", "Artes Visuais"),
                   font.label = list(size = 12, color = "#b02c57", face = "bold"),
                   ncol = 1, nrow = 2,
                   common.legend = TRUE, legend = "bottom")
#figure+ guides(fill=guide_legend(nrow = 2,byrow = TRUE))

annotate_figure(figure,
                top = text_grob("Qual é a sua carga de trabalho semanal na área cultural?", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

ggsave("graficos_v7/entrega2_carga_trab_genero.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)

#--------------------------------------------------------------------------------------
# idade

library(ggplot2)

d = espetaculo %>% select(carga_de_trab_area_cultural,faixa_idade)

d = d %>%   group_by(faixa_idade,carga_de_trab_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = faixa_idade, y = freq, fill = carga_de_trab_area_cultural) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Qual é a sua carga de trabalho semanal na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v7/entrega2_espetaculo_carga_trab_idade.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------
table(visuais$carga_de_trab_area_cultural,visuais$faixa_idade)


e = visuais %>% select(carga_de_trab_area_cultural,faixa_idade)
e = e %>%   group_by(faixa_idade,carga_de_trab_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = faixa_idade, y = freq, fill = carga_de_trab_area_cultural) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Qual é a sua carga de trabalho semanal na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v7/entrega2_visuais_carga_trab_idade.png",width = 20, height = 14, units = "cm")

library(ggpubr)
figure = ggarrange(g1, g2, 
                   labels = c("Espetáculo", "Artes Visuais"),
                   font.label = list(size = 12, color = "#b02c57", face = "bold"),
                   ncol = 1, nrow = 2,
                   common.legend = TRUE, legend = "bottom")
#figure+ guides(fill=guide_legend(nrow = 2,byrow = TRUE))

annotate_figure(figure,
                top = text_grob("Qual é a sua carga de trabalho semanal na área cultural?", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

ggsave("graficos_v7/entrega2_carga_trab_idade.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)

#----------------------------------------------------------------------------------------------
# tempo

library(ggplot2)
table(espetaculo$carga_de_trab_area_cultural)

d = espetaculo %>% select(carga_de_trab_area_cultural,ha_quanto_tempo_voce_atua_nessa_area)

d = d %>%   group_by(ha_quanto_tempo_voce_atua_nessa_area,carga_de_trab_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = ha_quanto_tempo_voce_atua_nessa_area, y = freq, fill = carga_de_trab_area_cultural) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Qual é a sua carga de trabalho semanal na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v7/entrega2_espetaculo_carga_trab_tempo.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------
table(visuais$carga_de_trab_area_cultural,visuais$ha_quanto_tempo_voce_atua_nessa_area)


e = visuais %>% select(carga_de_trab_area_cultural,ha_quanto_tempo_voce_atua_nessa_area)

e = e %>%   group_by(ha_quanto_tempo_voce_atua_nessa_area,carga_de_trab_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100
e
g2 = ggplot(e) +
  aes(x = ha_quanto_tempo_voce_atua_nessa_area, y = freq, fill = carga_de_trab_area_cultural) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Qual é a sua carga de trabalho semanal na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v7/entrega2_visuais_carga_trab_tempo.png",width = 20, height = 14, units = "cm")

library(ggpubr)
figure = ggarrange(g1, g2, 
                   labels = c("Espetáculo", "Artes Visuais"),
                   font.label = list(size = 12, color = "#b02c57", face = "bold"),
                   ncol = 1, nrow = 2,
                   common.legend = TRUE, legend = "bottom")
#figure+ guides(fill=guide_legend(nrow = 2,byrow = TRUE))

annotate_figure(figure,
                top = text_grob("Qual é a sua carga de trabalho semanal na área cultural?", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

ggsave("graficos_v7/entrega2_carga_trab_tempo.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)


#----------------------------------------------------------------------------------------------
# AP

library(ggplot2)
table(espetaculo$carga_de_trab_area_cultural)

d = espetaculo %>% select(carga_de_trab_area_cultural,regiao)
d = d %>% filter(!is.na(regiao))
d = d %>%   group_by(regiao,carga_de_trab_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = regiao, y = freq, fill = carga_de_trab_area_cultural) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Qual é a sua carga de trabalho semanal na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v7/entrega2_espetaculo_carga_trab_AP.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------
table(visuais$carga_de_trab_area_cultural,visuais$regiao)


e = visuais %>% select(carga_de_trab_area_cultural,regiao)
e = e %>% filter(!is.na(regiao))
e = e %>%   group_by(regiao,carga_de_trab_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = regiao, y = freq, fill = carga_de_trab_area_cultural) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Qual é a sua carga de trabalho semanal na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v7/entrega2_visuais_carga_trab_AP.png",width = 20, height = 14, units = "cm")

library(ggpubr)
figure = ggarrange(g1, g2, 
                   labels = c("Espetáculo", "Artes Visuais"),
                   font.label = list(size = 12, color = "#b02c57", face = "bold"),
                   ncol = 1, nrow = 2,
                   common.legend = TRUE, legend = "bottom")
#figure+ guides(fill=guide_legend(nrow = 2,byrow = TRUE))

annotate_figure(figure,
                top = text_grob("Qual é a sua carga de trabalho semanal na área cultural?", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

ggsave("graficos_v7/entrega2_carga_trab_AP.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)