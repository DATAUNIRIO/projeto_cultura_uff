source('/home/steven/Documentos/GitHub/projeto_cultura_uff/code8/carregar_as_bases.R')

library(ggplot2)

d = espetaculo %>% select(renda,como_voce_se_identifica_em_relacao_a_raca)
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')
d = d %>% filter(!is.na(renda))
#d = d %>% filter(renda!='Não sei / Não quero responder')
d = d %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100


g1 = ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Qual a média da sua renda individual mensal levando em conta \nos salários anteriores a pandemia?" , 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega4_espetaculo_renda_cor.png",width = 26, height = 18, units = "cm")
remove(d) 

#------------------------------------------------------------------------

e = visuais %>% select(renda,como_voce_se_identifica_em_relacao_a_raca)
e = e %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
e = e %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')
e = e %>% filter(!is.na(renda))
e = e %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Qual é a média da sua renda individual mensal?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega4_visuais_renda_cor.png",width = 20, height = 14, units = "cm")

library(ggpubr)

#g1 = g1 + theme(legend.spacing.x = unit(0.8, "cm"))
#g2 = g2 + theme(legend.spacing.x = unit(0.8, "cm"))

figure = ggarrange(g1, g2, 
                   labels = c("Espetáculo", "Artes Visuais"),
                   font.label = list(size = 12, color = "#b02c57", face = "bold"),
                   ncol = 1, nrow = 2,
                   common.legend = TRUE, legend = "bottom")

annotate_figure(figure,
                top = text_grob(" ", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

#library(gridGraphics)
#library(patchwork)
#wrap_elements(~print(g1)) + wrap_elements(~print(g2))


ggsave("graficos_v8/entrega4_renda_cor.png",width = 28, height = 20, units = "cm")

remove(d,e)
remove(g1,g2)
remove(figure)

#----------------------------------------------------------------------------------------------
# Genero
library(ggplot2)

d = espetaculo %>% select(renda,como_voce_se_identifica_em_relacao_ao_genero)
d = d %>% filter(!is.na(renda))

d = d %>%   group_by(como_voce_se_identifica_em_relacao_ao_genero,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_ao_genero, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Qual a média da sua renda individual mensal levando em conta \nos salários anteriores a pandemia?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega4_espetaculo_renda_genero.png",width = 26, height = 18, units = "cm")
remove(d) 


#-----------------------------------------------------------------------

e = visuais %>% select(renda,como_voce_se_identifica_em_relacao_ao_genero)
e = e %>% filter(!is.na(renda))

e = e %>%   group_by(como_voce_se_identifica_em_relacao_ao_genero,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100
e = e %>% filter(!is.na(como_voce_se_identifica_em_relacao_ao_genero))


g2 = ggplot(e) +
  aes(x = como_voce_se_identifica_em_relacao_ao_genero, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Qual é a média da sua renda individual mensal?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega4_visuais_renda_genero.png",width = 20, height = 14, units = "cm")

library(ggpubr)



#g1 = g1 + theme(legend.spacing.x = unit(0.8, "cm"))
#g2 = g2 + theme(legend.spacing.x = unit(0.8, "cm"))

figure = ggarrange(g1, g2, 
                   labels = c("Espetáculo", "Artes Visuais"),
                   font.label = list(size = 12, color = "#b02c57", face = "bold"),
                   ncol = 1, nrow = 2,
                   common.legend = TRUE, legend = "bottom")
#figure+ guides(fill=guide_legend(nrow = 2,byrow = TRUE))

annotate_figure(figure,
                top = text_grob("", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

ggsave("graficos_v8/entrega4_renda_genero.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)

#--------------------------------------------------------------------------------------
# idade

library(ggplot2)

d = espetaculo %>% select(renda,faixa_idade)
d = d %>% filter(!is.na(renda))
d = d %>%   group_by(faixa_idade,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = faixa_idade, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Qual a média da sua renda individual mensal levando em conta \nos salários anteriores a pandemia?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega4_espetaculo_renda_idade.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------
e = visuais %>% select(renda,faixa_idade)
e = e %>% filter(!is.na(renda))
e = e %>%   group_by(faixa_idade,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = faixa_idade, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Qual é a média da sua renda individual mensal?",
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega4_visuais_renda_idade.png",width = 20, height = 14, units = "cm")

library(ggpubr)

#g1 = g1 + theme(legend.spacing.x = unit(0.8, "cm"))
#g2 = g2 + theme(legend.spacing.x = unit(0.8, "cm"))

figure = ggarrange(g1, g2, 
                   labels = c("Espetáculo", "Artes Visuais"),
                   font.label = list(size = 12, color = "#b02c57", face = "bold"),
                   ncol = 1, nrow = 2,
                   common.legend = TRUE, legend = "bottom")
#figure+ guides(fill=guide_legend(nrow = 2,byrow = TRUE))

annotate_figure(figure,
                top = text_grob(" ", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

ggsave("graficos_v8/entrega4_renda_idade.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)

#----------------------------------------------------------------------------------------------
# tempo

library(ggplot2)
table(espetaculo$renda)

d = espetaculo %>% select(renda,ha_quanto_tempo_voce_atua_nessa_area)
d = d %>% filter(!is.na(renda))
d = d %>%   group_by(ha_quanto_tempo_voce_atua_nessa_area,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = ha_quanto_tempo_voce_atua_nessa_area, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Qual a média da sua renda individual mensal levando em conta \nos salários anteriores a pandemia?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega4_espetaculo_renda_tempo_atua_na_area.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------


e = visuais %>% select(renda,ha_quanto_tempo_voce_atua_nessa_area)
e = e %>% filter(!is.na(renda))
e = e %>%   group_by(ha_quanto_tempo_voce_atua_nessa_area,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100
e
g2 = ggplot(e) +
  aes(x = ha_quanto_tempo_voce_atua_nessa_area, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Qual é a média da sua renda individual mensal? ", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega4_visuais_renda_tempo_atua_area.png",width = 20, height = 14, units = "cm")

library(ggpubr)

#g1 = g1 + theme(legend.spacing.x = unit(0.8, "cm"))
#g2 = g2 + theme(legend.spacing.x = unit(0.8, "cm"))

figure = ggarrange(g1, g2, 
                   labels = c("Espetáculo", "Artes Visuais"),
                   font.label = list(size = 12, color = "#b02c57", face = "bold"),
                   ncol = 1, nrow = 2,
                   common.legend = TRUE, legend = "bottom")
#figure+ guides(fill=guide_legend(nrow = 2,byrow = TRUE))

annotate_figure(figure,
                top = text_grob(" ", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

ggsave("graficos_v8/entrega4_renda_tempo.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)


#----------------------------------------------------------------------------------------------
# AP

library(ggplot2)

d = espetaculo %>% select(renda,regiao)
d = d %>% filter(!is.na(regiao))
d = d %>% filter(!is.na(renda))

d = d %>%   group_by(regiao,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = regiao, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Qual a média da sua renda individual mensal levando em conta \nos salários anteriores a pandemia?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega4_espetaculo_renda_AP.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------

e = visuais %>% select(renda,regiao)
e = e %>% filter(!is.na(regiao))
e = e %>% filter(!is.na(renda))
e = e %>%   group_by(regiao,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = regiao, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Qual é a média da sua renda individual mensal? ", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega4_visuais_renda_AP.png",width = 20, height = 14, units = "cm")

library(ggpubr)
#g1 = g1 + theme(legend.spacing.x = unit(0.8, "cm"))
#g2 = g2 + theme(legend.spacing.x = unit(0.8, "cm"))


figure = ggarrange(g1, g2, 
                   labels = c("Espetáculo", "Artes Visuais"),
                   font.label = list(size = 12, color = "#b02c57", face = "bold"),
                   ncol = 1, nrow = 2,
                   common.legend = TRUE, legend = "bottom")
#figure+ guides(fill=guide_legend(nrow = 2,byrow = TRUE))

annotate_figure(figure,
                top = text_grob(" ", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

ggsave("graficos_v8/entrega4_renda_AP.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)



#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
# “Você trabalha exclusivamente na área cultural?”
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------

table(espetaculo$voce_trabalha_exclusivamente_na_area_cultural)
table(visuais$voce_trabalha_exclusivamente_na_area_cultural)

library(ggplot2)

d = espetaculo %>% select(renda,voce_trabalha_exclusivamente_na_area_cultural)
d = d %>% filter(!is.na(voce_trabalha_exclusivamente_na_area_cultural))
d = d %>% filter(!is.na(renda))

d = d %>%   group_by(voce_trabalha_exclusivamente_na_area_cultural,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = voce_trabalha_exclusivamente_na_area_cultural, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Qual a média da sua renda individual mensal levando em conta \nos salários anteriores a pandemia?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1 + labs(x ="Você trabalha exclusivamente na área cultural?")
ggsave("graficos_v8/entrega4_espetaculo_renda_trab_exclusivo.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------

e = visuais %>% select(renda,voce_trabalha_exclusivamente_na_area_cultural)
e = e %>% filter(!is.na(voce_trabalha_exclusivamente_na_area_cultural))
e = e %>% filter(!is.na(renda))
e = e %>%   group_by(voce_trabalha_exclusivamente_na_area_cultural,renda) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = voce_trabalha_exclusivamente_na_area_cultural, y = freq, fill = renda) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Qual é a média da sua renda individual mensal? ", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2 + labs(x ="Você trabalha exclusivamente na área cultural?")

remove(e)
ggsave("graficos_v8/entrega4_visuais_renda_trab_exclusivo.png",width = 20, height = 14, units = "cm")

library(ggpubr)
##g1 = g1 + theme(legend.spacing.x = unit(0.8, "cm"))
##g2 = g2 + theme(legend.spacing.x = unit(0.8, "cm"))


figure = ggarrange(g1, g2, 
                   labels = c("Espetáculo", "Artes Visuais"),
                   font.label = list(size = 12, color = "#b02c57", face = "bold"),
                   ncol = 1, nrow = 2,
                   common.legend = TRUE, legend = "bottom")
#figure+ guides(fill=guide_legend(nrow = 2,byrow = TRUE))

annotate_figure(figure,
                top = text_grob(" ", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

ggsave("graficos_v8/entrega4_renda_trab_exclusivo.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)




