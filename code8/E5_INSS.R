source('/home/steven/Documentos/GitHub/projeto_cultura_uff/code8/carregar_as_bases.R')


espetaculo$inss =  espetaculo$pensando_em_sua_aposentadoria_voce_contribui_para_o_inss
visuais$inss = visuais$pensando_em_sua_aposentadoria_voce_contribui_para_o_inss


espetaculo$inss = gsub('Não sei  / Não quero responder',NA,espetaculo$inss)
visuais$inss = gsub('Não sei  / Não quero responder',NA,visuais$inss)

table(espetaculo$inss)
table(visuais$inss)


d = espetaculo %>% select(inss,como_voce_se_identifica_em_relacao_a_raca)
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')
d = d %>% filter(!is.na(inss))
d = d %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,inss) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = inss) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Pensando em sua aposentadoria, você contribui para o INSS?" , 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega5_espetaculo_inss_cor.png",width = 26, height = 18, units = "cm")
remove(d) 

#------------------------------------------------------------------------

e = visuais %>% select(inss,como_voce_se_identifica_em_relacao_a_raca)
e = e %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
e = e %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')
e = e %>% filter(!is.na(inss))
e = e %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,inss) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = inss) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Pensando em sua aposentadoria, você contribui para o INSS?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega5_visuais_inss_cor.png",width = 24, height = 14, units = "cm")

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


ggsave("graficos_v8/entrega5_inss_cor.png",width = 28, height = 20, units = "cm")

remove(d,e)
remove(g1,g2)
remove(figure)

#----------------------------------------------------------------------------------------------
# Genero

d = espetaculo %>% select(inss,como_voce_se_identifica_em_relacao_ao_genero)
d = d %>% filter(!is.na(inss))

d = d %>%   group_by(como_voce_se_identifica_em_relacao_ao_genero,inss) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_ao_genero, y = freq, fill = inss) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Pensando em sua aposentadoria, você contribui para o INSS?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega5_espetaculo_inss_genero.png",width = 26, height = 18, units = "cm")
remove(d) 


#-----------------------------------------------------------------------

e = visuais %>% select(inss,como_voce_se_identifica_em_relacao_ao_genero)
e = e %>% filter(!is.na(inss))

e = e %>%   group_by(como_voce_se_identifica_em_relacao_ao_genero,inss) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100
e = e %>% filter(!is.na(como_voce_se_identifica_em_relacao_ao_genero))


g2 = ggplot(e) +
  aes(x = como_voce_se_identifica_em_relacao_ao_genero, y = freq, fill = inss) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Pensando em sua aposentadoria, você contribui para o INSS?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega5_visuais_inss_genero.png",width = 24, height = 14, units = "cm")

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

ggsave("graficos_v8/entrega5_inss_genero.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)

#--------------------------------------------------------------------------------------
# idade

d = espetaculo %>% select(inss,faixa_idade)
d = d %>% filter(!is.na(inss))
d = d %>%   group_by(faixa_idade,inss) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = faixa_idade, y = freq, fill = inss) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Pensando em sua aposentadoria, você contribui para o INSS?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega5_espetaculo_inss_idade.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------
e = visuais %>% select(inss,faixa_idade)
e = e %>% filter(!is.na(inss))
e = e %>%   group_by(faixa_idade,inss) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = faixa_idade, y = freq, fill = inss) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Pensando em sua aposentadoria, você contribui para o INSS? ",
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega5_visuais_inss_idade.png",width = 24, height = 14, units = "cm")

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

ggsave("graficos_v8/entrega5_inss_idade.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)

#----------------------------------------------------------------------------------------------
# tempo
d = espetaculo %>% select(inss,ha_quanto_tempo_voce_atua_nessa_area)
d = d %>% filter(!is.na(inss))
d = d %>%   group_by(ha_quanto_tempo_voce_atua_nessa_area,inss) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = ha_quanto_tempo_voce_atua_nessa_area, y = freq, fill = inss) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Pensando em sua aposentadoria, você contribui para o INSS?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega5_espetaculo_inss_tempo_atua_na_area.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------


e = visuais %>% select(inss,ha_quanto_tempo_voce_atua_nessa_area)
e = e %>% filter(!is.na(inss))
e = e %>%   group_by(ha_quanto_tempo_voce_atua_nessa_area,inss) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100
e
g2 = ggplot(e) +
  aes(x = ha_quanto_tempo_voce_atua_nessa_area, y = freq, fill = inss) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Pensando em sua aposentadoria, você contribui para o INSS?  ", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega5_visuais_inss_tempo_atua_area.png",width = 24, height = 14, units = "cm")

#-------------------------------------------------------------------------------------------------
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

ggsave("graficos_v8/entrega5_inss_tempo.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)


#----------------------------------------------------------------------------------------------
# AP
d = espetaculo %>% select(inss,regiao)
d = d %>% filter(!is.na(regiao))
d = d %>% filter(!is.na(inss))

d = d %>%   group_by(regiao,inss) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = regiao, y = freq, fill = inss) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Pensando em sua aposentadoria, você contribui para o INSS?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega5_espetaculo_inss_AP.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------

e = visuais %>% select(inss,regiao)
e = e %>% filter(!is.na(regiao))
e = e %>% filter(!is.na(inss))
e = e %>%   group_by(regiao,inss) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = regiao, y = freq, fill = inss) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Pensando em sua aposentadoria, você contribui para o INSS?  ", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega5_visuais_inss_AP.png",width = 24, height = 14, units = "cm")

#-----------------------------------------------------------------------------------------
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

ggsave("graficos_v8/entrega5_inss_AP.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)



