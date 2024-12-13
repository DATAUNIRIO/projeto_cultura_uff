source('/home/steven/Documentos/GitHub/projeto_cultura_uff/code8/carregar_as_bases.R')
library(ggplot2)

# Como se dá o seu vínculo trabalhista na área cultural? 
perguntas[27]

table(espetaculo$como_se_da_o_seu_vinculo_trabalhista_na_area_cultural_voce_pode_marcar_mais_de_uma_opcao_29)
# table(espetaculo$como_se_da_o_seu_vinculo_trabalhista_na_area_cultural_voce_pode_marcar_mais_de_uma_opcao_30)


espetaculo$vinculo_trabalhista = espetaculo$como_se_da_o_seu_vinculo_trabalhista_na_area_cultural_voce_pode_marcar_mais_de_uma_opcao_30

View(data.frame(table(espetaculo$vinculo_trabalhista)))

espetaculo$vinculo_trabalhista = ifelse(stringr::str_detect(espetaculo$vinculo_trabalhista,'MAIS DE UM'),'Mais de um vínculo',espetaculo$vinculo_trabalhista)
espetaculo$vinculo_trabalhista = gsub('OUTROS: NÃO IDENTIFICADO','Outro vínculo',espetaculo$vinculo_trabalhista)
espetaculo$vinculo_trabalhista = gsub('OUTROS: GESTORA ESTATUTÁRIA','Outro vínculo',espetaculo$vinculo_trabalhista)
espetaculo$vinculo_trabalhista = gsub('Não sei / Não quero responder',NA,espetaculo$vinculo_trabalhista)
espetaculo$vinculo_trabalhista = gsub('Estagiário\\(a\\); Profissional autônomo','Mais de um vínculo',espetaculo$vinculo_trabalhista)
table(espetaculo$vinculo_trabalhista)

visuais$vinculo_trabalhista = visuais$como_se_da_o_seu_vinculo_trabalhista_na_area_cultural_voce_pode_marcar_mais_de_uma_opcao_35

visuais$vinculo_trabalhista = ifelse(stringr::str_detect(visuais$vinculo_trabalhista,'MAIS DE UM'),'Mais de um vínculo',visuais$vinculo_trabalhista)
visuais$vinculo_trabalhista = gsub('Não sei / Não quero responder',NA,visuais$vinculo_trabalhista)
visuais$vinculo_trabalhista = gsub('Não sei responder',NA,visuais$vinculo_trabalhista)
visuais$vinculo_trabalhista = gsub('Não sei responder',NA,visuais$vinculo_trabalhista)
visuais$vinculo_trabalhista = gsub('Com vínculo público \\(CLT\\) / Terceirizado','Mais de um vínculo',visuais$vinculo_trabalhista)


table(visuais$vinculo_trabalhista)
d = espetaculo %>% select(vinculo_trabalhista,como_voce_se_identifica_em_relacao_a_raca)
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')
d = d %>% filter(!is.na(vinculo_trabalhista))
#d = d %>% filter(vinculo_trabalhista!='Não sei / Não quero responder')
d = d %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100


g1 = ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Como se dá o seu vínculo trabalhista na área cultural?" , 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega4_espetaculo_vinculo_trabalhista_cor.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------

e = visuais %>% select(vinculo_trabalhista,como_voce_se_identifica_em_relacao_a_raca)
e = e %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
e = e %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')
e = e %>% filter(!is.na(vinculo_trabalhista))
e = e %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Como se dá o seu vínculo trabalhista na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega4_visuais_vinculo_cor.png",width = 20, height = 14, units = "cm")

library(ggpubr)

g1 = g1 + theme(legend.spacing.x = unit(0.8, "cm"))
g2 = g2 + theme(legend.spacing.x = unit(0.8, "cm"))

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


ggsave("graficos_v8/entrega4_vinculo_cor.png",width = 28, height = 20, units = "cm")

remove(d,e)
remove(g1,g2)
remove(figure)

#----------------------------------------------------------------------------------------------
# Genero
library(ggplot2)

d = espetaculo %>% select(vinculo_trabalhista,como_voce_se_identifica_em_relacao_ao_genero)
d = d %>% filter(!is.na(vinculo_trabalhista))

d = d %>%   group_by(como_voce_se_identifica_em_relacao_ao_genero,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_ao_genero, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Como se dá o seu vínculo trabalhista na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega4_espetaculo_vinculo_genero.png",width = 26, height = 18, units = "cm")
remove(d) 


#-----------------------------------------------------------------------

e = visuais %>% select(vinculo_trabalhista,como_voce_se_identifica_em_relacao_ao_genero)
e = e %>% filter(!is.na(vinculo_trabalhista))

e = e %>%   group_by(como_voce_se_identifica_em_relacao_ao_genero,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100
e = e %>% filter(!is.na(como_voce_se_identifica_em_relacao_ao_genero))


g2 = ggplot(e) +
  aes(x = como_voce_se_identifica_em_relacao_ao_genero, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Como se dá o seu vínculo trabalhista na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega4_visuais_vinculo_genero.png",width = 20, height = 14, units = "cm")

library(ggpubr)



g1 = g1 + theme(legend.spacing.x = unit(0.8, "cm"))
g2 = g2 + theme(legend.spacing.x = unit(0.8, "cm"))

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

ggsave("graficos_v8/entrega4_vinculo_genero.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)

#--------------------------------------------------------------------------------------
# idade

library(ggplot2)

d = espetaculo %>% select(vinculo_trabalhista,faixa_idade)
d = d %>% filter(!is.na(vinculo_trabalhista))
d = d %>%   group_by(faixa_idade,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = faixa_idade, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Como se dá o seu vínculo trabalhista na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega4_espetaculo_vinculo_idade.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------
e = visuais %>% select(vinculo_trabalhista,faixa_idade)
e = e %>% filter(!is.na(vinculo_trabalhista))
e = e %>%   group_by(faixa_idade,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = faixa_idade, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Como se dá o seu vínculo trabalhista na área cultural?",
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega4_visuais_vinculo_idade.png",width = 20, height = 14, units = "cm")

library(ggpubr)

g1 = g1 + theme(legend.spacing.x = unit(0.8, "cm"))
g2 = g2 + theme(legend.spacing.x = unit(0.8, "cm"))

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

ggsave("graficos_v8/entrega4_vinculo_idade.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)

#----------------------------------------------------------------------------------------------
# tempo

library(ggplot2)
table(espetaculo$vinculo_trabalhista)

d = espetaculo %>% select(vinculo_trabalhista,ha_quanto_tempo_voce_atua_nessa_area)
d = d %>% filter(!is.na(vinculo_trabalhista))
d = d %>%   group_by(ha_quanto_tempo_voce_atua_nessa_area,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = ha_quanto_tempo_voce_atua_nessa_area, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Como se dá o seu vínculo trabalhista na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega4_espetaculo_vinculo_tempo_atua_na_area.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------


e = visuais %>% select(vinculo_trabalhista,ha_quanto_tempo_voce_atua_nessa_area)
e = e %>% filter(!is.na(vinculo_trabalhista))
e = e %>%   group_by(ha_quanto_tempo_voce_atua_nessa_area,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100
e
g2 = ggplot(e) +
  aes(x = ha_quanto_tempo_voce_atua_nessa_area, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Como se dá o seu vínculo trabalhista na área cultural? ", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega4_visuais_vinculo_tempo_atua_area.png",width = 20, height = 14, units = "cm")

library(ggpubr)

g1 = g1 + theme(legend.spacing.x = unit(0.8, "cm"))
g2 = g2 + theme(legend.spacing.x = unit(0.8, "cm"))

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

ggsave("graficos_v8/entrega4_vinculo_tempo.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)


#----------------------------------------------------------------------------------------------
# AP

library(ggplot2)

d = espetaculo %>% select(vinculo_trabalhista,regiao)
d = d %>% filter(!is.na(regiao))
d = d %>% filter(!is.na(vinculo_trabalhista))

d = d %>%   group_by(regiao,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = regiao, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Como se dá o seu vínculo trabalhista na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega4_espetaculo_vinculo_AP.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------

e = visuais %>% select(vinculo_trabalhista,regiao)
e = e %>% filter(!is.na(regiao))
e = e %>% filter(!is.na(vinculo_trabalhista))
e = e %>%   group_by(regiao,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = regiao, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Como se dá o seu vínculo trabalhista na área cultural? ", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2
remove(e)
ggsave("graficos_v8/entrega4_visuais_vinculo_AP.png",width = 20, height = 14, units = "cm")

library(ggpubr)
g1 = g1 + theme(legend.spacing.x = unit(0.8, "cm"))
g2 = g2 + theme(legend.spacing.x = unit(0.8, "cm"))


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

ggsave("graficos_v8/entrega4_vinculo_AP.png",width = 28, height = 20, units = "cm")
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

d = espetaculo %>% select(vinculo_trabalhista,voce_trabalha_exclusivamente_na_area_cultural)
d = d %>% filter(!is.na(voce_trabalha_exclusivamente_na_area_cultural))
d = d %>% filter(!is.na(vinculo_trabalhista))

d = d %>%   group_by(voce_trabalha_exclusivamente_na_area_cultural,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100

g1 = ggplot(d) +
  aes(x = voce_trabalha_exclusivamente_na_area_cultural, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Como se dá o seu vínculo trabalhista na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1 + labs(x ="Você trabalha exclusivamente na área cultural?")
ggsave("graficos_v8/entrega4_espetaculo_vinculo_trab_exclusivo.png",width = 26, height = 18, units = "cm")
remove(d) 


#------------------------------------------------------------------------

e = visuais %>% select(vinculo_trabalhista,voce_trabalha_exclusivamente_na_area_cultural)
e = e %>% filter(!is.na(voce_trabalha_exclusivamente_na_area_cultural))
e = e %>% filter(!is.na(vinculo_trabalhista))
e = e %>%   group_by(voce_trabalha_exclusivamente_na_area_cultural,vinculo_trabalhista) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e$freq = e$freq*100

g2 = ggplot(e) +
  aes(x = voce_trabalha_exclusivamente_na_area_cultural, y = freq, fill = vinculo_trabalhista) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Como se dá o seu vínculo trabalhista na área cultural? ", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g2 + labs(x ="Você trabalha exclusivamente na área cultural?")

remove(e)
ggsave("graficos_v8/entrega4_visuais_vinculo_trab_exclusivo.png",width = 20, height = 14, units = "cm")

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

ggsave("graficos_v8/entrega4_vinculo_trab_exclusivo.png",width = 28, height = 20, units = "cm")
remove(d,e)
remove(g1,g2)
remove(figure)



