library(ggplot2)
table(espetaculo$voce_trabalha_exclusivamente_na_area_cultural)

d = espetaculo %>% select(voce_trabalha_exclusivamente_na_area_cultural,como_voce_se_identifica_em_relacao_ao_genero)

d = d %>%   group_by(como_voce_se_identifica_em_relacao_ao_genero,voce_trabalha_exclusivamente_na_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d

g1 = ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_ao_genero, y = freq, fill = voce_trabalha_exclusivamente_na_area_cultural) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "") +
  labs(x = "", y = "Percentual", #subtitle = "Espetáculo: Você trabalha exclusivamente na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ecoar_theme2()

g1
#ggsave("graficos_v6/entrega2_espetaculo_trabalho_exclusivo_cor.png",width = 20, height = 14, units = "cm")
#remove(d) 


#------------------------------------------------------------------------
table(visuais$voce_trabalha_exclusivamente_na_area_cultural,visuais$como_voce_se_identifica_em_relacao_ao_genero)


e = visuais %>% select(voce_trabalha_exclusivamente_na_area_cultural,como_voce_se_identifica_em_relacao_ao_genero)

e = e %>%   group_by(como_voce_se_identifica_em_relacao_ao_genero,voce_trabalha_exclusivamente_na_area_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

e = e[1:5,]
e

g2 = ggplot(e) +
  aes(x = como_voce_se_identifica_em_relacao_ao_genero, y = freq, fill = voce_trabalha_exclusivamente_na_area_cultural) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "") +
  labs(x = "", y = "Percentual", #subtitle = "Artes Visuais: Você trabalha exclusivamente na área cultural?", 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ecoar_theme2()

g2

library(ggpubr)
figure = ggarrange(g1, g2, 
          labels = c("Espetáculo", "Artes Visuais"),
          font.label = list(size = 14, color = "#b02c57", face = "bold"),
          ncol = 1, nrow = 2,
          common.legend = TRUE, legend = "bottom")

annotate_figure(figure,
                top = text_grob("Você trabalha exclusivamente na área cultural?", color = "#b02c57", face = "bold", size = 14),
                bottom = text_grob("Fonte: ECOA", color = "#b02c57",
                                   hjust = 1, x = 1, face = "italic", size = 10))+
  theme(plot.title = element_text(colour  = "white", size = 18), legend.position = "bottom",
        legend.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        #legend.key = element_rect(fill = "#b8cfc6", colour = "#b8cfc6" ),
        plot.background = element_rect(fill = "#b8cfc6", colour = "#b8cfc6"))

ggsave("graficos_v6/entrega2_trabalho_exclusivo_genero.png",width = 20, height = 14, units = "cm")
remove(d,e) 
