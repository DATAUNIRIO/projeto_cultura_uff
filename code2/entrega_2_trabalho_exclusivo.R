
table(visuais$voce_trabalha_exclusivamente_na_area_cultural)

tabela = tabyl(visuais$voce_trabalha_exclusivamente_na_area_cultural)
tabela2 = tabyl(espetaculo$voce_trabalha_exclusivamente_na_area_cultural)

colnames(tabela)[1] = 'exclusivo'
tabela$tipo= rep("artes visuais",2)
tabela2$tipo= rep("espetáculo",2)
colnames(tabela2)[1] = 'exclusivo'
# Combine data.frames
all_df <- rbind(cbind(tabela, facet = "Artes Visuais"),
                cbind(tabela2, facet = "Espetáculo"))
library(ggplot2)
library(ggh4x)
ggplot(all_df, aes(percent, forcats::fct_rev(exclusivo),fill=facet)) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  geom_text(aes(label = percent),nudge_x = 4) +
  facet_wrap(~ facet, scales = "free_x") +
  facetted_pos_scales(x = list(
    scale_x_reverse(limits = c(.55, 0)),
    scale_x_continuous(limits = c(0, 0.55))
  ))+
  labs(y='Trabalho exclusivo',x='Percentual',subtitle = 'Você trabalha exclusivamente na \nárea cultural?')+
  theme_minimal()+
  ecoar_theme()

ggsave("graficos/entrega2_trabalho_exclusivo_na_area_de_cultura.png",width = 20, height = 14, units = "cm")

#-------------------------------------------------------------------------------------------------------------------------
#          Espetaculo
#-------------------------------------------------------------------------------------------------------------------------

tabela = data.frame(prop.table(table(espetaculo$voce_trabalha_exclusivamente_na_area_cultural,espetaculo$como_voce_se_identifica_em_relacao_ao_genero),2)*100)
tabela
library(ggplot2)
ggplot(tabela) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "Espetáculo: Você trabalha exclusivamente na \nárea cultural?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var1))+
  ecoar_theme2()

ggsave("graficos/entrega2_espetaculo_trabalho_exclusivo_genero.png",width = 20, height = 14, units = "cm")

tabela = data.frame(prop.table(table(espetaculo$voce_trabalha_exclusivamente_na_area_cultural,espetaculo$como_voce_se_identifica_em_relacao_ao_genero),1)*100)
tabela

ggplot(tabela) +
  aes(x = Var1, y = Freq, fill = Var2) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7","#ec6d13"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "Espetáculo: Você trabalha exclusivamente na \nárea cultural?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var1))+
  ecoar_theme2()
ggsave("graficos/entrega2_espetaculo_trabalho_exclusivo_genero_v3.png",width = 20, height = 14, units = "cm")

remove(tabela)

#---------------------------------------------------------------------------------------
#       ARTES VISUAIS
#---------------------------------------------------------------------------------------

tabela = data.frame(prop.table(table(visuais$voce_trabalha_exclusivamente_na_area_cultural,visuais$como_voce_se_identifica_em_relacao_ao_genero),2)*100)
tabela

ggplot(tabela) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "Artes Visuais: Você trabalha exclusivamente na \nárea cultural?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var1))+
  ecoar_theme2()

ggsave("graficos/entrega2_visuais_trabalho_exclusivo_genero.png",width = 20, height = 14, units = "cm")

tabela = data.frame(prop.table(table(visuais$voce_trabalha_exclusivamente_na_area_cultural,visuais$como_voce_se_identifica_em_relacao_ao_genero),1)*100)
tabela

ggplot(tabela) +
  aes(x = Var1, y = Freq, fill = Var2) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7","#ec6d13"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "Artes Visuais: Você trabalha exclusivamente na \nárea cultural?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var1))+
  ecoar_theme2()
ggsave("graficos/entrega2_visuais_trabalho_exclusivo_genero_v3.png",width = 20, height = 14, units = "cm")

remove(tabela)
