

#-----------------------------------------------------------------------------------------
# GENERO
#-----------------------------------------------------------------------------------------
table(espetaculo$voce_trabalha_exclusivamente_na_area_cultural)

d = data.frame(prop.table(table(espetaculo$voce_trabalha_exclusivamente_na_area_cultural,espetaculo$como_voce_se_identifica_em_relacao_ao_genero),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)
d



ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "Espetáculo: Você trabalha exclusivamente na área cultural?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

ggsave("graficos_v4/entrega2_espetaculo_trab_exclusivo_genero_v1.png",width = 20, height = 14, units = "cm")
ggsave("graficos_v4/entrega2_espetaculo_trab_exclusivo_genero_v2.png",width = 20, height = 14, units = "cm")

remove(d) 

#-----------------------------------------------------------------------------------------
#   IDADE
#-----------------------------------------------------------------------------------------

d = data.frame(prop.table(table(espetaculo$voce_trabalha_exclusivamente_na_area_cultural,espetaculo$faixa_idade),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)
d

ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  labs(x = "Idade", y = "Percentual", subtitle = "Espetáculo: Você trabalha exclusivamente na área cultural?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

ggsave("graficos_v4/entrega2_espetaculo_trab_exclusivo_idade_v1.png",width = 20, height = 14, units = "cm")
ggsave("graficos_v4/entrega2_espetaculo_trab_exclusivo_idade_v2.png",width = 20, height = 14, units = "cm")
remove(d) 


#-----------------------------------------------------------------------------------------
#   COR
#-----------------------------------------------------------------------------------------

d = data.frame(prop.table(table(espetaculo$voce_trabalha_exclusivamente_na_area_cultural,espetaculo$como_voce_se_identifica_em_relacao_a_raca),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)
d

ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  labs(x = "Cor", y = "Percentual", subtitle = "Espetáculo: Você trabalha exclusivamente na área cultural?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

ggsave("graficos_v4/entrega2_espetaculo_trab_exclusivo_cor_v1.png",width = 20, height = 14, units = "cm")
ggsave("graficos_v4/entrega2_espetaculo_trab_exclusivo_cor_v2.png",width = 20, height = 14, units = "cm")
remove(d) 



#-----------------------------------------------------------------------------------------
#   AP
#-----------------------------------------------------------------------------------------

d = data.frame(prop.table(table(espetaculo$voce_trabalha_exclusivamente_na_area_cultural,espetaculo$regiao),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)
d

ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  labs(x = "Área de Planejamento", y = "Percentual", subtitle = "Espetáculo: Você trabalha exclusivamente na área cultural?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

ggsave("graficos_v4/entrega2_espetaculo_trab_exclusivo_AP_v1.png",width = 20, height = 14, units = "cm")
ggsave("graficos_v4/entrega2_espetaculo_trab_exclusivo_AP_v2.png",width = 20, height = 14, units = "cm")
remove(d) 
