

#-----------------------------------------------------------------------------------------
# GENERO
#-----------------------------------------------------------------------------------------
table(espetaculo$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_15)
table(espetaculo$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)

espetaculo$area_de_atuacao = tolower(espetaculo$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)
table(espetaculo$area_de_atuacao)

d = data.frame(prop.table(table(espetaculo$area_de_atuacao,espetaculo$como_voce_se_identifica_em_relacao_ao_genero),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)
d



ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "Espetáculo: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  #coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

RColorBrewer::brewer.pal(10,"Spectral")

ggsave("graficos_v4/entrega2_espetaculo_area_atuacao_genero_v1.png",width = 20, height = 14, units = "cm")
ggsave("graficos_v4/entrega2_espetaculo_area_atuacao_genero_v2.png",width = 20, height = 14, units = "cm")

remove(d) 

#-----------------------------------------------------------------------------------------
#   IDADE
#-----------------------------------------------------------------------------------------

d = data.frame(prop.table(table(espetaculo$area_de_atuacao,espetaculo$faixa_idade),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)
d

ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "Categoria") +
  labs(x = "Idade", y = "Percentual", subtitle = "Espetáculo: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

RColorBrewer::brewer.pal(10,"Spectral")

ggsave("graficos_v4/entrega2_espetaculo_area_atuacao_idade_v1.png",width = 20, height = 14, units = "cm")
ggsave("graficos_v4/entrega2_espetaculo_area_atuacao_idade_v2.png",width = 20, height = 14, units = "cm")
remove(d) 


#-----------------------------------------------------------------------------------------
#   COR
#-----------------------------------------------------------------------------------------

d = data.frame(prop.table(table(espetaculo$area_de_atuacao,espetaculo$como_voce_se_identifica_em_relacao_a_raca),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)
d

ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "Categoria") +
  labs(x = "Cor", y = "Percentual", subtitle = "Espetáculo: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

RColorBrewer::brewer.pal(10,"Spectral")

ggsave("graficos_v4/entrega2_espetaculo_area_atuacao_cor_v1.png",width = 20, height = 14, units = "cm")
ggsave("graficos_v4/entrega2_espetaculo_area_atuacao_cor_v2.png",width = 20, height = 14, units = "cm")
remove(d) 



#-----------------------------------------------------------------------------------------
#   AP
#-----------------------------------------------------------------------------------------

d = data.frame(prop.table(table(espetaculo$area_de_atuacao,espetaculo$regiao),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)
d

ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "Categoria") +
  labs(x = "Área de Planejamento", y = "Percentual", subtitle = "Espetáculo: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

RColorBrewer::brewer.pal(10,"Spectral")

ggsave("graficos_v4/entrega2_espetaculo_area_atuacao_AP_v1.png",width = 20, height = 14, units = "cm")
ggsave("graficos_v4/entrega2_espetaculo_area_atuacao_AP_v2.png",width = 20, height = 14, units = "cm")
remove(d) 
