

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
  labs(x = "Gênero", y = "Percentual", subtitle = "Artes do Espetáculo: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  #coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()


ggsave("graficos_v7/entrega2_espetaculo_area_atuacao_genero.png",width = 20, height = 14, units = "cm")


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
  labs(x = "Idade", y = "Percentual", subtitle = "Artes do Espetáculo: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  #coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

ggsave("graficos_v7/entrega2_espetaculo_area_atuacao_idade.png",width = 20, height = 14, units = "cm")

remove(d) 


#-----------------------------------------------------------------------------------------
#   COR
#-----------------------------------------------------------------------------------------

#d = data.frame(prop.table(table(espetaculo$area_de_atuacao,espetaculo$como_voce_se_identifica_em_relacao_a_raca),2))
#d$Freq = d$Freq*100
#d$Freq =round(d$Freq,2)
#d

d = espetaculo %>% select(area_de_atuacao,como_voce_se_identifica_em_relacao_a_raca)
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')

d = d %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,area_de_atuacao) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))



ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = area_de_atuacao) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "Categoria") +
  labs(x = "Cor", y = "Percentual", subtitle = "Artes do Espetáculo: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  #coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

ggsave("graficos_v7/entrega2_espetaculo_area_atuacao_cor.png",width = 20, height = 14, units = "cm")
remove(d) 



#-----------------------------------------------------------------------------------------
#   AP
#-----------------------------------------------------------------------------------------

table(espetaculo$regiao)
d = data.frame(prop.table(table(espetaculo$area_de_atuacao,espetaculo$regiao),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)
d

ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "Categoria") +
  labs(x = "Área de Planejamento", y = "Percentual", subtitle = "Artes do Espetáculo: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  #coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

ggsave("graficos_v7/entrega2_espetaculo_area_atuacao_AP.png",width = 20, height = 14, units = "cm")

remove(d) 
