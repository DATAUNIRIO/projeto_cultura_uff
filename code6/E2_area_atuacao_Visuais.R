

#-----------------------------------------------------------------------------------------
# GENERO
#-----------------------------------------------------------------------------------------
table(visuais$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_15)
table(visuais$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)

visuais$area_de_atuacao = tolower(visuais$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)
table(visuais$area_de_atuacao)

d = data.frame(prop.table(table(visuais$area_de_atuacao,visuais$como_voce_se_identifica_em_relacao_ao_genero),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)

#re-order factor levels 
d$Var2 <- factor(d$Var2, levels=c('HOMEM CIS','MULHER CIS','GÊNERO FLUÍDO'))

 

ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11,"Spectral"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "Artes Visuais: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  #coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

RColorBrewer::brewer.pal(11,"Spectral")

ggsave("graficos_v6/entrega2_visuais_area_atuacao_genero.png",width = 20, height = 14, units = "cm")


remove(d) 

#-----------------------------------------------------------------------------------------
#   IDADE
#-----------------------------------------------------------------------------------------

d = data.frame(prop.table(table(visuais$area_de_atuacao,visuais$faixa_idade),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)
d

ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11,"Spectral"),name = "Categoria") +
  labs(x = "Idade", y = "Percentual", subtitle = "Artes Visuais: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  #coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

ggsave("graficos_v6/entrega2_visuais_area_atuacao_idade.png",width = 20, height = 14, units = "cm")

remove(d) 


#-----------------------------------------------------------------------------------------
#   COR
#-----------------------------------------------------------------------------------------

d = visuais %>% select(area_de_atuacao,como_voce_se_identifica_em_relacao_a_raca)
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')

d = d %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,area_de_atuacao) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


#d = data.frame(prop.table(table(visuais$area_de_atuacao,visuais$como_voce_se_identifica_em_relacao_a_raca),2))
#d$Freq = d$Freq*100
#d$Freq =round(d$Freq,2)
#d

ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = area_de_atuacao) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11,"Spectral"),name = "Categoria") +
  labs(x = "Cor", y = "Percentual", subtitle = "Artes Visuais: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  #coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

ggsave("graficos_v6/entrega2_visuais_area_atuacao_cor.png",width = 20, height = 14, units = "cm")
remove(d) 



#-----------------------------------------------------------------------------------------
#   AP
#-----------------------------------------------------------------------------------------

d = data.frame(prop.table(table(visuais$area_de_atuacao,visuais$regiao),2))
d$Freq = d$Freq*100
d$Freq =round(d$Freq,2)
d

table(visuais$regiao)
table(visuais$area_de_atuacao,visuais$regiao)

ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11,"Spectral"),name = "Categoria") +
  labs(x = "Área de Planejamento", y = "Percentual", subtitle = "Artes Visuais: Qual a sua principal área de atuação no campo \nda cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  #coord_flip()+
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var2))+
  ecoar_theme2()

ggsave("graficos_v6/entrega2_visuais_area_atuacao_AP.png",width = 20, height = 14, units = "cm")
remove(d) 
