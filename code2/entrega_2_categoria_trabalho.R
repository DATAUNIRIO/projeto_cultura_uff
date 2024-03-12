
table(visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra_voce_pode_marcar_mais_de_uma_opcao)

tabela = tabyl(visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra)

visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra = gsub('^(Arte-educação)$','Apenas ûm',visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra)
visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra = gsub('^(Artístico)$','Apenas ûm',visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra)
visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra = gsub('^(Produção/Gestão)$','Apenas ûm',visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra)

library(stringr)
visuais = visuais %>%
  mutate(
    categoria_trabalho = case_when(
      str_detect('MAIS DE UM',dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra) ~ "MAIS DE UM",
      str_detect('Apenas ûm',dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra) ~ "APENAS UM",
    TRUE                      ~ "MAIS DE UM"))

table(visuais$categoria_trabalho,visuais$como_voce_se_identifica_em_relacao_ao_genero)

#--------------------------------------------------------------------------------------------------------------
#         Espetaculos
#--------------------------------------------------------------------------------------------------------------

tabela = tabyl(espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra)

espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra = gsub('^(Técnico)$','Apenas ûm',espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra)
espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra = gsub('^(Artístico)$','Apenas ûm',espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra)
espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra = gsub('^(Produção/Gestão)$','Apenas ûm',espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra)


espetaculo = espetaculo %>%
  mutate(
    categoria_trabalho = case_when(
      str_detect('MAIS DE UM',dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra) ~ "MAIS DE UM",
      str_detect('Apenas ûm',dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra) ~ "APENAS UM",
      TRUE                      ~ "MAIS DE UM"))

table(espetaculo$categoria_trabalho)

table(espetaculo$categoria_trabalho,espetaculo$como_voce_se_identifica_em_relacao_ao_genero)

#-----------------------------------------------------------------------------------------------------------
tabela = data.frame(prop.table(table(espetaculo$categoria_trabalho,espetaculo$como_voce_se_identifica_em_relacao_ao_genero),2)*100)
tabela
library(ggplot2)
ggplot(tabela) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "Espetáculo: Dentro da área cultural, em qual \ncategoria seu trabalho se enquadra?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var1))+
  ecoar_theme2()

ggsave("graficos/entrega2_espetaculo_trabalho_genero.png",width = 20, height = 14, units = "cm")

tabela = data.frame(prop.table(table(espetaculo$categoria_trabalho,espetaculo$como_voce_se_identifica_em_relacao_ao_genero),1)*100)
tabela
ggplot(tabela) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "ERRADO Espetáculo: Dentro da área cultural, em qual \ncategoria seu trabalho se enquadra?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var1))+
  ecoar_theme2()

ggsave("graficos/entrega2_espetaculo_trabalho_genero_v2.png",width = 20, height = 14, units = "cm")

ggplot(tabela) +
  aes(x = Var1, y = Freq, fill = Var2) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7","#ec6d13"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "Espetáculo: Dentro da área cultural, em qual \ncategoria seu trabalho se enquadra?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var1))+
  ecoar_theme2()
ggsave("graficos/entrega2_espetaculo_trabalho_genero_v3.png",width = 20, height = 14, units = "cm")

remove(tabela)

#--------------------------------------------------------------------------------------------------------------
#         Artes Visuais
#--------------------------------------------------------------------------------------------------------------


tabela = data.frame(prop.table(table(visuais$categoria_trabalho,visuais$como_voce_se_identifica_em_relacao_ao_genero),2)*100)
tabela

ggplot(tabela) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "Artes Visuais: Dentro da área cultural, em qual \ncategoria seu trabalho se enquadra?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var1))+
  ecoar_theme2()

ggsave("graficos/entrega2_visuais_trabalho_genero.png",width = 20, height = 14, units = "cm")

tabela = data.frame(prop.table(table(visuais$categoria_trabalho,visuais$como_voce_se_identifica_em_relacao_ao_genero),1)*100)
tabela
ggplot(tabela) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "ERRADO Visuais: Dentro da área cultural, em qual \ncategoria seu trabalho se enquadra?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var1))+
  ecoar_theme2()

ggsave("graficos/entrega2_visuais_trabalho_genero_v2.png",width = 20, height = 14, units = "cm")

tabela

ggplot(tabela) +
  aes(x = Var1, y = Freq, fill = Var2) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7","#ec6d13"),name = "Categoria") +
  labs(x = "Gênero", y = "Percentual", subtitle = "Espetáculo: Dentro da área cultural, em qual \ncategoria seu trabalho se enquadra?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(Var1))+
  ecoar_theme2()
ggsave("graficos/entrega2_visuais_trabalho_genero_v3.png",width = 20, height = 14, units = "cm")

remove(tabela)
