library(stringr)

table(visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra_voce_pode_marcar_mais_de_uma_opcao_13)
table(visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra_voce_pode_marcar_mais_de_uma_opcao_14)

visuais$categoria = tolower(visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra_voce_pode_marcar_mais_de_uma_opcao_14)
# 
# table(visuais$area_de_atuacao_cultura)
# 
# # tem que agrupar e corrigir
# arte digital 
# arte urbana 
# artesanato 
# fotografia 
# mais de um: artista multidisciplinar autonoma 
# mais de um: autora, professora, promotora de eventos culturais 
# mais de um: editora montadora design 
# mais de um: pintora, desenhista, gravadora 
# mais de um: videomaker, roteirista, cinegrafista, editor, produtor cultural 
# outro: artista 
# outro: atriz 
# outro: coordenadora 
# outro: diretora 
# outro: mediação  e curadoria 
# outro: produção 
# outro: produção-produtora cultural 
# outro: produçao gestao curadoria projetos 
# outro: produção, curadoria, pesquisa, design da montagem, gestão da execução da montagem, plano de comunicação... 
# outro: produtor 
# performance 
# pintura 
# tatuagem 
# videoarte
# 
# 
# visuais = visuais %>%
#   mutate(
#     area_atuacao_carnaval = case_when(
#       str_detect(area_de_atuacao_cultura,'carnaval') ~ "Carnaval",
#       .default = 'Não'))
# visuais = visuais %>%
#   mutate(
#     area_atuacao_musica = case_when(
#       str_detect(area_de_atuacao_cultura,'musica') ~ "Música",
#       .default = 'Não'))
# visuais = visuais %>%
#   mutate(
#     area_atuacao_danca = case_when(
#       str_detect(area_de_atuacao_cultura,'dança') ~ "Dança",
#       .default = 'Não'))
# 
# visuais = visuais %>%
#   mutate(
#     area_atuacao_teatro = case_when(
#       str_detect(area_de_atuacao_cultura,'teatro') ~ "Teatro",
#       .default = 'Não'))
# 
# visuais = visuais %>%
#   mutate(
#     area_atuacao_outros = case_when(
#       str_detect(area_de_atuacao_cultura,'outros') ~ "Outra área",
#       str_detect(area_de_atuacao_cultura,'cinema') ~ "Outra área",
#       str_detect(area_de_atuacao_cultura,'circo') ~ "Outra área",
#       str_detect(area_de_atuacao_cultura,'produção e gestão cultural') ~ "Outra área",
#       str_detect(area_de_atuacao_cultura,'teatro de fantoches e oficinas criativas do livre brincar') ~ "Outra área",
#       .default = 'Não'))
# 
# 
# d1 = data.frame(table(visuais$area_atuacao_carnaval))
# d2 = data.frame(table(visuais$area_atuacao_musica))
# d3 = data.frame(table(visuais$area_atuacao_danca))
# d4 = data.frame(table(visuais$area_atuacao_teatro))
# d5 = data.frame(table(visuais$area_atuacao_outros))
# 
# d1 = d1 %>% filter(Var1!="Não")
# d2 = d2 %>% filter(Var1!="Não")
# d3 = d3 %>% filter(Var1!="Não")
# d4 = d4 %>% filter(Var1!="Não")
# d5 = d5 %>% filter(Var1!="Não")
# 
# d = d1 %>% add_row(d2)
# d = d %>% add_row(d3)
# d = d %>% add_row(d4)
# d = d %>% add_row(d5)
# 
# 
# d$percentual = (d$Freq/sum(d$Freq))
# 
# library(ggplot2)
# ggplot(d) +
#   aes(x = Var1, y = percentual) +
#   geom_col(fill='#b02c57') +
#   labs(x = "Área de Atuação", y = "Percentual", 
#        subtitle = "Dentro da área cultural, em qual categoria seu trabalho se enquadra?", 
#        caption = "Fonte: ECOA", fill = "Categoria") +
#   theme_minimal() +
#   ecoar_theme2()
# 
# ggsave("graficos/entrega1_visual_categoria.png",width = 20, height = 14, units = "cm")
# 
# remove(d,d1,d2,d3,d4,d5)
#-----------------------------------------------------------------------------------------

table(espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra_voce_pode_marcar_mais_de_uma_opcao_13)
table(espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra_voce_pode_marcar_mais_de_uma_opcao_14)

espetaculo$area_de_atuacao_cultura = tolower(espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra_voce_pode_marcar_mais_de_uma_opcao_13)
table(espetaculo$area_de_atuacao_cultura)

espetaculo = espetaculo %>%
  mutate(
    area_atuacao_artistico = case_when(
      str_detect(area_de_atuacao_cultura,'artístico') ~ "Artístico",
      .default = 'Não'))
espetaculo = espetaculo %>%
  mutate(
    area_atuacao_producao = case_when(
      str_detect(area_de_atuacao_cultura,'produção/gestão') ~ "Produção/Gestão",
      str_detect(area_de_atuacao_cultura,'gestor cultural na area de carnaval') ~ "Produção/Gestão",
      .default = 'Não'))

espetaculo = espetaculo %>%
  mutate(
    area_atuacao_tec = case_when(
      str_detect(area_de_atuacao_cultura,'técnico') ~ "Técnico",
      .default = 'Não'))

espetaculo = espetaculo %>%
  mutate(
    area_atuacao_pesquisa = case_when(
      str_detect(area_de_atuacao_cultura,'pesquisadora') ~ "Pesquisa",
      str_detect(area_de_atuacao_cultura,'pesquisa') ~ "Pesquisa",
      .default = 'Não'))


d1 = data.frame(table(espetaculo$area_atuacao_artistico))
d2 = data.frame(table(espetaculo$area_atuacao_producao))
d3 = data.frame(table(espetaculo$area_atuacao_tec))
d4 = data.frame(table(espetaculo$area_atuacao_pesquisa))

d1 = d1 %>% filter(Var1!="Não")
d2 = d2 %>% filter(Var1!="Não")
d3 = d3 %>% filter(Var1!="Não")
d4 = d4 %>% filter(Var1!="Não")

d = d1 %>% add_row(d2)
d = d %>% add_row(d3)
d = d %>% add_row(d4)

d$percentual = (d$Freq/sum(d$Freq))*100

d$percentual = round(d$percentual,2)
d
50+35.54+13.25+1.2

library(ggplot2)
ggplot(d) +
  aes(x = Var1, y = percentual) +
  geom_col(fill='#b02c57') +
  labs(x = "Área de Atuação", y = "Percentual", 
       subtitle = "Dentro da área cultural, em qual categoria seu trabalho se enquadra?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  ecoar_theme2()

ggsave("graficos_v3/entrega1_espetaculo_categoria.png",width = 20, height = 14, units = "cm")

remove(d,d1,d2,d3,d4) 

