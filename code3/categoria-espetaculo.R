library(stringr)


table(espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra) 
espetaculo$area_de_atuacao_cultura = tolower(espetaculo$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra)
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

d$percentual = (d$Freq/sum(d$Freq))
d$percentual = d$percentual*100
d$percentual = round(d$percentual,2)

library(ggplot2)
ggplot(d) +
  aes(x = Var1, y = percentual) +
  geom_col(fill='#b02c57') +
  labs(x = "Área de Atuação", y = "Percentual", 
       subtitle = "Dentro da área cultural, em qual categoria seu trabalho se enquadra?*", 
       caption = "* Pergunta com resposta múltipla. Os resultados podem somar mais de 100%") +
  theme_minimal() +
  ecoar_theme2()

ggsave("graficos_v2/entrega1_espetaculo_categoria.png",width = 20, height = 14, units = "cm")

remove(d,d1,d2,d3,d4,d5) 


