list.files(path = "artigos", full.names=T)


paste0('#### **[1.', list.files(path = "artigos", full.names=T))

# ](https://docs.google.com/viewer?url=https://raw.githubusercontent.com/DATAUNIRIO/biblioteca_virtual/main/artigos/1_diagnostico_da_escolarizacao.pdf)**

  
library(readxl)
library(janitor)
espetaculo <- read_excel("C:/Users/08451589707/Desktop/pasta_pessoal/LA_UFF/ECOA Niterói - Artes do Espetáculo (respostas).xlsx") %>% clean_names()
#str(espetaculo)

#table(espetaculo$autoriza_o_uso_da_informacao_para_finalidade_academica_e_cientifica)
#names(espetaculo)

espetaculo = espetaculo[,4:48]


nomes = c("id_genero","id_raca","deficiencia","faixa_etaria","escolaridade",
          "renda_media","cidade_nasceu","mora_trabalha_em_niteroi",       
          "qual_seu_bairro",
          "categoria",                                                                                            "area_de_atuacao_no_campo_da_cultura",                                                                  "voce_pode_especificar_sua_area",                                                                       "qual_e_sua_ocupacao_dentro_desta_area",                                                                "ha_quanto_tempo_voce_atua_nessa_area",                                                                 "faz_parte_de_algum_grupo_artistico",                                                                   "qual_o_nome_do_grupo_artistico",                                                                       "ha_quanto_tempo_esse_grupo_existe",                                                                    "ha_quanto_tempo_voce_participa_desse_grupo",                                                           "com_quais_linguagens_esse_grupo_trabalha",
          "ha_financiamento",                                                                                     "motivacao_insercao_profissional_na_area_cultural",                                                     "carga_de_trabalho_semanal_na_area_cultural",                                                           "vinculo_trabalhista_na_area_cultural",                                                                 "todos_os_seus_trabalhos_cultural_se_firmam_contrato",                                                  "trabalho_desrespeitados",                                                                              "exclusivamente_na_area_cultural",                                                                      "qual_e_a_sua_outra_area_de_ocupacao",                                                                  "qual_e_a_sua_carga_de_trabalho_semanal_em_outra_area",                                                 "outra_area_ocupacao_vinculo_trabalhista",                                                              "voce_contribui_para_o_inss",                                                                           "voce_contribui_para_a_previdencia_privada",                                                            "qual_ou_quais_desses_beneficios_voce_recebe",                                                          "voce_possui_plano_de_saude",                                                                           "voce_participa_de_organizacao_de_classe_na_cultura",                                                   "como_voce_avalia_instalacoes_fisicas_cultural",                                                        "relacoes_de_trabalho_foram_afetadas_durante_a_pandemia",                                               "a_pandemia_afetou_carga_horaria_trabalho_na_cultura",                                                  "a_pandemia_afetou_sua_vida_financeira",                                                                "pandemia_foi_necessario_complementar_sua_renda",                                                       "complementar_renda_como_fez",                                                                          "auxilio_emergencial_e_edital",                                                                         "redes_digitais_impactos_cultural_pandemia_covid_19",
          "dificuldade_em_seu_trabalho",                                                       "maior_potencial_em_seu_trabalho_na_area_da_cultura",
          "sugestao")   

NOMES_COMPLETO = names(espetaculo)

colnames(espetaculo) = nomes

#library(rpivotTable)
#rpivotTable(espetaculo)

library(forcats)
table(espetaculo$id_raca)
espetaculo$id_raca = forcats::fct_lump_n(espetaculo$id_raca,n=3,other_level = "Outros(as)")

espetaculo$id_raca = gsub("Branco (a)","Branco(a)",espetaculo$id_raca, fixed = TRUE)
espetaculo$id_raca = gsub("Pardo (a)","Pardo(a)",espetaculo$id_raca, fixed = TRUE)

pie(table(espetaculo$id_raca))

library(ggplot2)
ggplot(espetaculo) +
 aes(x = id_raca) +
 geom_bar(fill = "#112446") +
 #coord_polar("y", start=0) +
 labs(x = "Cor/Raça", y = "Quantidade de respostas", 
 subtitle = "Gráfico 1 - Cor/Raça dos entrevistados", caption = "Fonte: ECOA") +
 coord_flip() +
 theme_minimal()

library(waffle)
waffle::waffle(c('Branco(a)'=49.56,'Pardo(a)'=19.47,'Preto(a)'=26,'Outros'=6) )

#--------------------------------------------------------------------------------------
  
table(espetaculo$categoria)
library(dplyr)
espetaculo = espetaculo %>%
  mutate(
    Artistico = case_when(
      categoria=='Artístico' ~ "Artístico",
      categoria=='Artístico, Dança de salão' ~ "Artístico",
      categoria=='Artístico, Produção/Gestão' ~ "Artístico",
      categoria=='Artístico, Técnico' ~ "Artístico",
      categoria=='Artístico, Técnico, Produção/Gestão' ~ "Artístico",
      categoria=='Artístico, Técnico, Produção/Gestão, Pesquisa' ~ "Artístico",
      categoria=='Música' ~ "Artístico",
      TRUE ~ "Não"))
table(espetaculo$Artistico)

library(dplyr)
espetaculo = espetaculo %>%
  mutate(
    Producao_Gestao = case_when(
      categoria=='Artístico, Produção/Gestão' ~ "Produção/Gestão",
      categoria=='Artístico, Técnico, Produção/Gestão' ~ "Produção/Gestão",
      categoria=='Artístico, Técnico, Produção/Gestão, Pesquisa' ~ "Produção/Gestão",
      categoria=='GESTOR CULTURAL NA AREA DE CARNAVAL' ~ "Produção/Gestão",
      categoria=='Produção/Gestão' ~ "Produção/Gestão",
      categoria=='Produção/Gestão, Pesquisadora' ~ "Produção/Gestão",
      categoria=='Técnico, Produção/Gestão' ~ "Produção/Gestão",      
      TRUE ~ "Não"))
table(espetaculo$Producao_Gestao)

espetaculo = espetaculo %>%
  mutate(
    Tecnico  = case_when(
      categoria=='Artístico, Técnico' ~ "Técnico",
      categoria=='Artístico, Técnico, Produção/Gestão' ~ "Técnico",
      categoria=='Artístico, Técnico, Produção/Gestão, Pesquisa' ~ "Técnico",
      categoria=='Técnico' ~ "Técnico",
      categoria=='Técnico, Produção/Gestão' ~ "Técnico",
      TRUE ~ "Não"))
table(espetaculo$Tecnico)

# é interessante olhar quem é artista e gestor ao mesmo tempo?
# ou é mais interessante ver a cor/raça do artistico e a cor raça do gestor?
# em outras palavras, quais são os objetivos da pesquisa?

table(espetaculo$Artistico,espetaculo$Producao_Gestao)

d1 = data.frame(table(espetaculo$Artistico,espetaculo$id_raca))
d2 = data.frame(table(espetaculo$Producao_Gestao,espetaculo$id_raca))
d3 = data.frame(table(espetaculo$Tecnico,espetaculo$id_raca))

d1 = d1 %>% filter(Var1!="Não")
d2 = d2 %>% filter(Var1!="Não")
d3 = d3 %>% filter(Var1!="Não")

d = d1 %>% add_row(d2)
d = d %>% add_row(d3)


library(ggplot2)

ggplot(d) +
 aes(x = Var2, y = Freq, fill = Var1) +
 geom_col() +
 scale_fill_manual(values = c(Artístico = "#E91203", 
`Produção/Gestão` = "#02EBC2", Técnico = "#0651EB")) +
 labs(x = "cor/raça", y = "quantidade", subtitle = "Dentro da área cultural, em qual categoria seu trabalho se enquadra?", 
 caption = "Fonte: ECOA", fill = "Categoria") +
 theme_minimal() +
 theme(legend.position = "bottom") +
 facet_wrap(vars(Var1))



#-----------------------------------------------------------------------------------------------
table(espetaculo$vinculo_trabalhista_na_area_cultural)
espetaculo = espetaculo %>%
  mutate(
    vinculo_CLT_area_cultural = case_when(
      str_detect(vinculo_trabalhista_na_area_cultural,'CLT') ~ "CLT",
      .default = 'Não'))
espetaculo = espetaculo %>%
  mutate(
    vinculo_MEI_area_cultural = case_when(
      str_detect(vinculo_trabalhista_na_area_cultural,'CNPJ') ~ "Não",
      str_detect(vinculo_trabalhista_na_area_cultural,'MEI') ~ "MEI",
      .default = 'Não'))
espetaculo = espetaculo %>%
  mutate(
    vinculo_auto_area_cultural = case_when(
      str_detect(vinculo_trabalhista_na_area_cultural,'autônomo') ~ "autônomo",
      .default = 'Não'))
espetaculo = espetaculo %>%
  mutate(
    vinculo_cnpj_area_cultural = case_when(
      str_detect(vinculo_trabalhista_na_area_cultural,'Empreendedor com CNPJ') ~ "CNPJ",
      .default = 'Não'))

espetaculo = espetaculo %>%
  mutate(
    vinculo_outros_area_cultural = case_when(
      str_detect(vinculo_trabalhista_na_area_cultural,'AMADORA') ~ "Outros (Amador/Estatutário/Estagiário(a)/)",
      str_detect(vinculo_trabalhista_na_area_cultural,'estatutário') ~ "Outros (Amador/Estatutário/Estagiário(a)/)",
      str_detect(vinculo_trabalhista_na_area_cultural,'Estagiário(a)') ~ "Outros (Amador/Estatutário/Estagiário(a)/)",
      str_detect(vinculo_trabalhista_na_area_cultural,'ESTATUTARIA') ~ "Outros (Amador/Estatutário/Estagiário(a)/)",
      .default = 'Não'))

d1 = data.frame(table(espetaculo$vinculo_CLT_area_cultural,espetaculo$id_raca))
d2 = data.frame(table(espetaculo$vinculo_MEI_area_cultural,espetaculo$id_raca))
d3 = data.frame(table(espetaculo$vinculo_auto_area_cultural,espetaculo$id_raca))
d4 = data.frame(table(espetaculo$vinculo_cnpj_area_cultural,espetaculo$id_raca))
d5 = data.frame(table(espetaculo$vinculo_outros_area_cultural,espetaculo$id_raca))


d1 = d1 %>% filter(Var1!="Não")
d2 = d2 %>% filter(Var1!="Não")
d3 = d3 %>% filter(Var1!="Não")
d4 = d4 %>% filter(Var1!="Não")
d5 = d5 %>% filter(Var1!="Não")

d = d1 %>% add_row(d2)
d = d %>% add_row(d3)
d = d %>% add_row(d4)
d = d %>% add_row(d5)
remove(d1,d2,d3,d4,d5)

library(ggplot2)

ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c(CLT = "#E91203", 
                               MEI = "#02EBC2", autônomo = "#0651EB",CNPJ="yellow",`Outros (Amador/Estatutário/Estagiário(a)/)`="gray")) +
  labs(x = "cor/raça", y = "quantidade", subtitle = "vinculo_trabalhista_na_area_cultural", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(vars(Var1))




table(espetaculo$outra_area_ocupacao_vinculo_trabalhista)
espetaculo = espetaculo %>%
  mutate(
    vinculo_CLT_area_cultural = case_when(
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'CLT') ~ "CLT",
      .default = 'Não'))
espetaculo = espetaculo %>%
  mutate(
    vinculo_MEI_area_cultural = case_when(
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'CNPJ') ~ "Não",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'MEI') ~ "MEI",
      .default = 'Não'))
espetaculo = espetaculo %>%
  mutate(
    vinculo_auto_area_cultural = case_when(
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'autônomo') ~ "autônomo",
      .default = 'Não'))
espetaculo = espetaculo %>%
  mutate(
    vinculo_cnpj_area_cultural = case_when(
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'Empreendedor com CNPJ') ~ "CNPJ",
      .default = 'Não'))

espetaculo = espetaculo %>%
  mutate(
    vinculo_outros_area_cultural = case_when(
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'CULTURAL') ~ "Exclusivo na área de cultura",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'Nada') ~ "Exclusivo na área de cultura",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'NAO') ~ "Exclusivo na área de cultura",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'não atuo em outra área') ~ "Exclusivo na área de cultura",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'NÃO ATUO EM OUTRA ÁREA') ~ "Exclusivo na área de cultura",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'NAO POSSUO') ~ "Exclusivo na área de cultura",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'NAO POSSUO') ~ "Exclusivo na área de cultura",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'Não tenho') ~ "Exclusivo na área de cultura",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'TRABALHO EXCLUSIVAMENTE COM CULTURA') ~ "Exclusivo na área de cultura",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'Trabalho exclusivamente na área da cultura') ~ "Exclusivo na área de cultura",
        .default = 'Não'))

table(espetaculo$vinculo_CLT_area_cultural,espetaculo$vinculo_CLT_area_cultural)

espetaculo = espetaculo %>%
  mutate(
    vinculo_outros_area_cultural = case_when(
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'PORTEIRO') ~ "Outros (Estatutário/Estagiário(a))",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'estatutário') ~ "Outros (Estatutário/Estagiário(a))",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'Estagiário(a)') ~ "Outros (Estatutário/Estagiário(a))",
      str_detect(outra_area_ocupacao_vinculo_trabalhista,'ESTATUTARIA') ~ "Outros (Estatutário/Estagiário(a))",
      .default = 'Não'))
