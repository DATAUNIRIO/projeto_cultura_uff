library(readxl)
library(janitor)

artes_visuais <- read_excel("C:/Users/08451589707/Desktop/pasta_pessoal/LA_UFF/ECOA Niterói - Artes Visuais (respostas).xlsx") %>% clean_names()
#head(artes_visuais)

artes_visuais = artes_visuais[,4:53]
#names(artes_visuais)


nomes = c("id_genero","id_raca","deficiencia","faixa_etaria","escolaridade","renda_media",
          "cidade_voce_nasceu","morador_ou_trabalha_em_niteroi", "bairro", "categoria",
          "area_de_atuacao_cultura","especificar_sua_area","qual_e_sua_ocupacao_dentro_desta_area",
          "ha_quanto_tempo_atua_area","grupo_coletivo_artistico","grupo_em_que_voce_mais_atua",                   "qual_e_o_nome_do_grupo_coletivo_artistico",
          "ha_quanto_tempo_grupo_existe","ha_quanto_tempo_participa_grupo",                                       "com_quais_linguagens_esse_grupo_coletivo_artistico_trabalha",
          "referente_aos_ultimos_cinco_anos_ha_houve_financiamento",
          "principal_motivacao_para_insercao_profissional_na_area_cultural",
          "carga_de_trabalho_semanal_na_area_cultural",                                                           "a_pandemia_de_covid_19_afetou_a_sua_carga_horaria_dedicada_ao_trabalho_na_area_da_cultura",            "como_se_da_o_seu_vinculo_trabalhista_na_area_cultural",                                                "todos_os_seus_trabalhos_na_area_cultural_se_firmam_a_partir_de_contrato",                              "se_voce_ja_teve_contratos_de_trabalho_na_area_cultural_responda",                                      "voce_trabalha_exclusivamente_na_area_cultural",                                                        "qual_e_a_sua_outra_area_de_ocupacao_voce_pode_marcar_mais_de_uma_opcao",                               "qual_e_a_sua_carga_de_trabalho_semanal_em_outra_area_que_nao_a_da_cultura",                            "na_sua_outra_area_de_ocupacao_como_se_da_o_seu_vinculo_trabalhista",                                   "pensando_em_sua_aposentadoria_voce_contribui_para_o_inss",                                             "voce_contribui_para_a_previdencia_privada_ou_complementar",                                            "qual_ou_quais_desses_beneficios_voce_recebe",                                                          "voce_possui_plano_de_saude",
          "avalia_as_instalacoes_fisicas_publicas_ou_privadas_na_area_cultural",
          "voce_recebeu_e_ou_participou_de_algum_edital_publico",
          "voce_participa_de_alguma_organizacao_de_classe_na_area_da_cultura",                                    "se_sim_qual_organizacao_de_classe_na_area_da_cultura_voce_participa",
          "espaco_para_guardar_equipamentos",
          "espaco_possui_para_guardar_obras_equipamentos",
          "instituicoes_auxiliam_na_comercializacao_obras_oferta",                                                "principal_espaco_plataforma_de_comercializacao",                                                       "principal_espaco_plataforma_de_divulgacao",                                                            "investimento_para_a_presenca_online_de_seu_trabalho",
          "uso_tecnologias_impactos_no_fazer_cultural_durante_pandemia_do_covid_19",
          "pandemia_de_covid_19_a_retomada_trabalho_cultural",                                                    "maior_dificuldade_trabalho_na_area_da_cultura",                                                        "maior_potencial_trabalho_na_area_da_cultura",                                                          "sugestao_e_ou_critica") 

NOMES_COMPLETO = names(artes_visuais)

colnames(artes_visuais) = nomes

#-----------------------------------------------------------------------------------------------------

table(artes_visuais$area_de_atuacao_cultura)

artes_visuais$area_de_atuacao_cultura = gsub("Artes Visuais, Ilustrações, Ensino e Pesquisa, EnsinoArquitetura e Urbanismo","Diferentes linguagens",artes_visuais$area_de_atuacao_cultura)
artes_visuais$area_de_atuacao_cultura = gsub("Mista com Desenho, gravura e pintura","Diferentes linguagens",artes_visuais$area_de_atuacao_cultura)
artes_visuais$area_de_atuacao_cultura = gsub("Pintura, desenho, escultura","Diferentes linguagens",artes_visuais$area_de_atuacao_cultura)
artes_visuais$area_de_atuacao_cultura = gsub("Pintura e Arte Digital","Diferentes linguagens",artes_visuais$area_de_atuacao_cultura)

artes_visuais$area_de_atuacao_cultura = gsub("TEATRO","Teatro",artes_visuais$area_de_atuacao_cultura)
artes_visuais$area_de_atuacao_cultura = gsub("ILUSTRAÇÃO","Ilustração",artes_visuais$area_de_atuacao_cultura)           
artes_visuais$area_de_atuacao_cultura = gsub("Organização de feira de EcoSol","Organização de feira",artes_visuais$area_de_atuacao_cultura)           


rpivotTable(artes_visuais)



library(ggplot2)
library(dplyr)

artes_visuais %>% select(area_de_atuacao_cultura,voce_trabalha_exclusivamente_na_area_cultural) %>% na.omit() %>% ggplot() +
  aes(
    x = area_de_atuacao_cultura,
    fill = voce_trabalha_exclusivamente_na_area_cultural,
    colour = voce_trabalha_exclusivamente_na_area_cultural,
    group = voce_trabalha_exclusivamente_na_area_cultural
  ) +
  geom_bar() +
  scale_fill_manual(
    values = c(Não = "#E4281B",
               Sim = "#382BC6")
  ) +
  scale_color_manual(
    values = c(Não = "#E4281B",
               Sim = "#382BC6")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  coord_flip() + 
  labs(title = "Gráfico 1 - avaliação do trabalho exclusivo na área de cultura por área de atuação",
  y = 'Quantidade',
  x = 'Área de atuação')

  

artes_visuais %>% select(area_de_atuacao_cultura) %>% na.omit() %>% ggplot() +
  aes(
    x = area_de_atuacao_cultura) +
  geom_bar(fill='red')+
  theme_minimal() +
  theme(legend.position = "bottom")+
  coord_flip() + 
  labs(title = "Gráfico 2 - Área de atuação",
       y = 'Quantidade',
       x = 'Área de atuação')


artes_visuais %>% select(voce_trabalha_exclusivamente_na_area_cultural) %>% na.omit() %>% ggplot() +
  aes(x = voce_trabalha_exclusivamente_na_area_cultural) +
  geom_bar(fill=c('red','royalblue'))+
  theme_minimal() +
  theme(legend.position = "bottom")+
  coord_flip() + 
  labs(title = "Gráfico 3 - Trabalha exclusivamente na área de cultura",
       y = 'Quantidade',
       x = 'Trabalha exclusivamente na área de cultura')


