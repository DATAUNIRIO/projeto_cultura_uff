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

bairros_av = data.frame(table(artes_visuais$bairro))

#-----------------------------------------------------------------------------------------------------

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
          "dificuldade_em_seu_trabalho_na_area_da_cultura",                                                       "maior_potencial_em_seu_trabalho_na_area_da_cultura",
          "sugestao")   

NOMES_COMPLETO = names(espetaculo)

colnames(espetaculo) = nomes
bairros_espetaculo = data.frame(table(espetaculo$qual_seu_bairro))

#-----------------------------------------------------------------------------------------------------


library(geojsonio)
bairros_niteroi <- geojson_read("C:/Users/08451589707/Documents/GitHub/projeto_cultura_uff/mapas/Limite_de_Bairros.geojson",  what = "sp")
names(bairros_niteroi)


head(bairros_niteroi$tx_nome)
head(bairros_av$Var1)

class(bairros_niteroi$tx_nome)
class(bairros_av$Var1)

bairros_av$Var1 = as.character(bairros_av$Var1)
bairros_espetaculo$Var1 = as.character(bairros_espetaculo$Var1)

library(dplyr)

bairros_av = bairros_av %>% rename(tx_nome = Var1) 
bairros_av = bairros_av %>% rename(artesvisuais = Freq) 

bairros_espetaculo = bairros_espetaculo %>% rename(tx_nome = Var1) 
bairros_espetaculo = bairros_espetaculo %>% rename(espetaculo = Freq) 

bairros_niteroi@data = bairros_niteroi@data %>% left_join(bairros_av)
bairros_niteroi@data = bairros_niteroi@data %>% left_join(bairros_espetaculo)

#library(sf)
#st_write(bairros_niteroi, "bairros_niteroi.shp"))


summary(bairros_niteroi@data$artesvisuais)
summary(bairros_niteroi@data$espetaculo)


library(leaflet)
bins <- c(0, 2 , 5, 10, Inf)
pal <- colorBin("YlOrRd", domain = bairros_niteroi$artesvisuais, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g Artes Visuais ",
  bairros_niteroi$tx_nome, bairros_niteroi$artesvisuais) %>% lapply(htmltools::HTML)

leaflet(bairros_niteroi) %>%
  addTiles() |>
  addProviderTiles("Esri.WorldImagery") %>%
  #setView(-96, 37.8, 4) %>%
  addPolygons(
    fillColor = ~pal(artesvisuais),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~artesvisuais, opacity = 0.7, title = NULL,
            position = "bottomright")

#--------------------------------------------------------------------------------------

bins2 <- c(0, 2 , 5, 10, Inf)
pal2 <- colorBin("YlOrRd", domain = bairros_niteroi$espetaculo, bins = bins2)

labels2 <- sprintf(
  "<strong>%s</strong><br/>%g Espetaculo ",
  bairros_niteroi$tx_nome, bairros_niteroi$espetaculo) %>% lapply(htmltools::HTML)

leaflet(bairros_niteroi) %>%
  addTiles() |>
  addProviderTiles("Esri.WorldImagery") %>%
  #setView(-96, 37.8, 4) %>%
  addPolygons(
    fillColor = ~pal(espetaculo),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal2, values = ~espetaculo, opacity = 0.7, title = NULL,
            position = "bottomright")


#--------------------------------------------------------------------------------------------


mapa = leaflet(bairros_niteroi) %>%
  addTiles() |>
  addProviderTiles("Esri.WorldImagery") %>%
  #setView(-96, 37.8, 4) %>%
  addPolygons(
    fillColor = ~pal(artesvisuais),
    group="Artes Visuais",
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  
  addPolygons(
    fillColor = ~pal(espetaculo),
    group="Espetáculo",
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLayersControl(
    baseGroups = c("Artes Visuais", "Espetáculo")) %>%
  addLegend(pal = pal2, values = ~espetaculo, opacity = 0.7, title = NULL,
            position = "bottomright")

  
mapa  

library(htmlwidgets)
saveWidget(mapa, file="C:/Users/08451589707/Documents/GitHub/projeto_cultura_uff/mapa2/mapa.html")
