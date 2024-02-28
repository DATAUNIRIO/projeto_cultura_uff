library(readxl)
library(janitor)
library(dplyr)
library(stringr)

#-----------------------------------------------------
#     CORES
library("scales")
library(ggsci)
pal_tron("legacy")(7)
show_col(pal_tron("legacy")(7))

#-----------------------------------------------------
artes_visuais <- read_excel("C:/Users/08451589707/Desktop/pasta_pessoal/LA_UFF/ECOA Niterói - Artes Visuais (respostas).xlsx") %>% clean_names()
artes_visuais = artes_visuais[,4:53]

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
remove(nomes,NOMES_COMPLETO)
#-----------------------------------------------------------------------------------------------------
library(forcats)
table(artes_visuais$id_raca)
artes_visuais$id_raca = forcats::fct_lump_n(artes_visuais$id_raca,n=3,other_level = "Outros(as)")
table(artes_visuais$id_raca)
#-----------------------------------------------------------------------------------------------------

table(artes_visuais$area_de_atuacao_cultura)

artes_visuais$area_de_atuacao_cultura = gsub("Artes Visuais, Ilustrações, Ensino e Pesquisa, EnsinoArquitetura e Urbanismo","Diferentes linguagens",artes_visuais$area_de_atuacao_cultura)
artes_visuais$area_de_atuacao_cultura = gsub("Mista com Desenho, gravura e pintura","Diferentes linguagens",artes_visuais$area_de_atuacao_cultura)
artes_visuais$area_de_atuacao_cultura = gsub("Pintura, desenho, escultura","Diferentes linguagens",artes_visuais$area_de_atuacao_cultura)
artes_visuais$area_de_atuacao_cultura = gsub("Pintura e Arte Digital","Diferentes linguagens",artes_visuais$area_de_atuacao_cultura)

artes_visuais$area_de_atuacao_cultura = gsub("TEATRO","Teatro",artes_visuais$area_de_atuacao_cultura)
artes_visuais$area_de_atuacao_cultura = gsub("ILUSTRAÇÃO","Ilustração",artes_visuais$area_de_atuacao_cultura)           
artes_visuais$area_de_atuacao_cultura = gsub("Organização de feira de EcoSol","Organização de feira",artes_visuais$area_de_atuacao_cultura)           

#--------------------------------------------------------------------------

artes_visuais = artes_visuais %>%
  mutate(
    voce_possui_plano_de_saude = case_when(
      str_detect(voce_possui_plano_de_saude,'Não possuo plano de saúde') ~ "Não",
      str_detect(voce_possui_plano_de_saude,'Sim, plano particular') ~ "Sim, plano particular",
      str_detect(voce_possui_plano_de_saude,'Aeronáutica (militar)') ~ "Sim, plano da empresa/sindicato",
      str_detect(voce_possui_plano_de_saude,'Sim, plano de saúde vinculado a CLT') ~ "Sim, plano da empresa/sindicato",
      str_detect(voce_possui_plano_de_saude,'Sim, plano do sindicato') ~ "Sim, plano da empresa/sindicato",
      .default = NA))

table(artes_visuais$voce_possui_plano_de_saude)                  

#-----------------------------------------------------------------------------------------------

artes_visuais$voce_contribui_para_a_previdencia_privada_ou_complementar = gsub('Sim, com recursos oriundos do trabalho no campo da cultura','Sim, com recursos da cultura',artes_visuais$voce_contribui_para_a_previdencia_privada_ou_complementar)
artes_visuais$voce_contribui_para_a_previdencia_privada_ou_complementar = gsub('Sim, com recursos oriundos do meu trabalho em outras áreas','Sim, com recursos de outras áreas',artes_visuais$voce_contribui_para_a_previdencia_privada_ou_complementar)
artes_visuais$voce_contribui_para_a_previdencia_privada_ou_complementar = gsub('Não sei  / Não quero responder',NA,artes_visuais$voce_contribui_para_a_previdencia_privada_ou_complementar)
table(artes_visuais$voce_contribui_para_a_previdencia_privada_ou_complementar)

#-----------------------------------------------------------------------------------------------
a = table(artes_visuais$voce_recebeu_e_ou_participou_de_algum_edital_publico) %>% data.frame()
artes_visuais$edital_publico3 = artes_visuais$voce_recebeu_e_ou_participou_de_algum_edital_publico
library(tidyr)
artes_visuais = artes_visuais %>% separate(voce_recebeu_e_ou_participou_de_algum_edital_publico, into = c("edital_publico", "edital_publico2"), sep = c(3)) 
artes_visuais %>% select(-edital_publico2)

artes_visuais$edital_publico3 = gsub('Não, mas tenho interesse.','Não',artes_visuais$edital_publico3)
artes_visuais$edital_publico3 = gsub('Não, não me sinto habilitado/a para participar','Não',artes_visuais$edital_publico3)
artes_visuais$edital_publico3 = gsub('Não, não tenho interesse','Não',artes_visuais$edital_publico3)
artes_visuais$edital_publico3 = gsub('Não, nunca encontrei um edital que contemplasse o meu fazer cultural','Não',artes_visuais$edital_publico3)
artes_visuais$edital_publico3 = gsub('Não, nunca tive acesso às informações','Não',artes_visuais$edital_publico3)
artes_visuais$edital_publico3 = gsub('Não, Não','Não',artes_visuais$edital_publico3)
a = table(artes_visuais$edital_publico3) %>% data.frame()

artes_visuais = artes_visuais %>%
  mutate(
    edital_publico_federal = case_when(
      str_detect(edital_publico3,'federal') ~ "federal",
      .default = 'Não'))
artes_visuais = artes_visuais %>%
  mutate(
    edital_publico_estadual = case_when(
      str_detect(edital_publico3,'estadual') ~ "estadual",
      .default = 'Não'))
artes_visuais = artes_visuais %>%
  mutate(
    edital_publico_municipal  = case_when(
      str_detect(edital_publico3,'municipal ') ~ "municipal",
      .default = 'Não'))

d1 = data.frame(table(artes_visuais$edital_publico_federal,artes_visuais$id_raca))
d2 = data.frame(table(artes_visuais$edital_publico_estadual,artes_visuais$id_raca))
d3 = data.frame(table(artes_visuais$edital_publico_municipal,artes_visuais$id_raca))

d1 = d1 %>% filter(Var1!="Não")
d2 = d2 %>% filter(Var1!="Não")
d3 = d3 %>% filter(Var1!="Não")

d = d1 %>% add_row(d2)
d = d %>% add_row(d3)
library(ggplot2)
ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c(municipal = "#E91203", 
                               estadual = "#02EBC2", federal = "#0651EB")) +
  labs(x = "cor/raça", y = "quantidade", subtitle = "voce_recebeu_e_ou_participou_de_algum_edital_publico", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(vars(Var1))
remove(d,d1,d2,d3)
remove(a)

#-----------------------------------------------------------------------------------------------
artes_visuais = artes_visuais %>%
  mutate(
    vinculo_CLT_area_cultural = case_when(
      str_detect(como_se_da_o_seu_vinculo_trabalhista_na_area_cultural,'CLT') ~ "CLT",
      .default = 'Não'))
artes_visuais = artes_visuais %>%
  mutate(
    vinculo_MEI_area_cultural = case_when(
      str_detect(como_se_da_o_seu_vinculo_trabalhista_na_area_cultural,'CNPJ') ~ "Não",
      str_detect(como_se_da_o_seu_vinculo_trabalhista_na_area_cultural,'MEI') ~ "MEI",
      .default = 'Não'))
artes_visuais = artes_visuais %>%
  mutate(
    vinculo_auto_area_cultural = case_when(
      str_detect(como_se_da_o_seu_vinculo_trabalhista_na_area_cultural,'autônomo') ~ "autônomo",
      .default = 'Não'))

d1 = data.frame(table(artes_visuais$vinculo_CLT_area_cultural,artes_visuais$id_raca))
d2 = data.frame(table(artes_visuais$vinculo_MEI_area_cultural,artes_visuais$id_raca))
d3 = data.frame(table(artes_visuais$vinculo_auto_area_cultural,artes_visuais$id_raca))

d1 = d1 %>% filter(Var1!="Não")
d2 = d2 %>% filter(Var1!="Não")
d3 = d3 %>% filter(Var1!="Não")

d = d1 %>% add_row(d2)
d = d %>% add_row(d3)
library(ggplot2)
ggplot(d) +
  aes(x = Var2, y = Freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(values = c(CLT = "#E91203", 
                               MEI = "#02EBC2", autônomo = "#0651EB")) +
  labs(x = "cor/raça", y = "quantidade", subtitle = "como_se_da_o_seu_vinculo_trabalhista_na_area_cultural", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(vars(Var1))
remove(d,d1,d2,d3)

#-----------------------------------------------------------------------------------------

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

t1 = artes_visuais %>% 
  select(voce_trabalha_exclusivamente_na_area_cultural) %>%
  group_by(voce_trabalha_exclusivamente_na_area_cultural) %>% na.omit() %>% summarise(cnt = n()) %>% 
  mutate(props = round(cnt / sum(cnt), 2)) # getting frequency
t1$props = t1$props*100

t1 %>% ggplot() +
  aes(x = voce_trabalha_exclusivamente_na_area_cultural,y=props) +
  geom_col(fill=c('red','royalblue'))+
  geom_text(aes(label = props ), position = position_dodge(0.9), vjust = 0)+
  theme_minimal() +
  theme(legend.position = "bottom")+
  coord_flip() + 
  labs(title = "Gráfico 3 - Trabalha exclusivamente na área de cultura",
       y = 'Proporção',
              x = 'Trabalha exclusivamente na área de cultura')

remove(t1)
#--------------------------------------------------------------------------------------------

table(artes_visuais$se_voce_ja_teve_contratos_de_trabalho_na_area_cultural_responda)
artes_visuais$se_voce_ja_teve_contratos_de_trabalho_na_area_cultural_responda= gsub('Não sei / Não quero responder',NA,artes_visuais$se_voce_ja_teve_contratos_de_trabalho_na_area_cultural_responda)

t2 = artes_visuais %>% 
  select(se_voce_ja_teve_contratos_de_trabalho_na_area_cultural_responda) %>%
  group_by(se_voce_ja_teve_contratos_de_trabalho_na_area_cultural_responda) %>% na.omit() %>% summarise(cnt = n()) %>% 
  mutate(props = round(cnt / sum(cnt), 2)) # getting frequency

library(scales)
t2$props = percent(t2$props,.11)

library("ggsci")


t2 %>% ggplot() +
  aes(x = se_voce_ja_teve_contratos_de_trabalho_na_area_cultural_responda,y=props) +
  geom_col(fill=c("#FF410DFF", "#6EE2FFFF", "#F7C530FF"))+
  geom_text(aes(label = props ), position = position_dodge(0.9), vjust = 0)+
  theme_minimal() +
  theme(legend.position = "bottom")+
  coord_flip() + 
  labs(title = "Gráfico 3 - se_voce_ja_teve_contratos_de_trabalho_na_area_cultural_responda",
       y = 'Proporção',
       x = '')
remove(t2)

#----------------------------------------------------------------------------------------

