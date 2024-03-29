#----------------------------------------------------------------------------------------
# carregar
#----------------------------------------------------------------------------------------
library(stringr)
library(readxl)
library(dplyr)
library(janitor)

# linux
#espetaculo <- read_excel("~/Área de Trabalho/ecoar_uff/ECOA Niterói - Artes do Espetáculo.xlsx") %>% clean_names()
#visuais <- read_excel("~/Área de Trabalho/ecoar_uff/ECOA Niterói - Artes Visuais.xlsx") %>% clean_names()

# windows casa da gloria
visuais <- read_excel("~/diretorioR/ecoar_uff/19_03_2024/Cópia Steven  ECOA Niterói - Artes Visuais (44 respostas).xlsx") %>% clean_names()
espetaculo <- read_excel("~/diretorioR/ecoar_uff/19_03_2024/Cópia Steven ECOA Niterói - Artes do Espetáculo (104 respostas).xlsx") %>% clean_names()

names(espetaculo)

table(espetaculo$autoriza_o_uso_da_informacao_para_finalidade_academica_e_cientifica)
table(visuais$autoriza_o_uso_da_informacao_para_finalidade_academica_e_cientifica)

#PRIMEIRA ENTREGA: Eixo de Análise (variáveis explicativas)

table(espetaculo$como_voce_se_identifica_em_relacao_ao_genero_preencha_em_letra_maiuscula_sem_abreviacoes_ou_acentos_cedilhas)
table(visuais$como_voce_se_identifica_em_relacao_ao_genero_ex_mulher_homem_mulher_cis_mulher_trans_homem_cis_homem_trans_nao_binario_etc_preencha_em_letra_maiuscula_sem_abreviacoes_ou_acentos_cedilhas)

espetaculo$como_voce_se_identifica_em_relacao_ao_genero = espetaculo$como_voce_se_identifica_em_relacao_ao_genero_preencha_em_letra_maiuscula_sem_abreviacoes_ou_acentos_cedilhas
visuais$como_voce_se_identifica_em_relacao_ao_genero = visuais$como_voce_se_identifica_em_relacao_ao_genero_ex_mulher_homem_mulher_cis_mulher_trans_homem_cis_homem_trans_nao_binario_etc_preencha_em_letra_maiuscula_sem_abreviacoes_ou_acentos_cedilhas


visuais$como_voce_se_identifica_em_relacao_ao_genero = gsub('EMPRESA',NA,visuais$como_voce_se_identifica_em_relacao_ao_genero)

table(visuais$como_voce_se_identifica_em_relacao_a_raca)
visuais$como_voce_se_identifica_em_relacao_a_raca = gsub('(Preto\\(a\\))','Preto(a)/Pardo(a)',visuais$como_voce_se_identifica_em_relacao_a_raca)
visuais$como_voce_se_identifica_em_relacao_a_raca = gsub('^(Pardo\\(a\\))','Preto(a)/Pardo(a)',visuais$como_voce_se_identifica_em_relacao_a_raca)

espetaculo$como_voce_se_identifica_em_relacao_a_raca = gsub('(Preto\\(a\\))','Preto(a)/Pardo(a)',espetaculo$como_voce_se_identifica_em_relacao_a_raca)
espetaculo$como_voce_se_identifica_em_relacao_a_raca = gsub('^(Pardo\\(a\\))','Preto(a)/Pardo(a)',espetaculo$como_voce_se_identifica_em_relacao_a_raca)


#re-order factor levels 
espetaculo$como_voce_se_identifica_em_relacao_a_raca <- factor(espetaculo$como_voce_se_identifica_em_relacao_a_raca, levels=c('Branco(a)','Preto(a)/Pardo(a)','Amarelo(a)','Indígena','Outros','Não sei / Não quero responder'))
visuais$como_voce_se_identifica_em_relacao_a_raca <- factor(visuais$como_voce_se_identifica_em_relacao_a_raca, levels=c('Branco(a)','Preto(a)/Pardo(a)','Amarelo(a)','Indígena','Outros','Não sei / Não quero responder'))
espetaculo$ha_quanto_tempo_voce_atua_nessa_area <- factor(espetaculo$ha_quanto_tempo_voce_atua_nessa_area, levels=c('Até 5 anos', 'Mais de 5 até 10 anos', 'Mais de 10 até 15 anos', 'Mais de 15 anos'))
visuais$ha_quanto_tempo_voce_atua_nessa_area <- factor(visuais$ha_quanto_tempo_voce_atua_nessa_area, levels=c('Até 5 anos', 'Mais de 5 até 10 anos', 'Mais de 10 até 15 anos', 'Mais de 15 anos'))


espetaculo = espetaculo %>%
  mutate(
    faixa_idade = case_when(
      em_qual_faixa_etaria_voce_se_encontra=='18 a 21 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='22 a 24 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='25 a 29 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='30 a 34 anos' ~ "30 a 39 anos",
      em_qual_faixa_etaria_voce_se_encontra=='35 a 39 anos' ~ "30 a 39 anos",
      em_qual_faixa_etaria_voce_se_encontra=='40 a 44 anos' ~ "40 a 49 anos",
      em_qual_faixa_etaria_voce_se_encontra=='45 a 49 anos' ~ "40 a 49 anos",
      em_qual_faixa_etaria_voce_se_encontra=='50 a 54 anos' ~ "50 a 60 anos",
      em_qual_faixa_etaria_voce_se_encontra=='55 a 60 anos' ~ "50 a 60 anos",
      em_qual_faixa_etaria_voce_se_encontra=='Mais de 60' ~ "Mais de 60 anos",
      TRUE                      ~ "outros"))

visuais = visuais %>%
  mutate(
    faixa_idade = case_when(
      em_qual_faixa_etaria_voce_se_encontra=='18 a 21 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='22 a 24 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='25 a 29 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='30 a 34 anos' ~ "30 a 39 anos",
      em_qual_faixa_etaria_voce_se_encontra=='35 a 39 anos' ~ "30 a 39 anos",
      em_qual_faixa_etaria_voce_se_encontra=='40 a 44 anos' ~ "40 a 49 anos",
      em_qual_faixa_etaria_voce_se_encontra=='45 a 49 anos' ~ "40 a 49 anos",
      em_qual_faixa_etaria_voce_se_encontra=='50 a 54 anos' ~ "50 a 60 anos",
      em_qual_faixa_etaria_voce_se_encontra=='55 a 60 anos' ~ "50 a 60 anos",
      em_qual_faixa_etaria_voce_se_encontra=='Mais de 60' ~ "Mais de 60 anos",
      TRUE                      ~ "outros"))

#---------------------------------------------------------------
# AREA DO PLANEJAMENTO
#---------------------------------------------------------------

AP <- read_excel("mapas/AP.xlsx") %>% clean_names()
head(AP)

visuais = visuais %>% rename(bairro=qual_seu_bairro)
table(visuais$bairro)

visuais = visuais %>% left_join(AP)


espetaculo = espetaculo %>% rename(bairro=qual_seu_bairro)
table(espetaculo$bairro)

espetaculo = espetaculo %>% left_join(AP)
table(espetaculo$regiao)

#---------------------------------------------------------------
# ECOAR THEME
#---------------------------------------------------------------
ecoar_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#b8cfc6"),    
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(family = "Broadway",face = "bold", size = 28,color = "#dedfe0"),
    plot.subtitle = element_text(family = "Broadway", face = "bold", # size = 15,
                                 color = "#b02c57"),
    strip.text = element_text(family = "Satisfy",size = 13,face = "bold", color = "#b02c57"),
    strip.background = element_rect(fill = "#bfbbb4", color = "#b02c57"),
    panel.background = element_rect(fill = "#bfbbb4", color = "#b02c57"),
    axis.title = element_text(family = "Roboto", face = "bold", 
                              color = "#b02c57"),
    plot.caption = element_text(family = "Roboto",size = 8,
                                color = "#b02c57",margin = margin(t = 15)),
    axis.text = element_text(family = "Roboto", color = "#b02c57"),
    axis.title.x = element_text(margin = margin(t = 15), hjust = 1,size = 15, color = "#c6ced6"),
    axis.title.y = element_text(margin = margin(r = 15), hjust = 1),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#b02c57", linetype = "dashed"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.text = element_text(colour = "#b02c57"),
    legend.title = element_text(colour = "#b02c57"),
    #panel.grid.major.y = element_line(color = "#364049")#,
    #panel.grid.minor.y = element_line(color = "#364049")#,c("Como você se identifica em relação ao gênero? (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Como você se identifica em relação à raça?", "Você possui alguma deficiência?", "Em qual faixa etária você se encontra?", "Qual seu grau de escolaridade?", "Qual a média da sua renda individual mensal levando em conta os salários anteriores a pandemia (considerando o salário mínimo atual)?", "Em qual cidade você nasceu? (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Você é morador e/ou trabalha em Niterói?", "Qual seu bairro?", "Dentro da área cultural", "em qual categoria seu trabalho se enquadra? (Você pode marcar mais de uma opção)", "Dentro da área cultural", "em qual categoria seu trabalho se enquadra? (Você pode marcar mais de uma opção)", "Qual a sua principal área de atuação no campo da cultura?", "Qual a sua principal área de atuação no campo da cultura?", "Você pode especificar sua área", "por favor? (Ex: música erudita) (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Qual é sua ocupação dentro desta área? (Ex: baterista) (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Qual é sua ocupação dentro desta área? (Ex: baterista) (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Há quanto tempo você atua nessa área?", "Faz parte de algum grupo artístico?", "Qual o nome do grupo artístico (coletivos", "agremiações", "trupes", "blocos", "companhias e etc) que você participa? (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Há quanto tempo esse grupo existe?", "Há quanto tempo você participa desse grupo?", "Com quais linguagens esse grupo trabalha? (Ex: música", "dança) (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Há financiamento para a existência do grupo?", "Qual foi a sua principal motivação para inserção profissional na área cultural?", "Qual é a sua carga de trabalho semanal na área cultural? (Considerando todo o processo de trabalho: pré-produção", "produção e pós-produção)", "Como se dá o seu vínculo trabalhista na área cultural? (Você pode marcar mais de uma opção)", "Como se dá o seu vínculo trabalhista na área cultural? (Você pode marcar mais de uma opção)", "Todos os seus trabalhos na área cultural se firmam a partir de contrato?", "Você já teve contratos de trabalho na área cultural desrespeitados?", "Você trabalha exclusivamente na área cultural?", "Caso não trabalhe exclusivamente na área cultural", "qual é a sua outra área de ocupação? (Você pode marcar mais de uma opção)", "Caso não trabalhe exclusivamente na área cultural", "qual é a sua outra área de ocupação? (Você pode marcar mais de uma opção)", "Qual é a sua carga de trabalho semanal em outra área que não a da cultura?", "Na sua outra área de ocupação como se dá o seu vínculo trabalhista?", "Pensando em sua aposentadoria", "você contribui para o INSS?", "Você contribui para a previdência privada ou complementar?", "Qual (ou quais) desses benefícios você recebe? (Você pode marcar mais de uma opção)", "Você possui plano de saúde?", "Você participa de alguma organização de classe na área da cultura?", "Segundo a sua experiência", "como você avalia a média das instalações físicas na área cultural da cidade de Niterói (espaços culturais", "museus", "casas de espetáculo", "espaços de apresentação teatros", "etc)?", "As suas relações de trabalho foram afetadas durante a pandemia?", "A pandemia afetou a sua carga horária dedicada ao trabalho na área da cultura?", "A pandemia afetou sua vida financeira?  (Você pode marcar mais de uma)", "Em algum momento da pandemia foi necessário complementar sua renda?", "Se teve necessidade de complementar renda", "como fez/tem feito? (Você pode marcar mais de uma)", "Você recebeu o Auxílio Emergencial e/ou participou de algum edital público? (Você pode marcar mais de uma opção)", "Você recebeu o Auxílio Emergencial e/ou participou de algum edital público? (Você pode marcar mais de uma opção)", "Considerando o uso das tecnologias e redes digitais", "quais foram os impactos no seu fazer cultural durante a pandemia do COVID-19 (uso das redes digitais para divulgação do trabalho", "para oferta de serviço e/ou busca de oportunidades de emprego ou uso das plataformas digitais para disponibilização do trabalho", "por exemplo)? (Você pode marcar mais de uma opção)", "O que você apontaria como maior dificuldade em seu trabalho na área da cultura?", "O que você apontaria como o maior potencial em seu trabalho na área da cultura?")
    
    #panel.background = element_rect(fill = "#1e242a", color = "#22292F")
    #panel.spacing = unit(3, "lines"),  ) 
  ) 
}



ecoar_theme2 <- function() {
  theme(
    plot.background = element_rect(fill = "#b8cfc6"),    
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(family = "Broadway",face = "bold", size = 28,color = "#dedfe0"),
    plot.subtitle = element_text(family = "Broadway", face = "bold", # size = 15,
                                 color = "#b02c57"),
    strip.text = element_text(family = "Satisfy",size = 13,face = "bold", color = "#b02c57"),
    strip.background = element_rect(fill = "#bfbbb4", color = "#b02c57"),
    panel.background = element_rect(fill = "#bfbbb4", color = "#b02c57"),
    axis.title = element_text(family = "Roboto", face = "bold", 
                              color = "#b02c57"),
    plot.caption = element_text(family = "Roboto",size = 8,
                                color = "#b02c57",margin = margin(t = 15)),
    axis.text = element_text(family = "Roboto", color = "#b02c57"),
    axis.title.x = element_text(margin = margin(t = 15), hjust = 1,size = 15, color = "#c6ced6"),
    axis.title.y = element_text(margin = margin(r = 15), hjust = 1),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#b02c57", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.text = element_text(colour = "#b02c57"),
    legend.title = element_text(colour = "#b02c57"),
  ) 
}




perguntas = c("Como você se identifica em relação ao gênero?",
              "Como você se identifica em relação à raça?",
              "Você possui alguma deficiência?",
              "Em qual faixa etária você se encontra?",
              "Qual seu grau de escolaridade?",
              "Qual a média da sua renda individual mensal levando em conta os salários anteriores a pandemia (considerando o salário mínimo atual)?",
              "Em qual cidade você nasceu? ",
              "Você é morador e/ou trabalha em Niterói?",
              "Qual seu bairro?",
              "Dentro da área cultural, em qual categoria seu trabalho se enquadra? (Você pode marcar mais de uma opção)",
              "Dentro da área cultural, em qual categoria seu trabalho se enquadra? (Você pode marcar mais de uma opção)",
              "Qual a sua principal área de atuação no campo da cultura?",
              "Qual a sua principal área de atuação no campo da cultura?",
              "Você pode especificar sua área, por favor? (Ex: música erudita) ",
              "Qual é sua ocupação dentro desta área? (Ex: baterista) ",
              "Qual é sua ocupação dentro desta área? (Ex: baterista) ",
              "Há quanto tempo você atua nessa área?",
              "Faz parte de algum grupo artístico?",
              "Qual o nome do grupo artístico (coletivos, agremiações, trupes, blocos, companhias e etc) que você participa? ",
              "Há quanto tempo esse grupo existe?",
              "Há quanto tempo você participa desse grupo?",
              "Com quais linguagens esse grupo trabalha? (Ex: música, dança) ",
              "Há financiamento para a existência do grupo?",
              "Qual foi a sua principal motivação para inserção profissional na área cultural?",
              "Qual é a sua carga de trabalho semanal na área cultural? (Considerando todo o processo de trabalho: pré-produção, produção e pós-produção)",
              "Como se dá o seu vínculo trabalhista na área cultural? (Você pode marcar mais de uma opção)",
              "Como se dá o seu vínculo trabalhista na área cultural? (Você pode marcar mais de uma opção)",
              "Todos os seus trabalhos na área cultural se firmam a partir de contrato?",
              "Você já teve contratos de trabalho na área cultural desrespeitados?",
              "Você trabalha exclusivamente na área cultural?",
              "Caso não trabalhe exclusivamente na área cultural, qual é a sua outra área de ocupação? (Você pode marcar mais de uma opção)",
              "Caso não trabalhe exclusivamente na área cultural, qual é a sua outra área de ocupação? (Você pode marcar mais de uma opção)",
              "Qual é a sua carga de trabalho semanal em outra área que não a da cultura?",
              "Na sua outra área de ocupação como se dá o seu vínculo trabalhista?",
              "Pensando em sua aposentadoria, você contribui para o INSS?",
              "Você contribui para a previdência privada ou complementar?",
              "Qual (ou quais) desses benefícios você recebe? (Você pode marcar mais de uma opção)",
              "Você possui plano de saúde?",
              "Você participa de alguma organização de classe na área da cultura?",
              "Segundo a sua experiência, como você avalia a média das instalações físicas na área cultural da cidade de Niterói (espaços culturais, museus, casas de espetáculo, espaços de apresentação teatros, etc)?",
              "As suas relações de trabalho foram afetadas durante a pandemia?",
              "A pandemia afetou a sua carga horária dedicada ao trabalho na área da cultura?",
              "A pandemia afetou sua vida financeira?  (Você pode marcar mais de uma)",
              "Em algum momento da pandemia foi necessário complementar sua renda?",
              "Se teve necessidade de complementar renda, como fez/tem feito? (Você pode marcar mais de uma)",
              "Você recebeu o Auxílio Emergencial e/ou participou de algum edital público? (Você pode marcar mais de uma opção)",
              "Você recebeu o Auxílio Emergencial e/ou participou de algum edital público? (Você pode marcar mais de uma opção)",
              "Considerando o uso das tecnologias e redes digitais, quais foram os impactos no seu fazer cultural durante a pandemia do COVID-19 (uso das redes digitais para divulgação do trabalho, para oferta de serviço e/ou busca de oportunidades de emprego ou uso das plataformas digitais para disponibilização do trabalho, por exemplo)? (Você pode marcar mais de uma opção)",
              "O que você apontaria como maior dificuldade em seu trabalho na área da cultura?",
              "O que você apontaria como o maior potencial em seu trabalho na área da cultura?")

remove(AP)
