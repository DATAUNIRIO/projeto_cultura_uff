
table(visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra_voce_pode_marcar_mais_de_uma_opcao)
table(visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra)

visuais$area_de_atuacao_cultura = tolower(visuais$dentro_da_area_cultural_em_qual_categoria_seu_trabalho_se_enquadra_voce_pode_marcar_mais_de_uma_opcao)

table(visuais$area_de_atuacao_cultura)

#artístico
#artístico, dança de salão 
#artístico, produção/gestão
#artístico, técnico 
#artístico, técnico, produção/gestão 
#artístico, técnico, produção/gestão, pesquisa 
#gestor cultural na area de carnaval                                        
#música 
#produção/gestão
#produção/gestão, pesquisadora 
#técnico                      
#técnico, produção/gestão 


artes_visuais = artes_visuais %>%
  mutate(
    vinculo_artístico = case_when(
      str_detect(como_se_da_o_seu_vinculo_trabalhista_na_area_cultural,'CLT') ~ "CLT",
      .default = 'Não'))
