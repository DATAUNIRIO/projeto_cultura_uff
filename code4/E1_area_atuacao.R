library(stringr)

table(visuais$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_15)
table(visuais$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)

visuais$area_de_atuacao_cultura = tolower(visuais$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)
data.frame(table(visuais$area_de_atuacao_cultura))

visuais$area_de_atuacao_cultura = gsub('outro: artista','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro: atriz','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro: coordenadora','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro: diretora','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro: mediação  e curadoria','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro: produção','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro: produção-produtora cultural','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro: produçao gestao curadoria projetos','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro: produção, curadoria, pesquisa, design da montagem, gestão da execução da montagem, plano de comunicação','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro, curadoria, pesquisa, design da montagem, gestão da execução da montagem, plano de comunicação','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro...','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro: produtor','outro',visuais$area_de_atuacao_cultura)
visuais$area_de_atuacao_cultura = gsub('outro-produtora cultural','outro',visuais$area_de_atuacao_cultura)

visuais$area_de_atuacao_cultura = gsub('mais de um: artista multidisciplinar autonoma','mais de um',visuais$area_de_atuacao_cultura) 
visuais$area_de_atuacao_cultura = gsub('mais de um: autora, professora, promotora de eventos culturais','mais de um',visuais$area_de_atuacao_cultura)  
visuais$area_de_atuacao_cultura = gsub('mais de um: editora montadora design','mais de um',visuais$area_de_atuacao_cultura) 
visuais$area_de_atuacao_cultura = gsub('mais de um: pintora, desenhista, gravadora','mais de um',visuais$area_de_atuacao_cultura)  
visuais$area_de_atuacao_cultura = gsub('mais de um: videomaker, roteirista, cinegrafista, editor, produtor cultural','mais de um',visuais$area_de_atuacao_cultura) 

visuais$area_de_atuacao_cultura = gsub('outroodutora cultural','outro',visuais$area_de_atuacao_cultura) 
visuais$area_de_atuacao_cultura = gsub('outroodutora cultural','outro',visuais$area_de_atuacao_cultura) 
visuais$area_de_atuacao_cultura = gsub('outrorodutor','outro',visuais$area_de_atuacao_cultura) 
table(visuais$area_de_atuacao_cultura)

visuais$area_de_atuacao_cultura = factor(visuais$area_de_atuacao_cultura,levels = c('mais de um','outro','arte digital','arte urbana','artesanato','fotografia','performance','pintura','tatuagem','videoarte'))
d = data.frame(table(visuais$area_de_atuacao_cultura))
d$percentual = (d$Freq/sum(d$Freq))*100
d

library(ggplot2)
ggplot(d) +
  aes(x = Var1, y = percentual) +
  geom_col(fill='#b02c57') +
  labs(x = "Área de Atuação", y = "Percentual", 
       subtitle = "Artes Visuais: Qual a sua principal área de atuação no campo da cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  theme_minimal() +
  coord_flip() + 
  ecoar_theme2()

ggsave("graficos_v3/entrega1_visual_area_atuacao.png",width = 20, height = 14, units = "cm")

remove(d)
#-----------------------------------------------------------------------------------------

table(espetaculo$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_15)
table(espetaculo$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)

espetaculo$area_de_atuacao = tolower(espetaculo$qual_a_sua_principal_area_de_atuacao_no_campo_da_cultura_16)
table(espetaculo$area_de_atuacao)

espetaculo$area_de_atuacao = gsub('mais de um: artista multidisciplinar autonoma','mais de um',espetaculo$area_de_atuacao) 
espetaculo$area_de_atuacao = gsub('mais de um: carnaval, música','mais de um',espetaculo$area_de_atuacao)
espetaculo$area_de_atuacao = gsub('mais de um: música, teatro','mais de um',espetaculo$area_de_atuacao) 
espetaculo$area_de_atuacao = gsub('mais de um: teatro, dança, carnaval','mais de um',espetaculo$area_de_atuacao)
espetaculo$area_de_atuacao = gsub('mais de um, gestão cultural','mais de um',espetaculo$area_de_atuacao)
espetaculo$area_de_atuacao = gsub('mais de um: teatro, dança, música','mais de um',espetaculo$area_de_atuacao)
espetaculo$area_de_atuacao = gsub('outros: audiovisual','outros',espetaculo$area_de_atuacao) 
espetaculo$area_de_atuacao = gsub('outros: cinema','outros',espetaculo$area_de_atuacao)       
espetaculo$area_de_atuacao = gsub('outros: contação de histórias','outros',espetaculo$area_de_atuacao)              
espetaculo$area_de_atuacao = gsub('outros: cultura urbana','outros',espetaculo$area_de_atuacao) 
espetaculo$area_de_atuacao = gsub('outros: feiras comunitárias','outros',espetaculo$area_de_atuacao)                  
espetaculo$area_de_atuacao = gsub('outros: literatura','outros',espetaculo$area_de_atuacao)                    
espetaculo$area_de_atuacao = gsub('outros: produção','outros',espetaculo$area_de_atuacao) 
espetaculo$area_de_atuacao = gsub('outros: produção, gestão cultural','outros',espetaculo$area_de_atuacao) 
espetaculo$area_de_atuacao = gsub('outros, gestão cultural','outros',espetaculo$area_de_atuacao) 
table(espetaculo$area_de_atuacao)

d = data.frame(table(espetaculo$area_de_atuacao))
d$percentual = (d$Freq/sum(d$Freq))*100

d$percentual = round(d$percentual,2)
d

library(ggplot2)
ggplot(d) +
  aes(x = Var1, y = percentual) +
  geom_col(fill='#b02c57') +
  labs(x = "Área de Atuação", y = "Percentual", 
       subtitle = "Espetáculo: Qual a sua principal área de atuação no campo da cultura?", 
       caption = "Fonte: ECOA", fill = "Categoria") +
  coord_flip()+
  theme_minimal() +
  ecoar_theme2()

ggsave("graficos_v3/entrega1_espetaculo_area_atuacao.png",width = 20, height = 14, units = "cm")

remove(d) 













,