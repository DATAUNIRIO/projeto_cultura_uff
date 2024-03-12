
genero_av = tabyl(visuais$como_voce_se_identifica_em_relacao_ao_genero)
genero_esp = tabyl(espetaculo$como_voce_se_identifica_em_relacao_ao_genero)

genero_av = genero_av[1:3,]
genero_av = genero_av %>% select(-percent)
genero_av = genero_av %>% rename(percent=valid_percent)
colnames(genero_av)[1] = 'genero'
genero_av$tipo= rep("artes visuais",3)
genero_esp$tipo= rep("espetáculo",3)
colnames(genero_esp)[1] = 'genero'
# Combine data.frames
all_df <- rbind(cbind(genero_av, facet = "Artes Visuais"),
                cbind(genero_esp, facet = "Espetáculo"))
library(ggplot2)
library(ggh4x)
ggplot(all_df, aes(percent, forcats::fct_rev(genero),fill=facet)) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  geom_text(aes(label = percent),nudge_x = 4) +
  facet_wrap(~ facet, scales = "free_x") +
  facetted_pos_scales(x = list(
    scale_x_reverse(limits = c(.75, 0)),
    scale_x_continuous(limits = c(0, 0.75))
  ))+
  labs(y='Gênero',x='Percentual',subtitle = 'Como você se identifica em relação ao gênero?')+
  theme_minimal()+
  ecoar_theme()

ggsave("graficos/entrega1_genero.png",width = 20, height = 14, units = "cm")

#   theme(
# plot.background = element_rect(fill = "#b8cfc6"),    
# plot.margin = unit(c(1, 1, 1, 1), "cm"),
# plot.title = element_text(family = "Broadway",size = 28,color = "#dedfe0"),
# plot.subtitle = element_text(family = "Broadway",# size = 15,
#                              color = "#b02c57"),
# strip.text = element_text(family = "Satisfy",size = 13,color = "#b02c57"),
# strip.background = element_rect(fill = "#85a79c", color = "#b02c57"),
# panel.background = element_rect(fill = "#85a79c", color = "#b02c57"),
# axis.title = element_text(family = "Roboto",
# color = "#b02c57"),
# plot.caption = element_text(family = "Roboto",size = 8,
#                             color = "#b02c57",margin = margin(t = 15)),
# axis.text = element_text(family = "Roboto", color = "#b02c57"),
# axis.title.x = element_text(margin = margin(t = 15), hjust = 1,size = 15, color = "#c6ced6"),
# axis.title.y = element_text(margin = margin(r = 15), hjust = 1),
# axis.ticks = element_blank(),
# panel.grid = element_line(color = "#b02c57", linetype = "dashed"),
# panel.grid.major.x = element_blank(),
# panel.grid.minor.x = element_blank(),
# #panel.grid.major.y = element_line(color = "#364049")#,
# #panel.grid.minor.y = element_line(color = "#364049")#,c("Como você se identifica em relação ao gênero? (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Como você se identifica em relação à raça?", "Você possui alguma deficiência?", "Em qual faixa etária você se encontra?", "Qual seu grau de escolaridade?", "Qual a média da sua renda individual mensal levando em conta os salários anteriores a pandemia (considerando o salário mínimo atual)?", "Em qual cidade você nasceu? (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Você é morador e/ou trabalha em Niterói?", "Qual seu bairro?", "Dentro da área cultural", "em qual categoria seu trabalho se enquadra? (Você pode marcar mais de uma opção)", "Dentro da área cultural", "em qual categoria seu trabalho se enquadra? (Você pode marcar mais de uma opção)", "Qual a sua principal área de atuação no campo da cultura?", "Qual a sua principal área de atuação no campo da cultura?", "Você pode especificar sua área", "por favor? (Ex: música erudita) (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Qual é sua ocupação dentro desta área? (Ex: baterista) (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Qual é sua ocupação dentro desta área? (Ex: baterista) (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Há quanto tempo você atua nessa área?", "Faz parte de algum grupo artístico?", "Qual o nome do grupo artístico (coletivos", "agremiações", "trupes", "blocos", "companhias e etc) que você participa? (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Há quanto tempo esse grupo existe?", "Há quanto tempo você participa desse grupo?", "Com quais linguagens esse grupo trabalha? (Ex: música", "dança) (Preencha em letra maiúscula", "sem abreviações ou acentos/cedilhas)", "Há financiamento para a existência do grupo?", "Qual foi a sua principal motivação para inserção profissional na área cultural?", "Qual é a sua carga de trabalho semanal na área cultural? (Considerando todo o processo de trabalho: pré-produção", "produção e pós-produção)", "Como se dá o seu vínculo trabalhista na área cultural? (Você pode marcar mais de uma opção)", "Como se dá o seu vínculo trabalhista na área cultural? (Você pode marcar mais de uma opção)", "Todos os seus trabalhos na área cultural se firmam a partir de contrato?", "Você já teve contratos de trabalho na área cultural desrespeitados?", "Você trabalha exclusivamente na área cultural?", "Caso não trabalhe exclusivamente na área cultural", "qual é a sua outra área de ocupação? (Você pode marcar mais de uma opção)", "Caso não trabalhe exclusivamente na área cultural", "qual é a sua outra área de ocupação? (Você pode marcar mais de uma opção)", "Qual é a sua carga de trabalho semanal em outra área que não a da cultura?", "Na sua outra área de ocupação como se dá o seu vínculo trabalhista?", "Pensando em sua aposentadoria", "você contribui para o INSS?", "Você contribui para a previdência privada ou complementar?", "Qual (ou quais) desses benefícios você recebe? (Você pode marcar mais de uma opção)", "Você possui plano de saúde?", "Você participa de alguma organização de classe na área da cultura?", "Segundo a sua experiência", "como você avalia a média das instalações físicas na área cultural da cidade de Niterói (espaços culturais", "museus", "casas de espetáculo", "espaços de apresentação teatros", "etc)?", "As suas relações de trabalho foram afetadas durante a pandemia?", "A pandemia afetou a sua carga horária dedicada ao trabalho na área da cultura?", "A pandemia afetou sua vida financeira?  (Você pode marcar mais de uma)", "Em algum momento da pandemia foi necessário complementar sua renda?", "Se teve necessidade de complementar renda", "como fez/tem feito? (Você pode marcar mais de uma)", "Você recebeu o Auxílio Emergencial e/ou participou de algum edital público? (Você pode marcar mais de uma opção)", "Você recebeu o Auxílio Emergencial e/ou participou de algum edital público? (Você pode marcar mais de uma opção)", "Considerando o uso das tecnologias e redes digitais", "quais foram os impactos no seu fazer cultural durante a pandemia do COVID-19 (uso das redes digitais para divulgação do trabalho", "para oferta de serviço e/ou busca de oportunidades de emprego ou uso das plataformas digitais para disponibilização do trabalho", "por exemplo)? (Você pode marcar mais de uma opção)", "O que você apontaria como maior dificuldade em seu trabalho na área da cultura?", "O que você apontaria como o maior potencial em seu trabalho na área da cultura?")
# 
# #panel.background = element_rect(fill = "#1e242a", color = "#22292F")
# #panel.spacing = unit(3, "lines"),  ) 
# ) 
# 
# 
# 
# 
# 
