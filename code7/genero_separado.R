
genero_av = tabyl(visuais$como_voce_se_identifica_em_relacao_ao_genero)
genero_esp = tabyl(espetaculo$como_voce_se_identifica_em_relacao_ao_genero)

genero_av = genero_av[1:3,]
genero_av = genero_av %>% select(-percent)
genero_av = genero_av %>% rename(percent=valid_percent)
colnames(genero_av)[1] = 'genero'
genero_av$tipo= rep("artes visuais",3)
genero_esp$tipo= rep("espetáculo",3)
colnames(genero_esp)[1] = 'genero'

genero_av$percent = genero_av$percent*100
genero_av$percent = round(genero_av$percent,2)

genero_esp$percent = genero_esp$percent*100
genero_esp$percent = round(genero_esp$percent,2)


library(ggplot2)

ggplot(genero_av, aes(percent, forcats::fct_rev(genero))) +
    geom_col(fill="#b02c57") +
    geom_text(aes(label = percent),nudge_x = -5.5) +
    scale_x_continuous(limits = c(0, 75)) +
    labs(y='Gênero',x='Percentual',subtitle = 'Artes Visuais: Como você se identifica \nem relação ao gênero?')+
    theme_minimal()+
    ecoar_theme()
  
ggsave("graficos_v7/entrega1_genero_AV.png",width = 20, height = 14, units = "cm")
  
ggplot(genero_esp, aes(percent, forcats::fct_rev(genero))) +
    geom_col(fill="#327fc7") +
    geom_text(aes(label = percent),nudge_x = -5.5) +
    scale_x_continuous(limits = c(0, 65)) +
    labs(y='Gênero',x='Percentual',subtitle = 'Artes do Espetáculo: Como você se identifica \n em relação ao gênero?')+
    theme_minimal()+
    ecoar_theme()
  
  
ggsave("graficos_v7/entrega1_genero_AE.png",width = 20, height = 14, units = "cm")
  

