
table(visuais$ha_quanto_tempo_voce_atua_nessa_area)
table(espetaculo$ha_quanto_tempo_voce_atua_nessa_area)

#-----------------------------------------------------------------------------

#re-order factor levels for region
espetaculo$ha_quanto_tempo_voce_atua_nessa_area <- factor(espetaculo$ha_quanto_tempo_voce_atua_nessa_area, levels=c('Até 5 anos', 'Mais de 5 até 10 anos', 'Mais de 10 até 15 anos', 'Mais de 15 anos'))
visuais$ha_quanto_tempo_voce_atua_nessa_area <- factor(visuais$ha_quanto_tempo_voce_atua_nessa_area, levels=c('Até 5 anos', 'Mais de 5 até 10 anos', 'Mais de 10 até 15 anos', 'Mais de 15 anos'))

#display factor levels for region
levels(espetaculo$ha_quanto_tempo_voce_atua_nessa_area)

tempo_av = tabyl(visuais$ha_quanto_tempo_voce_atua_nessa_area)
tempo_esp = tabyl(espetaculo$ha_quanto_tempo_voce_atua_nessa_area)

colnames(tempo_av)[1] = 'tempo'
tempo_av$tipo= rep("artes visuais",4)
tempo_esp$tipo= rep("espetáculo",4)
colnames(tempo_esp)[1] = 'tempo'

tempo_av$percent = tempo_av$percent*100
tempo_av$percent = round(tempo_av$percent,2)

tempo_esp$percent = tempo_esp$percent*100
tempo_esp$percent = round(tempo_esp$percent,2)

library(ggplot2)
    ggplot(tempo_av, aes(percent, forcats::fct_rev(tempo))) +
    geom_col(fill="#b02c57") +
    geom_text(aes(label = percent),nudge_x = -5.5) +
    scale_x_continuous(limits = c(0, 75)) +
    labs(y='Tempo de atuação',x='Percentual',subtitle = 'Artes Visuais: Há quanto tempo você atua nessa área?')+
    theme_minimal()+
    ecoar_theme()
  
ggsave("graficos_v7/entrega1_tempo_AV.png",width = 20, height = 14, units = "cm")
  
  ggplot(tempo_esp, aes(percent, forcats::fct_rev(tempo))) +
    geom_col(fill="#327fc7") +
    geom_text(aes(label = percent),nudge_x = -5.5) +
    scale_x_continuous(limits = c(0, 65)) +
    labs(y='Tempo de atuação',x='Percentual',subtitle = 'Artes do Espetáculo: Há quanto tempo você atua nessa área?')+
    theme_minimal()+
    ecoar_theme()
    
ggsave("graficos_v7/entrega1_tempo_AE.png",width = 20, height = 14, units = "cm")
  
remove(tempo_av,tempo_esp)
