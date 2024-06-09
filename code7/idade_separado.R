
table(espetaculo$faixa_idade)
table(visuais$faixa_idade)
#-----------------------------------------------------------------------------

idade_av = tabyl(visuais$faixa_idade)
idade_esp = tabyl(espetaculo$faixa_idade)

colnames(idade_av)[1] = 'idade'
idade_av$tipo= rep("artes visuais",5)
idade_esp$tipo= rep("espetáculo",5)
colnames(idade_esp)[1] = 'idade'


idade_av$percent = idade_av$percent*100
idade_av$percent = round(idade_av$percent,2)

idade_esp$percent = idade_esp$percent*100
idade_esp$percent = round(idade_esp$percent,2)

library(ggplot2)
ggplot(idade_av, aes(percent, forcats::fct_rev(idade))) +
    geom_col(fill="#b02c57") +
    geom_text(aes(label = percent),nudge_x = -5.5) +
    scale_x_continuous(limits = c(0, 75)) +
    labs(y='Idade',x='Percentual',subtitle = 'Artes Visuais: Em qual faixa etária você se encontra?')+
    theme_minimal()+
    ecoar_theme()
  
ggsave("graficos_v7/entrega1_idade_AV.png",width = 20, height = 14, units = "cm")
  
ggplot(idade_esp, aes(percent, forcats::fct_rev(idade))) +
    geom_col(fill="#327fc7") +
    geom_text(aes(label = percent),nudge_x = -5.5) +
    scale_x_continuous(limits = c(0, 65)) +
    labs(y='Idade',x='Percentual',subtitle = 'Artes do Espetáculo: Em qual faixa etária você se encontra?')+
    theme_minimal()+
    ecoar_theme()
  
ggsave("graficos_v7/entrega1_idade_AE.png",width = 20, height = 14, units = "cm")

    
        
    
remove(all_df,idade_av,idade_esp)
