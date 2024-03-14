
library(dplyr)


table(visuais$faixa_idade)
#-----------------------------------------------------------------------------

idade_esp = tabyl(espetaculo$faixa_idade)

colnames(idade_esp)[1] = 'idade'
idade_esp$percent = idade_esp$percent*100
idade_esp$percent = round(idade_esp$percent,2)
sum(idade_esp$percent)

library(ggplot2)
ggplot(idade_esp, aes(percent, forcats::fct_rev(idade),fill="#b02c57")) +
  geom_col() +
  geom_text(aes(label = percent),nudge_x = 2.5) +
  labs(y='Idade',x='Percentual',subtitle = 'Em qual faixa etária você se encontra?')+
  theme_minimal()+
  ecoar_theme()+
  theme(legend.position = "none")

ggsave("graficos_v2/entrega1_idade.png",width = 20, height = 14, units = "cm")
