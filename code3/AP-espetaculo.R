
AP = tabyl(espetaculo$regiao)

colnames(AP)[1] = 'ap'

AP$percent = AP$percent*100
AP$percent = round(AP$percent,2)


library(ggplot2)
theme_set(theme_minimal())
theme_set(ecoar_theme())

ggplot(AP, aes(percent, forcats::fct_rev(ap),fill="#b02c57")) +
  geom_col() +
  geom_text(aes(label = percent),nudge_x = 4) +
  labs(y='Cor/Raça',x='Percentual',subtitle = 'Como você se identifica em relação à Raça?')+
  theme_minimal()+
  ecoar_theme()+
  theme(legend.position = "none")

ggsave("graficos_v2/entrega1_cor.png",width = 20, height = 14, units = "cm")
