
#display factor levels for region
levels(espetaculo$ha_quanto_tempo_voce_atua_nessa_area)

tempo = tabyl(espetaculo$ha_quanto_tempo_voce_atua_nessa_area)

colnames(tempo)[1] = 'Tempo'
tempo$percent = tempo$percent*100
tempo$percent = round(tempo$percent,2)

library(ggplot2)

ggplot(tempo, aes(percent, forcats::fct_rev(Tempo),fill="#b02c57")) +
  geom_col() +
  geom_text(aes(label = percent),nudge_x = 4) +
  labs(y='Tempo de atuação',x='Percentual',subtitle = 'Há quanto tempo você atua nessa área?')+
  theme_minimal()+
  ecoar_theme()+
  theme(legend.position = "none")

ggsave("graficos_v2/entrega1_tempo.png",width = 20, height = 14, units = "cm")
