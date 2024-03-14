
genero_esp = tabyl(espetaculo$como_voce_se_identifica_em_relacao_ao_genero)
colnames(genero_esp)[1] = 'genero'
genero_esp$percent = genero_esp$percent*100
genero_esp$percent = round(genero_esp$percent,2)

library(ggplot2)
ggplot(genero_esp, aes(percent, forcats::fct_rev(genero),fill="#b02c57")) +
  geom_col() +
  geom_text(aes(label = percent),nudge_x = 4) +
  labs(y='Gênero',x='Percentual',subtitle = 'Como você se identifica em relação ao gênero?')+
  theme_minimal()+
  ecoar_theme()+
  theme(legend.position = "none")


ggsave("graficos_v2/entrega1_genero.png",width = 20, height = 14, units = "cm")


