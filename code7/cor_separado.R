

cor_av = tabyl(visuais$como_voce_se_identifica_em_relacao_a_raca)
cor_esp = tabyl(espetaculo$como_voce_se_identifica_em_relacao_a_raca)

colnames(cor_av)[1] = 'cor'
cor_av$tipo= rep("artes visuais",6)
cor_esp$tipo= rep("espetáculo",6)
colnames(cor_esp)[1] = 'cor'

cor_av$percent = cor_av$percent*100
cor_av$percent = round(cor_av$percent,2)

cor_esp$percent = cor_esp$percent*100
cor_esp$percent = round(cor_esp$percent,2)

library(ggplot2)
ggplot(cor_av, aes(percent, forcats::fct_rev(cor))) +
  geom_col(fill="#b02c57") +
  geom_text(aes(label = percent),nudge_x = -5.5) +
  scale_x_continuous(limits = c(0, 55)) +
  labs(y='Cor/Raça',x='Percentual',subtitle = 'Artes Visuais: Como você se identifica \n em relação à Raça?')+
  theme_minimal()+
  ecoar_theme()

ggsave("graficos_v7/entrega1_cor_AV.png",width = 20, height = 14, units = "cm")

ggplot(cor_esp, aes(percent, forcats::fct_rev(cor))) +
  geom_col(fill="#327fc7") +
  geom_text(aes(label = percent),nudge_x = -5.5) +
  scale_x_continuous(limits = c(0, 55)) +
  labs(y='Cor/Raça',x='Percentual',subtitle = 'Artes Espetáculo: Como você se identifica \n em relação à Raça?')+
  theme_minimal()+
  ecoar_theme()


ggsave("graficos_v7/entrega1_cor_AE.png",width = 20, height = 14, units = "cm")
