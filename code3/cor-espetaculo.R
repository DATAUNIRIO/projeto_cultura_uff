

cor_av = tabyl(visuais$como_voce_se_identifica_em_relacao_a_raca)
cor_esp = tabyl(espetaculo$como_voce_se_identifica_em_relacao_a_raca)

colnames(cor_av)[1] = 'cor'
cor_av$tipo= rep("artes visuais",5)
cor_esp$tipo= rep("espetáculo",6)
colnames(cor_esp)[1] = 'cor'
# Combine data.frames
all_df <- rbind(cbind(cor_av, facet = "Artes Visuais"),
                cbind(cor_esp, facet = "Espetáculo"))

cor_esp$percent = cor_esp$percent*100
cor_esp$percent = round(cor_esp$percent,2)

#re-order factor levels 
cor_esp$cor <- factor(cor_esp$cor, levels=c('Branco(a)','Preto(a)/Pardo(a)','Amarelo(a)','Indígena','Outros','Não sei / Não quero responder'))

library(ggplot2)
theme_set(theme_minimal())
theme_set(ecoar_theme())

ggplot(cor_esp, aes(percent, forcats::fct_rev(cor),fill="#b02c57")) +
  geom_col() +
  geom_text(aes(label = percent),nudge_x = 4) +
  labs(y='Cor/Raça',x='Percentual',subtitle = 'Como você se identifica em relação à Raça?')+
  theme_minimal()+
  ecoar_theme()+
  theme(legend.position = "none")

ggsave("graficos_v2/entrega1_cor.png",width = 20, height = 14, units = "cm")
