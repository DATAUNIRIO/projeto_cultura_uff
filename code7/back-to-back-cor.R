

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


# Combine data.frames
all_df <- rbind(cbind(cor_av, facet = "Artes Visuais"),
                cbind(cor_esp, facet = "Espetáculo"))
library(ggplot2)
library(ggh4x)
ggplot(all_df, aes(percent, forcats::fct_rev(cor),fill=facet)) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  geom_text(aes(label = percent),nudge_x = 8.5,color="#b02c57",fontface="bold") +
  facet_wrap(~ facet, scales = "free_x") +
  facetted_pos_scales(x = list(
    scale_x_reverse(limits = c(70, 0)),
    scale_x_continuous(limits = c(0, 70))
  ))+
  labs(y='Cor/Raça',x='Percentual',subtitle = 'Como você se identifica em relação à Raça?')+
  theme_minimal()+
  ecoar_theme()


ggsave("graficos_v7/entrega1_cor_dois.png",width = 20, height = 14, units = "cm")
