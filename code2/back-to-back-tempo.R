
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
# Combine data.frames
all_df <- rbind(cbind(tempo_av, facet = "Artes Visuais"),
                cbind(tempo_esp, facet = "Espetáculo"))
library(ggplot2)
library(ggh4x)
ggplot(all_df, aes(percent, tempo,fill=facet)) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  geom_text(aes(label = percent),nudge_x = 4) +
  facet_wrap(~ facet, scales = "free_x") +
  facetted_pos_scales(x = list(
    scale_x_reverse(limits = c(.55, 0)),
    scale_x_continuous(limits = c(0, 0.55))
  ))+
  labs(y='Tempo de atuação',x='Percentual',subtitle = 'Há quanto tempo você atua nessa área?')+
  theme_minimal()+
  ecoar_theme()

ggsave("graficos/entrega1_tempo.png",width = 20, height = 14, units = "cm")
