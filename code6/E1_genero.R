
genero_av = tabyl(visuais$como_voce_se_identifica_em_relacao_ao_genero)
genero_esp = tabyl(espetaculo$como_voce_se_identifica_em_relacao_ao_genero)

genero_av = genero_av[1:3,]
genero_av = genero_av %>% select(-percent)
genero_av = genero_av %>% rename(percent=valid_percent)
colnames(genero_av)[1] = 'genero'
genero_av$tipo= rep("artes visuais",3)
genero_esp$tipo= rep("espetáculo",3)
colnames(genero_esp)[1] = 'genero'
# Combine data.frames
all_df <- rbind(cbind(genero_av, facet = "Artes Visuais"),
                cbind(genero_esp, facet = "Espetáculo"))

all_df$percent = all_df$percent*100
all_df$percent = round(all_df$percent,2)

library(ggplot2)
library(ggh4x)
ggplot(all_df, aes(percent, forcats::fct_rev(genero),fill=facet)) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  geom_text(aes(label = percent),nudge_x = 10,color="#b02c57") +
  facet_wrap(~ facet, scales = "free_x") +
  facetted_pos_scales(x = list(
    scale_x_reverse(limits = c(90, 0)),
    scale_x_continuous(limits = c(0, 90))
  ))+
  labs(y='Gênero',x='Percentual',subtitle = 'Como você se identifica em relação ao gênero?')+
  theme_minimal()+
  ecoar_theme()

ggsave("graficos_v6/entrega1_genero.png",width = 20, height = 14, units = "cm")


remove(all_df,genero_av,genero_esp)
