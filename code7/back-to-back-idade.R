
table(espetaculo$faixa_idade)
table(visuais$faixa_idade)
#-----------------------------------------------------------------------------

idade_av = tabyl(visuais$faixa_idade)
idade_esp = tabyl(espetaculo$faixa_idade)

colnames(idade_av)[1] = 'idade'
idade_av$tipo= rep("artes visuais",5)
idade_esp$tipo= rep("espetáculo",5)
colnames(idade_esp)[1] = 'idade'

# Combine data.frames
all_df <- rbind(cbind(idade_av, facet = "Artes Visuais"),
                cbind(idade_esp, facet = "Espetáculo"))

all_df$percent = all_df$percent*100
all_df$percent = round(all_df$percent,2)

all_df

library(ggplot2)
library(ggh4x)
ggplot(all_df, aes(percent, idade,fill=facet)) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  geom_text(aes(label = percent),nudge_x = 5,color="#b02c57",fontface="bold") +
  facet_wrap(~ facet, scales = "free_x") +
  facetted_pos_scales(x = list(
    scale_x_reverse(limits = c(60, 0)),
    scale_x_continuous(limits = c(0, 60))
  ))+
  labs(y='Idade',x='Percentual',subtitle = 'Em qual faixa etária você se encontra?')+
  theme_minimal()+
  ecoar_theme()

ggsave("graficos_v7/entrega1_idade_dois.png",width = 20, height = 14, units = "cm")

remove(all_df,idade_av,idade_esp)
