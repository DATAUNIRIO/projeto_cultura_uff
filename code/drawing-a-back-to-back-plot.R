# https://stackoverflow.com/questions/66256292/drawing-a-back-to-back-plot
library(ggplot2)
library(ggh4x)
library(forcats)

table(artes_visuais$id_raca)
artes_visuais$id_raca = forcats::fct_lump_n(artes_visuais$id_raca,n=3,other_level = "Outros(as)")
table(artes_visuais$id_raca)

tabela_av = data.frame(table(artes_visuais$id_raca))
tabela_espetaculo = data.frame(table(espetaculo$id_raca))

tabela_av$tipo= rep("artes visuais",4)
tabela_espetaculo$tipo= rep("espetáculo",4)

# Combine data.frames
all_df <- rbind(cbind(tabela_av, facet = "Artes Visuais"),
                cbind(tabela_espetaculo, facet = "Espetáculo"))

ggplot(all_df, aes(Freq, forcats::fct_rev(Var1),fill=facet)) +
  geom_col() +
  scale_fill_manual(values = c("#E91203","#0651EB"),name = "Categoria") +
  geom_text(aes(label = Freq),nudge_x = 4) +
  facet_wrap(~ facet, scales = "free_x") +
  facetted_pos_scales(x = list(
    scale_x_reverse(limits = c(60, 0)),
    scale_x_continuous(limits = c(0, 60))
  ))+
  labs(y='cor/raça','quantidade')








