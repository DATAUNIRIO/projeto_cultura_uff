

table(espetaculo$em_qual_faixa_etaria_voce_se_encontra)
table(visuais$em_qual_faixa_etaria_voce_se_encontra)

library(dplyr)
espetaculo = espetaculo %>%
  mutate(
    faixa_idade = case_when(
      em_qual_faixa_etaria_voce_se_encontra=='18 a 21 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='22 a 24 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='25 a 29 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='30 a 34 anos' ~ "30 a 39 anos",
      em_qual_faixa_etaria_voce_se_encontra=='35 a 39 anos' ~ "30 a 39 anos",
      em_qual_faixa_etaria_voce_se_encontra=='40 a 44 anos' ~ "40 a 49 anos",
      em_qual_faixa_etaria_voce_se_encontra=='45 a 49 anos' ~ "40 a 49 anos",
      em_qual_faixa_etaria_voce_se_encontra=='50 a 54 anos' ~ "50 a 60 anos",
      em_qual_faixa_etaria_voce_se_encontra=='55 a 60 anos' ~ "50 a 60 anos",
      em_qual_faixa_etaria_voce_se_encontra=='Mais de 60' ~ "Mais de 60 anos",
    TRUE                      ~ "outros"))

table(espetaculo$faixa_idade)
#espetaculo %>% filter(faixa_idade=='outros') %>% select(em_qual_faixa_etaria_voce_se_encontra) %>% print()

visuais = espetaculo %>%
  mutate(
    faixa_idade = case_when(
      em_qual_faixa_etaria_voce_se_encontra=='18 a 21 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='22 a 24 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='25 a 29 anos' ~ "18 a 29 anos",
      em_qual_faixa_etaria_voce_se_encontra=='30 a 34 anos' ~ "30 a 39 anos",
      em_qual_faixa_etaria_voce_se_encontra=='35 a 39 anos' ~ "30 a 39 anos",
      em_qual_faixa_etaria_voce_se_encontra=='40 a 44 anos' ~ "40 a 49 anos",
      em_qual_faixa_etaria_voce_se_encontra=='45 a 49 anos' ~ "40 a 49 anos",
      em_qual_faixa_etaria_voce_se_encontra=='50 a 54 anos' ~ "50 a 60 anos",
      em_qual_faixa_etaria_voce_se_encontra=='55 a 60 anos' ~ "50 a 60 anos",
      em_qual_faixa_etaria_voce_se_encontra=='Mais de 60' ~ "Mais de 60 anos",
      TRUE                      ~ "outros"))

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
library(ggplot2)
library(ggh4x)
ggplot(all_df, aes(percent, idade,fill=facet)) +
  geom_col() +
  scale_fill_manual(values = c("#b02c57","#327fc7"),name = "Categoria") +
  geom_text(aes(label = percent),nudge_x = 4) +
  facet_wrap(~ facet, scales = "free_x") +
  facetted_pos_scales(x = list(
    scale_x_reverse(limits = c(.3, 0)),
    scale_x_continuous(limits = c(0, 0.3))
  ))+
  labs(y='Idade',x='Percentual',subtitle = 'Em qual faixa etária você se encontra?')+
  theme_minimal()+
  ecoar_theme()

ggsave("graficos/entrega1_idade.png",width = 20, height = 14, units = "cm")
