
source('/home/steven/Documentos/GitHub/projeto_cultura_uff/code8/carregar_as_bases.R')

View(data.frame(table(visuais$voce_recebeu_e_ou_participou_de_algum_edital_publico_voce_pode_marcar_mais_de_uma_opcao)))

visuais$edital =  visuais$voce_recebeu_e_ou_participou_de_algum_edital_publico_voce_pode_marcar_mais_de_uma_opcao
visuais$edital2 =  visuais$voce_recebeu_e_ou_participou_de_algum_edital_publico_voce_pode_marcar_mais_de_uma_opcao

visuais = visuais %>%
  mutate(
    edital = case_when(
      str_detect(edital,'Sim, mas não fui contemplado/a') ~ "Sim, mas não fui contemplado/a",
      str_detect(edital,'Sim, municipal - Niterói, Sim, mas não fui contemplado/a') ~ "Sim, mas não fui contemplado/a",
      str_detect(edital,'Sim, municipal - outros municípios') ~ NA,
      str_detect(edital,'Sim') ~ "Sim",
      str_detect(edital,'Não') ~ "Não",
      TRUE ~ "TESTE")
  )

#visuais %>% filter(edital=='TESTE') %>% select(voce_recebeu_o_auxilio_emergencial_e_ou_participou_de_algum_edital_publico_voce_pode_marcar_mais_de_uma_opcao_50) %>% table() %>% data.frame() %>% View()  
table(visuais$edital)



visuais = visuais %>%
  mutate(
    edital2 = case_when(
      str_detect(edital2,'Sim, mas não fui contemplado/a') ~ "Sim, mas não fui contemplado/a",
      str_detect(edital2,'Sim, municipal - Niterói, Sim, mas não fui contemplado/a') ~ "Sim, mas não fui contemplado/a",
      str_detect(edital2,'Sim, municipal - outros municípios') ~ NA,
      str_detect(edital2,'Sim, municipal - Niterói, sim e fui contemplada') ~ "Sim, municipal",
      str_detect(edital2,'Não') ~ "Não",
      str_detect(edital2,'Sim, federal, Sim, estadual, Sim, municipal') ~ "Sim, federal, Sim, estadual, Sim, municipal",
      str_detect(edital2,'Sim, estadual, Sim, municipal - Niterói') ~ "Sim, estadual, Sim, municipal",
      str_detect(edital2,'Sim, municipal - Niterói') ~ "Sim, municipal",
      
      TRUE ~ "TESTE")
  )

table(visuais$edital2)
# visuais %>% filter(edital2=='TESTE') %>% select(voce_recebeu_e_ou_participou_de_algum_edital_publico_voce_pode_marcar_mais_de_uma_opcao) %>% table() %>% data.frame() %>% View()  


d = visuais %>% select(edital,como_voce_se_identifica_em_relacao_a_raca)
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
d = d %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')
d = d %>% filter(!is.na(edital))
#d = d %>% filter(renda!='Não sei / Não quero responder')
d = d %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,edital) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d$freq = d$freq*100


g1 = ggplot(d) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = edital) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Participou de Editais?" , 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega5_visuais_edital_publico_M1_cor.png",width = 26, height = 18, units = "cm")
remove(d) 

#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------

q = visuais %>% select(edital2,como_voce_se_identifica_em_relacao_a_raca)
q = q %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Outros')
q = q %>% filter(como_voce_se_identifica_em_relacao_a_raca!='Não sei / Não quero responder')
q = q %>% filter(!is.na(edital2))
#d = d %>% filter(renda!='Não sei / Não quero responder')
q = q %>%   group_by(como_voce_se_identifica_em_relacao_a_raca,edital2) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

q$freq = q$freq*100


g3 = ggplot(q) +
  aes(x = como_voce_se_identifica_em_relacao_a_raca, y = freq, fill = edital2) +
  geom_col() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(10,"Spectral"),name = "") +
  labs(x = "", y = "Percentual", subtitle = "Artes Visuais: Participou de Editais?" , 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g3
ggsave("graficos_v8/entrega5_visuais_edital_publico_M2_cor.png",width = 26, height = 18, units = "cm")
remove(q) 
