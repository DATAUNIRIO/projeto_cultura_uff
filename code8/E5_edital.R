
source('/home/steven/Documentos/GitHub/projeto_cultura_uff/code8/carregar_as_bases.R')


#View(data.frame(table(espetaculo$voce_recebeu_o_auxilio_emergencial_e_ou_participou_de_algum_edital_publico_voce_pode_marcar_mais_de_uma_opcao_50)))
#View(data.frame(table(visuais$voce_recebeu_e_ou_participou_de_algum_edital_publico_voce_pode_marcar_mais_de_uma_opcao)))


espetaculo$edital =  espetaculo$voce_recebeu_o_auxilio_emergencial_e_ou_participou_de_algum_edital_publico_voce_pode_marcar_mais_de_uma_opcao_50
espetaculo$edital2 =  espetaculo$voce_recebeu_o_auxilio_emergencial_e_ou_participou_de_algum_edital_publico_voce_pode_marcar_mais_de_uma_opcao_50

espetaculo = espetaculo %>%
  mutate(
    edital = case_when(
      str_detect(edital,'Erika') ~ "Sim",
      str_detect(edital,'Fomento às Artes') ~ "Sim",
      str_detect(edital,'Edital Estadual') ~ "Sim",
      str_detect(edital,'Edital Municipal da Lei Aldir Blanc - Niterói') ~ "Sim",
      str_detect(edital,"Concorri a edital / editais, mas não fui contemplado") ~ "Sim, mas não fui contemplado", 
      str_detect(edital,"Não recebi auxílio, nem participei de nenhum edital público") ~"Não",
      str_detect(edital,"Edital Estadual") ~ "Sim" ,
      str_detect(edital,"Não sei / Não quero responder") ~ NA,
      str_detect(edital,"Solicitei auxílio e não recebi") ~ NA,
                 TRUE ~ "Não")
      )

#espetaculo %>% filter(edital=='other') %>% select(voce_recebeu_o_auxilio_emergencial_e_ou_participou_de_algum_edital_publico_voce_pode_marcar_mais_de_uma_opcao_50) %>% table() %>% data.frame() %>% View()  

table(espetaculo$edital)

espetaculo = espetaculo %>%
  mutate(
    edital2 = case_when(
      str_detect(edital2,'Erika') ~ "Municipal",
      str_detect(edital2,'Ferreira') ~ "Municipal",
      str_detect(edital2,'Municipal') ~ "Municipal",
      str_detect(edital2,'Fomento às Artes') ~ "Municipal",
      str_detect(edital2,'Edital Municipal da Lei Aldir Blanc - Niterói') ~ "Municipal",
      str_detect(edital2,"Concorri a edital / editais, mas não fui contemplado") ~ "Não fui contemplado", 
      str_detect(edital2,"Não recebi auxílio, nem participei de nenhum edital público") ~"Não participou de edital ou não é municipal",
      str_detect(edital2,"Não sei / Não quero responder") ~ NA,
      str_detect(edital2,"Solicitei auxílio e não recebi") ~ NA,
      TRUE ~ "Não participou de edital ou não é municipal")
  )
table(espetaculo$edital2)

#espetaculo %>% filter(edital2=="Não participou de edital ou não é municipal") %>% select(voce_recebeu_o_auxilio_emergencial_e_ou_participou_de_algum_edital_publico_voce_pode_marcar_mais_de_uma_opcao_50) %>% table() %>% data.frame() %>% View()  


d = espetaculo %>% select(edital,como_voce_se_identifica_em_relacao_a_raca)
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
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Participou de Editais?" , 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g1
ggsave("graficos_v8/entrega5_espetaculo_edital_publico_M1_cor.png",width = 26, height = 18, units = "cm")
remove(d) 

#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------

q = espetaculo %>% select(edital2,como_voce_se_identifica_em_relacao_a_raca)
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
  labs(x = "", y = "Percentual", subtitle = "Artes do Espetáculo: Participou de Editais Municipais?" , 
       #caption = "Fonte: ECOA", 
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2,byrow = TRUE))+
  ecoar_theme2()

g3
ggsave("graficos_v8/entrega5_espetaculo_edital_publico_M2_cor.png",width = 26, height = 18, units = "cm")
remove(q) 
