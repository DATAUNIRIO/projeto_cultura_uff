

# https://www.sigeo.niteroi.rj.gov.br/datasets/geoniteroi::limite-de-regi%C3%B5es-mapa-1/about


source('/home/steven/Documentos/GitHub/projeto_cultura_uff/code8/carregar_as_bases.R')
remove(perguntas,ecoar_theme,ecoar_theme2)




library(geojsonio)
regioes_niteroi <- geojson_read("/home/steven/Documentos/GitHub/projeto_cultura_uff/mapas/Limite_de_Regioes.geojson",  what = "sp")

library(sp)
par(mar=c(0,0,0,0))
#plot(regioes_niteroi)

regioes_niteroi@data

#-----------------------------------------------------------------------------------------------------
# Banco de dados
#-----------------------------------------------------------------------------------------------------

AP_av = data.frame(table(visuais$regiao))
AP_espetaculo = data.frame(table(espetaculo$regiao))

AP_av
AP_espetaculo

AP_av$Var1 = as.character(AP_av$Var1)
AP_espetaculo$Var1 = as.character(AP_espetaculo$Var1)

library(dplyr)
AP_av = AP_av %>% rename(tx_nome = Var1) 
AP_av = AP_av %>% rename(artesvisuais = Freq) 

AP_espetaculo = AP_espetaculo %>% rename(tx_nome = Var1) 
AP_espetaculo = AP_espetaculo %>% rename(espetaculo = Freq) 

AP_av$tx_nome = toupper(AP_av$tx_nome)
AP_espetaculo$tx_nome = toupper(AP_espetaculo$tx_nome)

#regioes_niteroi@data = regioes_niteroi@data %>% left_join(AP_av)
#regioes_niteroi@data = regioes_niteroi@data %>% left_join(AP_espetaculo)

regioes_niteroi <- merge(regioes_niteroi,AP_av, by="tx_nome")
regioes_niteroi <- merge(regioes_niteroi,AP_espetaculo, by="tx_nome")
regioes_niteroi@data


library(leaflet)
bins <- c(0, 5, 7 , 8, 10, Inf)
pal <- colorBin("YlOrRd", domain = regioes_niteroi$artesvisuais, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g Artes Visuais ",
  #regioes_niteroi$regiao, 
  regioes_niteroi$artesvisuais) %>% lapply(htmltools::HTML)

leaflet(regioes_niteroi) %>%
  addTiles() |>
  addProviderTiles("Esri.WorldImagery") %>%
  #setView(-96, 37.8, 4) %>%
  addPolygons(
    fillColor = ~pal(artesvisuais),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    #label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~artesvisuais, opacity = 0.7, title = NULL,
            position = "bottomright")

#--------------------------------------------------------------------------------------

regioes_niteroi@data

bins2 <- c(0, 6,24,25, 35,45, Inf)
pal2 <- colorBin("YlOrRd", domain = regioes_niteroi$espetaculo, bins = bins2)

labels2 <- sprintf(
  "<strong>%s</strong><br/>%g Espetaculo ",
  regioes_niteroi$regiao, regioes_niteroi$espetaculo) %>% lapply(htmltools::HTML)

leaflet(regioes_niteroi) %>%
  addTiles() |>
  addProviderTiles("Esri.WorldImagery") %>%
  #setView(-96, 37.8, 4) %>%
  addPolygons(
    fillColor = ~pal(espetaculo),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    #label = labels2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal2, values = ~espetaculo, opacity = 0.7, title = NULL,
            position = "bottomright")


#--------------------------------------------------------------------------------------------


mapa = leaflet(regioes_niteroi) %>%
  addTiles() |>
  addProviderTiles("Esri.WorldImagery") %>%
  #setView(-96, 37.8, 4) %>%
  addPolygons(
    fillColor = ~pal(artesvisuais),
    group="Artes Visuais",
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    #label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  
  addPolygons(
    fillColor = ~pal(espetaculo),
    group="Espetáculo",
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    #label = labels2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLayersControl(
    baseGroups = c("Artes Visuais", "Espetáculo")) %>%
  addLegend(pal = pal2, values = ~espetaculo, opacity = 0.7, title = NULL,
            position = "bottomright")


mapa  

library(htmlwidgets)
saveWidget(mapa, file="C:/Users/08451589707/Documents/GitHub/projeto_cultura_uff/mapa3/mapa_ap.html")

