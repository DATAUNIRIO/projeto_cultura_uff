

library(geojsonio)
bairros_niteroi <- geojson_read("C:/Users/Meu Computador/Documents/GitHub/projeto_cultura_uff/mapas/Limite_de_Bairros.geojson",  what = "sp")

bairros_niteroi2 = bairros_niteroi@data
names(bairros_niteroi)

library(readxl)
library(janitor)
AP <- read_excel("~/GitHub/projeto_cultura_uff/mapas/AP.xlsx") %>% clean_names()
names(AP)


library(dplyr)
AP = AP %>% rename(tx_nome=bairro)

bairros_niteroi2 = bairros_niteroi2 %>% left_join(AP)


bairros_niteroi = bairros_niteroi %>% left_join(bairros_niteroi2)

bairros_niteroi@data = bairros_niteroi@data %>% left_join(bairros_niteroi2)

remove(AP, bairros_niteroi2)

library(sp)
par(mar=c(0,0,0,0))
plot(bairros_niteroi)

#-----------------------------------------------------------------------------------------------------

data.frame(table(visuais$regiao))
data.frame(table(espetaculo$regiao))


AP_av = data.frame(table(visuais$regiao,visuais$bairro))
AP_espetaculo = data.frame(table(espetaculo$regiao,espetaculo$bairro))

sum(AP_espetaculo$Freq)


AP_av$Var1 = as.character(AP_av$Var1)
AP_espetaculo$Var1 = as.character(AP_espetaculo$Var1)
AP_av$Var2 = as.character(AP_av$Var2)
AP_espetaculo$Var2 = as.character(AP_espetaculo$Var2)

library(dplyr)

AP_av = AP_av %>% rename(tx_nome = Var2) 
AP_av = AP_av %>% rename(regiao = Var1) 
AP_av = AP_av %>% rename(artesvisuais = Freq) 

AP_av = AP_av %>% filter(tx_nome!='Não moro em Niterói')

AP_espetaculo = AP_espetaculo %>% rename(tx_nome = Var2) 
AP_espetaculo = AP_espetaculo %>% rename(regiao = Var1) 
AP_espetaculo = AP_espetaculo %>% rename(espetaculo = Freq) 
AP_espetaculo = AP_espetaculo %>% filter(tx_nome!='Não moro em Niterói')

bairros_niteroi@data = bairros_niteroi@data %>% left_join(AP_av)
bairros_niteroi@data = bairros_niteroi@data %>% left_join(AP_espetaculo)

#library(sf)
#st_write(bairros_niteroi, "bairros_niteroi.shp"))


summary(bairros_niteroi@data$artesvisuais)
summary(bairros_niteroi@data$espetaculo)


library(leaflet)
bins <- c(0, 2 , 5, 10, Inf)
pal <- colorBin("YlOrRd", domain = bairros_niteroi$artesvisuais, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g Artes Visuais ",
  bairros_niteroi$regiao, bairros_niteroi$artesvisuais) %>% lapply(htmltools::HTML)

leaflet(bairros_niteroi) %>%
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
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~artesvisuais, opacity = 0.7, title = NULL,
            position = "bottomright")

#--------------------------------------------------------------------------------------

bins2 <- c(0, 2 , 5, 10, Inf)
pal2 <- colorBin("YlOrRd", domain = bairros_niteroi$espetaculo, bins = bins2)

labels2 <- sprintf(
  "<strong>%s</strong><br/>%g Espetaculo ",
  bairros_niteroi$regiao, bairros_niteroi$espetaculo) %>% lapply(htmltools::HTML)

leaflet(bairros_niteroi) %>%
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
    label = labels2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal2, values = ~espetaculo, opacity = 0.7, title = NULL,
            position = "bottomright")


#--------------------------------------------------------------------------------------------


mapa = leaflet(bairros_niteroi) %>%
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
    label = labels,
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
    label = labels2,
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
