library(leaflet.providers)
str(providers_default()$providers_details, max.level = 2)


library(geojsonio)
bairros_niteroi <- geojson_read("C:/Users/08451589707/Documents/GitHub/projeto_cultura_uff/mapas/Limite_de_Bairros.geojson",  what = "sp")
library(sp)
par(mar=c(0,0,0,0))
plot(bairros_niteroi)

    
library(leaflet)
bins <- c(0, 1029667 , 2252844 , 2577625 , 2726640 , 3026640 , 5026640, Inf)
pal <- colorBin("YlOrRd", domain = bairros_niteroi$Shape__Area, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  bairros_niteroi$tx_nome, bairros_niteroi$Shape__Area
) %>% lapply(htmltools::HTML)

leaflet(bairros_niteroi) %>%
  addTiles() |>
  addProviderTiles("Esri.WorldImagery") %>%
  #setView(-96, 37.8, 4) %>%
  addPolygons(
    fillColor = ~pal(Shape__Area),
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
  addLegend(pal = pal, values = ~Shape__Area, opacity = 0.7, title = NULL,
            position = "bottomright")
