library(readxl)
library(tidyverse)
library(dplyr)
library(data.table)
library(knitr)
library(kableExtra)
library(sf)
library(leaflet)
library(openxlsx)
library(tidyr)
library(openxlsx)
library(raster)

library(leaflet)  # libreria para graficar mapas interactivos
library(sf)  # manejo de informacion geografica 
library(viridis)  # paletas de colores
library(RColorBrewer)  # mas paletas de colores
library(dplyr)  # manejo de bases de datos
library(htmlwidgets)
library(ggplot2)
library(htmltools)

rm(list=ls())

distritos <- st_read("indec_codgeo_pba_codgeo_pba_dpto (4).shp")

gral_rodriguez <- filter(distritos, departamen=="General Rodríguez")

obras_gralrodriguez <- read_excel("Obras - General Rodriguez (2).xlsx")

obras_gralrodriguez$Latitud <- as.numeric(obras_gralrodriguez$Latitud)

obras_gralrodriguez$Longitud <- as.numeric(obras_gralrodriguez$Longitud)

inicial <- filter(obras_gralrodriguez, NIVEL=="Inicial")

primario <- filter(obras_gralrodriguez, NIVEL=="Primario")

secundario <- filter(obras_gralrodriguez, NIVEL=="Secundario")


popup_inicial <- paste0("<b>", "Nombre del establecimiento: ", "</b>", as.character(inicial$ESTABLECIMIENTO),
                            "<br>", "<b>","Estado de obra: ", "</b>",
                            as.character(inicial$ESTADO), "<br>", "<b>", "Nivel: ", "</b>", as.character(inicial$NIVEL), "<br>",
                            "<b>","Cantidad de aulas: " ,"</b>",as.character(inicial$Aulas),"<br>","<b>",
                            "Financiamiento: ", "</b>",
                            as.character(inicial$`FUENTE DE FINANCIAMIENTO`))

popup_primario <- paste0("<b>", "Nombre del establecimiento: ", "</b>", as.character(primario$ESTABLECIMIENTO),
                         "<br>", "<b>","Estado de obra: ", "</b>",
                         as.character(primario$ESTADO), "<br>", "<b>", "Nivel: ", "</b>", as.character(primario$NIVEL), "<br>",
                         "<b>","Cantidad de aulas: " ,"</b>",as.character(primario$Aulas),"<br>","<b>",
                         "Financiamiento: ", "</b>",
                         as.character(primario$`FUENTE DE FINANCIAMIENTO`))

popup_secundario <- paste0("<b>", "Nombre del establecimiento: ", "</b>", as.character(secundario$ESTABLECIMIENTO),
                           "<br>", "<b>","Estado de obra: ", "</b>",
                           as.character(secundario$ESTADO), "<br>", "<b>", "Nivel: ", "</b>", as.character(secundario$NIVEL), "<br>",
                           "<b>","Cantidad de aulas: " ,"</b>",as.character(secundario$Aulas),"<br>","<b>",
                           "Financiamiento: ", "</b>",
                           as.character(secundario$`FUENTE DE FINANCIAMIENTO`))



cuadrado <- "C:/Users/esteb/OneDrive/Escritorio/MapaIntGralRodríguez/cuadrado_celeste.png"

rombo <- "C:/Users/esteb/OneDrive/Escritorio/MapaIntGralRodríguez/rombo_negro.png"

triangulo <- "C:/Users/esteb/OneDrive/Escritorio/MapaIntGralRodríguez/triangulo_amarillo.png"

cuadrado_celeste <- function(cuadrado) {
  makeIcon(
    iconUrl = cuadrado, # Usar el argumento iconPath aquí
    iconWidth = 12, # Ancho del icono en píxeles
    iconHeight = 12  # Altura del icono en píxeles
  )
}

rombo_negro <- function(rombo) {
  makeIcon(
    iconUrl = rombo, # Usar el argumento iconPath aquí
    iconWidth = 13, # Ancho del icono en píxeles
    iconHeight = 13  # Altura del icono en píxeles
  )
}

triangulo_amarillo <- function(triangulo) {
  makeIcon(
    iconUrl = triangulo, # Usar el argumento iconPath aquí
    iconWidth = 13, # Ancho del icono en píxeles
    iconHeight = 13  # Altura del icono en píxeles
  )
}




rodriguez <- leaflet() %>% addTiles()%>%
  addMarkers(data = inicial,
             lng = ~Longitud,
             lat = ~Latitud,
             icon = triangulo_amarillo(triangulo),
             popup = popup_inicial,
             label = inicial$`TIPO DE OBRA`,
             group = "Inicial")%>%
  addMarkers(data = primario,
             lng = ~Longitud,
             lat = ~Latitud,
             icon = rombo_negro(rombo),
             popup = popup_primario,
             label = primario$`TIPO DE OBRA`,
             group = "Primario")%>%
  addMarkers(data = secundario,
             lng = ~Longitud,
             lat = ~Latitud,
             icon = cuadrado_celeste(cuadrado),
             popup = popup_secundario,
             label = secundario$`TIPO DE OBRA`,
             group = "Secundario")%>%
  addPolygons(data=gral_rodriguez, fillOpacity = 0.5,
              color = "grey", weight = 1, popup = "",
              label = gral_rodriguez$departamen,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE),
              labelOptions = labelOptions(direction = "auto"))%>%
  addLegend("bottomright",
            colors = c("orange","black","cyan"),
            labels = c("Inicial", "Primario","Secundario"),
            title = "Nivel")%>%
addLayersControl(overlayGroups = c("Inicial", "Primario","Secundario"),
                 options = layersControlOptions(collapsed = FALSE))
rodriguez


htmlwidgets::saveWidget(rodriguez, "MapaFormasEditadas.html")

###### HASTA ACÁ

leyenda <- "<img src='C:/Users/esteb/OneDrive/Escritorio/MapaIntGralRodríguez/cuadrado_celeste.png' width='20' height='20'> Secundario <br>
<img src='C:/Users/esteb/OneDrive/Escritorio/MapaIntGralRodríguez/triangulo_amarillo.png' width='20' height='20'> Inicial <br>
<img src='C:/Users/esteb/OneDrive/Escritorio/MapaIntGralRodríguez/rombo_negro.png' width='20' height='20'> Primario <br>"


leyenda_html <- HTML(leyenda)


rodriguez <- leaflet() %>% addTiles()%>%
  addMarkers(data = inicial,
             lng = ~Longitud,
             lat = ~Latitud,
             icon = triangulo_amarillo(triangulo),
             popup = popup_inicial,group = "Creación")%>%
  addMarkers(data = primario,
             lng = ~Longitud,
             lat = ~Latitud,
             icon = rombo_negro(rombo),
             popup = popup_primario,group = "Refacción")%>%
  addMarkers(data = secundario,
             lng = ~Longitud,
             lat = ~Latitud,
             icon = cuadrado_celeste(cuadrado),
             popup = popup_secundario,group = "Sustitución")%>%
  addPolygons(data=gral_rodriguez, fillOpacity = 0.5,
              color = "grey", weight = 1, popup = "",
              label = gral_rodriguez$departamen,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE),
              labelOptions = labelOptions(direction = "auto"))%>%
  addControl(html = leyenda_html, position = "bottomright")
rodriguez


#####


library(leaflet)

# Define la leyenda como una cadena HTML
leyenda <- "<img src='C:/Users/esteb/OneDrive/Escritorio/MapaIntGralRodríguez/cuadrado_celeste.png' width='20' height='20'> Secundario <br>
<img src='C:/Users/esteb/OneDrive/Escritorio/MapaIntGralRodríguez/triangulo_amarillo.png' width='20' height='20'> Inicial <br>
<img src='C:/Users/esteb/OneDrive/Escritorio/MapaIntGralRodríguez/rombo_negro.png' width='20' height='20'> Primario"

# Convierte la leyenda en un objeto de carácter HTML
leyenda_html <- as.character(HTML(leyenda))

# Crea el mapa
rodriguez <- leaflet() %>%
  addTiles() %>%
  # Añade marcadores, polígonos, etc., según tu necesidad
  # ...
  
  # Agrega la leyenda personalizada al mapa
  rodriguez <- rodriguez %>%
  addControl(html = leyenda_html, position = "bottomleft")

# Muestra el mapa
rodriguez

