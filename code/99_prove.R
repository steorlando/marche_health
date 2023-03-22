# Read this shape file with the rgdal library. 
library(rgdal)
my_spdf <- readOGR( 
  dsn= paste0(getwd(),"/data/comuni_shape/") , 
  layer="TCom01012020_g_WGS84",
  verbose=FALSE
)

library(terra)
prova_shape <- sf::read_sf(
  dsn= ("data/comuni_shape/") , 
  layer="Com01012020_g_WGS84"
)


Aggiungo le province

# con i confini delle province 
tm_shape(marche_comuni_pop) + # il database con i dati
  tm_polygons("popolazione", # il dato da mappare (potrei fare una funzione)
              style = "cont", # non so cosa è?
              palette = "Blues", # la scala di colori
              title = "Comuni per popolazione") + # la legenda del colore
  
  tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5) # il titolo del grafico

library(tmap)
library(sf)

# Carica i dati
# marche_comuni_pop <- ...
# italy_province <- ...

# Mappa con i confini delle province aggiunti
tm_shape(marche_comuni_pop) +
  tm_polygons("perc_lim",
              style = "jenks",
              palette = "Blues",
              title = "Comuni per popolazione") +
  tm_shape(italy_province) + # Aggiungi i confini delle province
  tm_borders(lwd = 1.5, col = "red", alpha = 0.5) + # personalizza lo spessore, il colore e la trasparenza dei confini
  tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5)
