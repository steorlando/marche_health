# Creo il livello base con i confini dei comuni usando il file GEOJson preso da github
# italy_comuni <- st_read("data/comuni.txt", quiet = TRUE) %>% 
# filter(reg_istat_code == 11) %>%
# select(cod_istat = com_istat_code, geometry)

# Creo il livello base con i confini dei comuni usando lo shapefile del 2020 preso da ISTAT ####
# https://www.istat.it/it/archivio/222527
# Ho scaricato la versione meno dettagliata

italy_comuni <- sf::read_sf(
  dsn= ("data/comuni_shape/") , 
  layer="Com01012020_g_WGS84"
) %>% 
  filter(COD_REG == 11) %>% # Codice delle marche
  select(cod_istat = PRO_COM_T, geometry)

italy_province <- sf::read_sf(
  dsn= ("data/prov_shape/") , 
  layer="ProvCM01012020_g_WGS84"
) %>% 
  filter(COD_REG == 11) %>% # Codice delle marche
  select(cod_istat = COD_PROV, geometry)

# funzione per la mappa che mi serve
creo_mappa <- function(db_map, dato, titolo) {
  tm_shape(db_map) + # il database con i dati
    tm_polygons(dato, # il dato da mappare (potrei fare una funzione)
                style = "pretty", # capire quale sia lo stile migliore
                palette = "Blues", # la scala di colori
                title = titolo) + # la legenda del colore
    tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5) + # il titolo del grafico
    tm_tooltip(vars(territorio = "Comune", popolazione = "Pop")) # aggiungi tooltip per territorio e popolazione
  
}
#volendo posso aggiungere altri elementi alla mappa. 
# vedi https://r-tmap.github.io/tmap/

