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


#Aggiungo le province

# con i confini delle province 
tm_shape(marche_comuni_mappa) + # il database con i dati
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
tm_shape(marche_comuni_mappa) +
  tm_polygons("perc_lim",
              style = "jenks",
              palette = "Blues",
              title = "Comuni per popolazione") +
  tm_shape(italy_province) + # Aggiungi i confini delle province
  tm_borders(lwd = 1.5, col = "red", alpha = 1) + # personalizza lo spessore, il colore e la trasparenza dei confini
  tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5)

sum(comuni_map$pop_lim)

#PROVA MG3

#Comuni marche con rsa

rsa_n <- sum(comuni_map$rsa_presenza)

anziani_rsa <- comuni_map %>% 
  filter(rsa_presenza == 1)
anziani_rsa <- sum(anziani_rsa$over65)

anziani_rsa / tot_over65

sad_n <- sum(comuni_map$sad_presenza)

anziani_sad <- comuni_map %>% 
  filter(sad_presenza == 1)
anziani_sad <- sum(anziani_sad$over65)

anziani_sad / tot_over65

rsa_n <- sum(comuni_map$rsa_presenza)

anziani_rsa <- comuni_map %>% 
  filter(rsa_presenza == 1)
anziani_rsa <- sum(anziani_rsa$over65)

anziani_rsa / tot_over65

anziani_comuni <- comuni_map %>% 
  filter(popolazione < 1000)

hist(anziani_comuni$over65)

names(db)
db1 <- db %>% relocate(territorio, .before = cod_istat) %>% 
  var_labels(territorio         = "Sex",
             over65             = "Pop over 65")

db_map <- db

tmap_mode("view")
tm_shape(db_map) + # il database con i dati
  tm_polygons("over65", # il dato da mappare
              style = "pretty", # capire quale sia lo stile migliore
              palette = "Blues", # la scala di colori
              title = "Popolazione > 65 anni",
              border.lwd = 0.5,
              popup.vars = c("Pop > di 65" = "over65", 
                             "Pop totale" = "popolazione", 
                             "% anziani > 65" = "prop65_map")) + 
  tm_shape(italy_province) + # Aggiungi i confini delle province
  tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) + # personalizza lo spessore, il colore e la trasparenza dei confini
  tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5)

names(db)

db_map <- db %>% 
  filter(popolazione > 2000)

tmap_mode("view")
tm_shape(db_map) + # il database con i dati
  tm_polygons("perc_65", # il dato da mappare
              style = "pretty", # capire quale sia lo stile migliore
              palette = "Blues", # la scala di colori
              title = "Percentuale > 65 anni su popolazione",
              border.lwd = 0.5,
              popup.vars = c("% anziani > 65 " = "prop65_map",
                             "Pop totale" = "popolazione", 
                             "Pop > di 65" = "over65",
                             "Utenti ADI" = "adi_utenti",
                             "Utenti SAD" = "sad_utenti"
              )) +
  tm_shape(db_map) +
    tm_bubbles("adi_presenza", col = "red" , size = "adi_utenti", scale = 1.5, alpha = 0.5) +
  tm_shape(db_map) +
    tm_bubbles("sad_presenza", col = "orange" , size = "sad_utenti", scale = 1.5, alpha = 0.5) +
  tm_shape(italy_province) + # Aggiungi i confini delle province
  tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) + # personalizza lo spessore, il colore e la trasparenza dei confini
  tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5)

frq(db$perc_65)
names(db)

db_map <- db 

tmap_mode("view")
tm_shape(db_map) + # il database con i dati
  tm_polygons("spesa_utente", # il dato da mappare
              style = "pretty", # capire quale sia lo stile migliore
              palette = "Blues", # la scala di colori
              title = "Spesa per utente servizi territoriali",
              border.lwd = 0.5,
              popup.vars = c("Pop > di 65" = "over65", 
                             "Utenti" = "utenti_tot", 
                             "Spesa servizi" = "spesa_tot")) + 
  tm_shape(italy_province) + # Aggiungi i confini delle province
  tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) + # personalizza lo spessore, il colore e la trasparenza dei confini
  tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5)

library(tmap)
db_map <- db
tmap_mode("view")

# Map 1: Spesa per utente servizi territoriali
map1 <- tm_shape(db_map) +
  tm_polygons("spesa_utente",
              style = "pretty",
              palette = "Blues",
              title = "Spesa per utente servizi territoriali",
              border.lwd = 0.5,
              popup.vars = c("Pop > di 65" = "over65",
                             "Utenti" = "utenti_tot",
                             "Spesa servizi" = "spesa_tot")) +
  tm_shape(italy_province) +
  tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) +
  tm_layout(main.title = "Comuni della regione Marche - Map 1", main.title.size = 1.5)

# Map 2: Spesa per cittadini over 65
map2 <- tm_shape(db_map) +
  tm_polygons("spesa_citt65",
              style = "pretty",
              palette = "Blues",
              title = "Spesa per cittadini over 65",
              border.lwd = 0.5,
              popup.vars = c("Pop > di 65" = "over65",
                             "Utenti" = "utenti_tot",
                             "Spesa servizi" = "spesa_tot")) +
  tm_shape(italy_province) +
  tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) +
  tm_layout(main.title = "Comuni della regione Marche - Map 2", main.title.size = 1.5)

# Display the two maps side by side
tmap_arrange(map1, map2)
map2

names(sintesi_sdo)

frq(sintesi_sdo$ricoveri_totali)

ricoveri <- sintesi_sdo %>% 
  group_by(cod_istat) %>% 
  summarise(n = sum(ricoveri_totali))
ricoveri


p <- ggplot(db, aes(x=perc_65, y=perc_ricoveri)) + 
  geom_point() + 
  geom_smooth(method=lm, se=TRUE, color="red", fill="pink") + # Aggiunge una linea di regressione con intervallo di confidenza
  theme_minimal() +
  labs(x = "Percentuale di persone over 65", y = "Percentuale di ricoveri",
       title = "Scatter plot con linea di regressione")
p

db_r <- db %>% 
  filter(popolazione > 2000 )
# esegui la regressione lineare
model <- lm(perc_ricoveri ~ perc_80, data = db_r)

# crea la tabella di riepilogo
tbl_regression <- tbl_regression(model)

# stampa la tabella
print(tbl_regression)

names(db)

install.packages("raster")
install.packages("tmap")
install.packages("terra")

summary(db$rsa_spesa_tot)

# codice elaborato ma tolto . Lascio qui per backup ####
#Creo variabile con num sogg ricoverati per comune per patologia tra ps e ric_ordinario
calculate_counts <- function(data, var_name, ps_value, merge_data) {
  agg_data <- aggregate(data[[var_name]] ~ cod_istat, data = data, subset = ps == ps_value, FUN = length)
  names(agg_data)[2] <- paste0("num_", var_name, "_ps", ps_value)
  merged_data <- merge(merge_data, agg_data, by = "cod_istat", all.x = TRUE)
  return(merged_data)
}

variables <- c("ipertensione", "ipo_iper_tiroidismo", "asma",                 
               "bpco", "diabete",              
               "diabete_complicato", "cardiopatia_ischemica",
               "scompenso_cardiaco", "demenze",              
               "irc_non_dialitica")
ps_value <- c(0, 1)

for (var in variables) {
  for (ps_val in ps_value) {
    db <- calculate_counts(sintesi_sdo, var, ps_val, db)
  }
}


#Calcolo %over65/totale_ricoveri per ogni patologia cronica

percentuali <- function(sintesi_sdo, db, variabili) {
  #filtro eta = 1
  sintesi_filtered <- sintesi_sdo[sintesi_sdo$classe_di_eta == 1, ]
  
  #calcolo % per ogni variabile
  risultati <- sintesi_filtered %>%
    group_by(cod_istat) %>%
    summarise(across(all_of(variabili), ~ sum(. [classe_di_eta == 1]) / sum (ricoveri_totali), .names = "perc_{.col}")) %>%
    ungroup()
  
  db <- left_join(db, risultati, by = "cod_istat")
  
  
  return(db)
}

variabili <- c("ipertensione", "ipo_iper_tiroidismo", "asma",                 
               "bpco", "diabete",              
               "diabete_complicato", "cardiopatia_ischemica",
               "scompenso_cardiaco", "demenze",              
               "irc_non_dialitica")

db <- percentuali(sintesi_sdo, db, variabili)




somma <- function(sintesi_sdo, variabili) {
  #filtro per classi di eta 2 e 3
  filtered_2_3 <- sintesi_sdo %>% filter(classe_di_eta %in% c(2, 3))
  risultati <- aggregate(filtered_2_3[ ,variabili], by = list(filtered_2_3$cod_istat), FUN = sum)
  colnames(risultati)[1] <- "cod_istat"
  return(risultati)
}

hist(db$perc_ricoveri)
hist(log10 (db$ricoveri_w))

names(db)

p <- ggplot(db, aes(x=perc_ricoveri, y=perc_ricoveri_w)) + 
  geom_point() + 
  geom_smooth(method=lm, se=TRUE, color="red", fill="pink") + # Aggiunge una linea di regressione con intervallo di confidenza
  theme_minimal() +
  labs(x = "Percentuale di ricveri attribuiti a NCD", y = "Peso di salute ricoveri NCDs per cittadino",
       title = "Scatter plot con linea di regressione")
p
