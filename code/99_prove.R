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


# Mappa ADI con comuni bianchi (da provare 

library(RColorBrewer)

# create color palette
n <- 100 # the number of colors to be in the palette
palette <- colorRampPalette(c("white", brewer.pal(8, "Blues")))(n)

# Define breaks
breaks <- seq(0, max(db_map$adi_utenti), length.out = n)

tmap_mode("plot")
tm_shape(db_map) + 
  tm_polygons("adi_utenti", 
              style = "pretty", 
              palette = palette, 
              breaks = breaks, 
              title = "Numero utenti in ADI",
              border.lwd = 0.5,
              popup.vars = c("Pop > di 65" = "over65", 
                             "Pop totale" = "popolazione", 
                             "% anziani > 65" = "prop65_map")) + 
  tm_shape(db_map) +
  tm_bubbles("over65", col = "red" , size = "over65", scale = 1, alpha = 0.5, style = "pretty" ) +
  tm_shape(italy_province) + 
  tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) + 
  tm_layout(main.title = "Comuni per numero di utenti SAD", main.title.size = 0.8,
            legend.title.size = 0.8,
            legend.position = c("left", "bottom"))

# Load the necessary libraries
library(dplyr)
library(janitor)

# List of diseases
diseases <- c("ipertensione", "ipo_iper_tiroidismo", "asma", "bpco", "diabete", 
              "diabete_complicato", "cardiopatia_ischemica", "scompenso_cardiaco", 
              "demenze", "irc_non_dialitica")

# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(provincia, all_of(diseases), ricoveri_totali)

# Gather the diseases into a single column
df_long <- df_diseases %>%
  gather(key = "disease", value = "value", -provincia, -ricoveri_totali)

# Summarize the data by provincia and disease
df_summary <- df_long %>%
  group_by(provincia, disease) %>%
  summarise(sum_value = sum(value, na.rm = TRUE), 
            proportion = sum_value / sum(ricoveri_totali, na.rm = TRUE))

# Spread the data to wide format
df_wide <- df_summary %>%
  pivot_wider(names_from = provincia, values_from = c(sum_value, proportion))

# prov2 ####

# Load the necessary libraries
library(dplyr)
library(janitor)
library(tidyverse)

# List of diseases
diseases <- c("ipertensione", "ipo_iper_tiroidismo", "asma", "bpco", "diabete", 
              "diabete_complicato", "cardiopatia_ischemica", "scompenso_cardiaco", 
              "demenze", "irc_non_dialitica")

# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(provincia, all_of(diseases), ricoveri_totali)

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by provincia and disease
df_summary <- df_long %>%
  group_by(provincia, disease) %>%
  summarise(sum_value = sum(value, na.rm = TRUE), 
            proportion = sum_value / sum(ricoveri_totali, na.rm = TRUE))

# Spread the data to wide format
df_wide <- df_summary %>%
  pivot_wider(names_from = provincia, values_from = c(sum_value, proportion))

# Print the resulting dataframe
print(df_wide)

# prova 3 ####
# Load the necessary libraries
library(dplyr)
library(janitor)
library(tidyverse)

# List of diseases
diseases <- c("ipertensione", "ipo_iper_tiroidismo", "asma", "bpco", "diabete", 
              "diabete_complicato", "cardiopatia_ischemica", "scompenso_cardiaco", 
              "demenze", "irc_non_dialitica")

# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(provincia, all_of(diseases), ricoveri_totali)

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by provincia and disease
df_summary <- df_long %>%
  group_by(provincia, disease) %>%
  summarise(sum_value = sum(value, na.rm = TRUE), 
            proportion = round((sum_value / sum(ricoveri_totali, na.rm = TRUE)) * 100, 1))

# Spread the data to wide format
df_wide <- df_summary %>%
  pivot_wider(names_from = provincia, 
              values_from = c(sum_value, proportion),
              names_glue = "{provincia}_{.value}")

# Get the unique provinces
provinces <- unique(df_diseases$provincia)

# Manually specify the column order
column_order <- c("disease", sort(c(paste0(provinces, "_sum"), paste0(provinces, "_prop"))))

# Reorder the columns
df_wide <- df_wide[, column_order]


# Print the resulting dataframe
print(df_wide)

# prova 4 ####

# Load the necessary libraries
library(dplyr)
library(janitor)
library(tidyverse)

# List of diseases
diseases <- c("ipertensione", "ipo_iper_tiroidismo", "asma", "bpco", "diabete", 
              "diabete_complicato", "cardiopatia_ischemica", "scompenso_cardiaco", 
              "demenze", "irc_non_dialitica")

# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(provincia, all_of(diseases), ricoveri_totali)

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by provincia and disease
df_summary <- df_long %>%
  group_by(provincia, disease) %>%
  summarise(sum_value = sum(value, na.rm = TRUE), 
            proportion = round((sum_value / sum(ricoveri_totali, na.rm = TRUE)) * 100, 1))

# Spread the data to wide format for sum_value
df_wide_sum <- df_summary %>%
  select(-proportion) %>%
  pivot_wider(names_from = provincia, values_from = sum_value, names_glue = "{provincia}_sum")

# Spread the data to wide format for proportion
df_wide_prop <- df_summary %>%
  select(-sum_value) %>%
  pivot_wider(names_from = provincia, values_from = proportion, names_glue = "{provincia}_prop")

# Join the two data frames together
df_final <- df_wide_sum %>%
  full_join(df_wide_prop, by = "disease")


# Get the unique provinces
provinces <- unique(df_diseases$provincia)

# Manually specify the column order
column_order <- c("disease", sort(c(paste0(provinces, "_prop"), paste0(provinces, "_sum"))))

# Reorder the columns
df_final <- df_final[, column_order]

df_final <- df_final %>% 
  adorn_totals("row")


# Print the resulting dataframe
print(df_final)

db_map_res$col <- as.factor(db_map_res$col)

tmap_mode("plot")
tm_shape(db_map_res) + # il database con i dati
  tm_polygons("col", # il dato da mappare
              palette = c("green", "red", "white"), # set colors for different categories
              missing = "white", # set color for NA
              border.lwd = 0.5,
              popup.vars = c("Pop > di 65" = "over65", 
                             "Pop totale" = "popolazione", 
                             "% anziani > 65" = "prop65_map")) + 
  tm_shape(italy_province) + # Aggiungi i confini delle province
  tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) + # personalizza lo spessore, il colore e la trasparenza dei confini
  tm_layout(main.title = "Comuni per numero di anziani", main.title.size = 0.8,
            legend.title.size = 0.8, # Make the legend title smaller
            legend.position = c("left", "bottom")) # Reposition the legend

# Factorize your "col" column with custom labels
db_map_res$col <- factor(db_map_res$col, 
                         levels = c("red", "green", "Na"), 
                         labels = c("Più ricoveri", "Meno ricoveri", ""))

tmap_mode("plot")
tm_shape(db_map_res) + # il database con i dati
  tm_polygons("col", # il dato da mappare
              palette = c("red", "green", "white"), # set colors for different categories
              missing = "white", # set color for NA
              border.lwd = 0.5,
              popup.vars = c("Pop > di 65" = "over65", 
                             "Pop totale" = "popolazione", 
                             "% anziani > 65" = "prop65_map")) +
  tm_shape(italy_province) + # Aggiungi i confini delle province
  tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) + # personalizza lo spessore, il colore e la trasparenza dei confini
  tm_layout(main.title = "Comuni per numero di anziani", main.title.size = 0.8,
            legend.title.size = 0.8, # Make the legend title smaller
            legend.position = c("left", "bottom")) # Reposition the legend

# tabella delle patologie con ricoveri, giorni di degenza, e costo ####

## Elenco ricoveri per patologia  ####

# List of diseases
diseases <- c("ipertensione", "ipo_iper_tiroidismo", "asma", "bpco", "diabete", 
              "diabete_complicato", "cardiopatia_ischemica", "scompenso_cardiaco", 
              "demenze", "irc_non_dialitica")

# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(all_of(diseases), ricoveri_totali)

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by disease
df_summary <- df_long %>%
  group_by(disease) %>%
  summarise(ricoveri = sum(value, na.rm = TRUE), 
            prop_ricoveri = ricoveri / sum(ricoveri_totali, na.rm = TRUE))


## Elenco giorni di degenza per patologia  ####

# List of diseases
# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(ends_with("_d"))

names(df_diseases) <- sub("_d$", "", names(df_diseases))

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by disease
df_summary_d <- df_long %>%
  group_by(disease) %>%
  summarise(giorni_degenza = sum(value, na.rm = TRUE), 
            prop_giorni = giorni_degenza / sum(ricoveri_totali, na.rm = TRUE))

## Elenco costo ricoveri per patologia  ####

# List of diseases
# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(ends_with("_c"))

names(df_diseases) <- sub("_c$", "", names(df_diseases))

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by disease
df_summary_c <- df_long %>%
  group_by(disease) %>%
  summarise(costo_ricoveri = sum(value, na.rm = TRUE), 
            prop_costo = costo_ricoveri / sum(ricoveri_totali, na.rm = TRUE))

# Seconda tabella delle patologie con ricoveri, giorni di degenza, e costo ####

## Elenco ricoveri per patologia  ####

# List of diseases
diseases <- c("ipertensione", "ipo_iper_tiroidismo", "asma", "bpco", "diabete", 
              "diabete_complicato", "cardiopatia_ischemica", "scompenso_cardiaco", 
              "demenze", "irc_non_dialitica")

# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(all_of(diseases))

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by disease
df_summary <- df_long %>%
  group_by(disease) %>%
  summarise(ricoveri = sum(value, na.rm = TRUE))

altre_patologie <- sum(db$ricoveri_totali) - sum(df_summary$ricoveri)

new_row <- data.frame(disease = "altre_patologie", ricoveri = altre_patologie)
df_summary <- rbind(df_summary, new_row)

## Elenco giorni di degenza per patologia  ####

# List of diseases
# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(ends_with("_d"))

names(df_diseases) <- sub("_d$", "", names(df_diseases))

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by disease
df_summary_d <- df_long %>%
  group_by(disease) %>%
  summarise(giorni_degenza = sum(value, na.rm = TRUE))

sum(db$ricoveri_totali_d)

altre_patologie <- sum(db$ricoveri_totali_d) - sum(df_summary_d$giorni_degenza)
new_row <- data.frame(disease = "altre_patologie", giorni_degenza = altre_patologie)
df_summary_d <- rbind(df_summary_d, new_row)

## Elenco costo ricoveri per patologia  ####

# List of diseases
# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(ends_with("_c"))

names(df_diseases) <- sub("_c$", "", names(df_diseases))

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by disease
df_summary_c <- df_long %>%
  group_by(disease) %>%
  summarise(costo_ricoveri = sum(value, na.rm = TRUE))

altre_patologie <- sum(db$ricoveri_totali_c) - sum(df_summary_c$costo_ricoveri)
new_row <- data.frame(disease = "altre_patologie", costo_ricoveri = altre_patologie)
df_summary_c <- rbind(df_summary_c, new_row)

malattie <- left_join(df_summary, df_summary_d)
malattie <- left_join(malattie, df_summary_c)

malattie <- malattie %>% 
  mutate(giorni_degenza = round(giorni_degenza, digits = 0),
         costo_ricoveri = round(costo_ricoveri, digits = 0)) %>% 
  adorn_totals(where = c("row")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 3) %>% 
  adorn_ns(position = "front") %>% 
  print()

names(db)
  

sum(db$adi_utenti)
sum(db$over65)  


db_map1 <- db_map

# Ordina il dataframe in base alla colonna "over65" in ordine decrescente
db_map1_sorted <- db_map1[order(-db_map1$over65), ]

# Seleziona i primi 10 comuni
top_10_comuni <- db_map1_sorted[1:10, ]

# Aggiungi i nomi dei comuni alla mappa
tmap_mode("plot")
tm_shape(db_map1) + 
  tm_polygons("over65", 
              style = "pretty", 
              palette = "Blues", 
              title = "Popolazione > 65 anni",
              border.lwd = 0.5,
              popup.vars = c("Pop > di 65" = "over65", 
                             "Pop totale" = "popolazione", 
                             "% anziani > 65" = "prop65_map")) + 
  tm_shape(italy_province) + 
  tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) + 
  tm_shape(top_10_comuni) + # Aggiungi i top 10 comuni
  tm_text("territorio", size = 1, col = "black", remove.overlap = F) + # Aggiungi i nomi dei comuni
  tm_layout(legend.title.size = 0.8, 
            legend.position = c("left", "bottom"))

library(ggplot2)
library(ggrepel)

# Ordina il dataframe in base alla colonna "over65" in ordine decrescente
db_map1_sorted <- db_map1[order(-db_map1$over65), ]

# Seleziona i primi 20 comuni
top_20_comuni <- db_map1_sorted[1:20, ]

# Calcola il centroide di ogni comune
top_20_comuni$centroid <- st_centroid(top_20_comuni$geometry)

# Estrai le coordinate x e y dei centroidi
top_20_comuni$x <- st_coordinates(top_20_comuni$centroid)[, 1]
top_20_comuni$y <- st_coordinates(top_20_comuni$centroid)[, 2]

# Crea la mappa con ggplot2
ggplot() +
  geom_sf(data = db_map1, aes(fill = over65)) +
  geom_sf(data = italy_province, fill = NA, color = "darkgreen", size = 1.5) +
  geom_text_repel(data = top_20_comuni, 
                  aes(x = x, y = y, label = territorio), 
                  size = 3, 
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.3, "lines")) +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))



# Factorize your "col" column with custom labels costo ####
db_map_res_c$col <- factor(db_map_res_c$col, 
                           levels = c("red", "green", "Na"), 
                           labels = c("Più ricoveri", "Meno ricoveri", ""))

tmap_mode("plot")
tm_shape(db_map_res_c) + # il database con i dati
  tm_polygons("col", # il dato da mappare
              palette = c("red", "green", "white"), # set colors for different categories
              missing = "white", # set color for NA
              title = "Comuni con più o meno spesa per ricoveri NCD rispetto a valore atteso",
              border.lwd = 0.5) +
  tm_shape(italy_province) + # Aggiungi i confini delle province
  tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) + # personalizza lo spessore, il colore e la trasparenza dei confini
  tm_layout(legend.show = F) # Reposition the legend




db_map_res$col <- factor(db_map_res$col, 
                         levels = c("red", "green", "Na"), 
                         labels = c("Più ricoveri", "Meno ricoveri", ""))

tmap_mode("plot")
tm_shape(db_map_res) + # il database con i dati
  tm_polygons("col", # il dato da mappare
              palette = c("red", "green", "white"), # set colors for different categories
              missing = "white", # set color for NA
              title = "Comuni con più o meno spesa per ricoveri NCD rispetto a valore atteso",
              border.lwd = 0.5) +
  tm_shape(italy_province) + # Aggiungi i confini delle province
  tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) + # personalizza lo spessore, il colore e la trasparenza dei confini
  tm_layout(legend.show = F) # Reposition the legend

costi_farma

valori_mancanti <- setdiff(comuni_snai$territorio, db_snai_1$territorio)
print(valori_mancanti)

serra <- db %>% 
  filter(cod_istat == "041061")
print(serra$territorio)
