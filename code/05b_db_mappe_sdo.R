# Codice per aggiungere al databse per comen (db) i dati sulle patologie presi dalle SDO

#Creo variabili con num sogg ricoverati per comune per patologia
sintesi_sdo <- import("data/SINTESI_SDO_2019.xlsx") %>% 
  clean_names()

library(dplyr)
sintesi_sdo <- sintesi_sdo %>%
  rename(cod_istat = comresid)

somma <- function(sintesi_sdo, variabili) {
  #filtro per classi di eta 2 e 3
  risultati <- aggregate(sintesi_sdo[ ,variabili], by = list(sintesi_sdo$cod_istat), FUN = sum)
  colnames(risultati)[1] <- "cod_istat"
  return(risultati)
}

variabili <- c("ipertensione", "ipo_iper_tiroidismo", "asma",                 
               "bpco", "diabete",              
               "diabete_complicato", "cardiopatia_ischemica",
               "scompenso_cardiaco", "demenze",              
               "irc_non_dialitica", "ricoveri_totali")

risultati_somma <- somma(sintesi_sdo, variabili)

db <- left_join(db, risultati_somma, by = "cod_istat", all.x = T)



# Calcolo variabili aggregate
db <- db %>% 
  mutate(ricoveri_pat = ipertensione+ ipo_iper_tiroidismo+ asma+                 
           bpco+ diabete+              
           diabete_complicato+ cardiopatia_ischemica+
           scompenso_cardiaco+ demenze+              
           irc_non_dialitica,
         perc_ricoveri = ricoveri_pat / ricoveri_totali)


