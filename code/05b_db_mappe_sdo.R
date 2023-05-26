# Codice per aggiungere al databse per comen (db) i dati sulle patologie presi dalle SDO

#Creo variabili con num sogg ricoverati per comune per patologia
sintesi_sdo <- import("data/SINTESI_SDO_2019.xlsx") %>% 
  clean_names()

library(dplyr)
sintesi_sdo <- sintesi_sdo %>%
  rename(cod_istat = comresid)

somma <- function(sintesi_sdo, variabili) {
  #filtro per classi di eta 2 e 3
  filtered_2_3 <- sintesi_sdo %>% filter(classe_di_eta %in% c(2, 3))
  risultati <- aggregate(filtered_2_3[ ,variabili], by = list(filtered_2_3$cod_istat), FUN = sum)
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

#Aggiungo nel db la variabile reddito 
reddito <- read_excel("data/socio-demo/I invio Richiesta Sanita_09_02_23.xlsx",
                      sheet = "dati irpef 2015 2020") %>% 
  clean_names()

db <- left_join(db, reddito[ , c("cod_istat", "reddito_2019")], by = "cod_istat")



# Calcolo variabili aggregate
db <- db %>% 
  mutate(ricoveri_pat = ipertensione+ ipo_iper_tiroidismo+ asma+                 
           bpco+ diabete+              
           diabete_complicato+ cardiopatia_ischemica+
           scompenso_cardiaco+ demenze+              
           irc_non_dialitica,
         perc_ricoveri = ricoveri_pat / ricoveri_totali)

db <- db %>% relocate(territorio, .before = cod_istat) %>% 
  relocate(geometry, .after = cod_istat)

