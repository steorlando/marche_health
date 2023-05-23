# Parametri di base 
lim_eta <- 65
lim_eta2 <- 80
lim_giovani <- 14

# Popolazione over limite basso
dato <- residenti_eta %>% 
  filter(eta > lim_eta) %>% 
  group_by(cod_istat) %>% 
  summarise(over65 = sum(value))

comuni_map <- left_join(comuni, dato, by = "cod_istat")

# Popolazione over limite alto
dato <- residenti_eta %>% 
  filter(eta > lim_eta2) %>% 
  group_by(cod_istat) %>% 
  summarise(over80 = sum(value))

comuni_map <- left_join(comuni_map, dato, by = "cod_istat")

# Percentuale over limit
comuni_map <- comuni_map %>% 
  mutate(perc_65 = over65/popolazione,
         perc_80 = over80/popolazione)

#Percentuale over su totale popolazione degli anziani tutta la regione
tot_over65 <- sum(comuni_map$over65)

comuni_map$prop_over65 <- comuni_map$over65/tot_over65

#Popolazione under 
dato <- residenti_eta %>% 
  filter(eta < lim_giovani) %>% 
  group_by(cod_istat) %>% 
  summarise(pop_giovani = sum(value))

comuni_map <- left_join(comuni_map, dato, by = "cod_istat")

#Indice invecchiamento
comuni_map <- comuni_map %>%   #percentuale under
  mutate(perc_giovani = pop_giovani/popolazione)

comuni_map$indice_inv <- comuni_map$perc_65/comuni_map$perc_giovani

# Aggiungere altri dati per ciascun comune

# ADI, SAD, RSA

adi <- import("data/socio-demo/DB_D02_Ass_Domic_Integ_ServSan_bis.xlsx") %>% 
  clean_names() %>% 
  select(cod_istat = pro_com_a,
         presenza,
         utenti,
         spesa_tot,
         spesa_utenti,
         spesa_ssn,
         spesanetta) %>% 
  rename_with(~ ifelse(. == "cod_istat", ., paste0("adi_", .)), everything())

sad <- import("data/socio-demo/DB_D01_Ass_Domic_Socio_Assist_bis.xlsx") %>% 
  clean_names() %>% 
  select(cod_istat = pro_com_a,
         presenza,
         utenti,
         spesa_tot,
         spesa_utenti,
         spesa_ssn,
         spesanetta) %>% 
  rename_with(~ ifelse(. == "cod_istat", ., paste0("sad_", .)), everything())

rsa <- import("data/socio-demo/DB_H01_Strutture_Residenziali_bis.xlsx") %>% 
  clean_names() %>% 
  mutate(cod_istat = substr(pro_com, start = 4, stop = 9)) %>% 
  select(cod_istat,
         presenza,
         utenti,
         spesa_tot,
         spesa_utenti,
         spesa_ssn,
         spesanetta) %>% 
  rename_with(~ ifelse(. == "cod_istat", ., paste0("rsa_", .)), everything())


comuni_map <- left_join(comuni_map, adi)
comuni_map <- left_join(comuni_map, sad)
comuni_map <- left_join(comuni_map, rsa)

# DB per la mappa in cui unisco i dati per ciascun comune con i confini dei comuni
db <- left_join(italy_comuni, comuni_map, by = "cod_istat")

db <- db %>% relocate(territorio, .before = cod_istat) %>% 
  mutate(prop65_map = percent(perc_65, accuracy = 0.01)) %>% 
  mutate(prop80_map = percent(perc_80, accuracy = 0.01)) %>% 
  var_labels(territorio         = "Comune",
             over65             = "Pop over 65",
             over80             = "Pop over 80")


# Calcolo spesa territoriale totale

db <- db %>% 
  mutate(spesa_tot = adi_spesa_tot + sad_spesa_tot + rsa_spesa_tot,
         utenti_tot = adi_utenti + sad_utenti + rsa_utenti,
         spesa_utente = spesa_tot / utenti_tot,
         spesa_citt65 = spesa_tot / over65) 

#Creo variabili con num sogg ricoverati per comune per patologia
sintesi_sdo <- import("data/SINTESI_SDO_2019.xlsx") %>% 
  clean_names()
 
library(dplyr)
sintesi_sdo <- sintesi_sdo %>%
  rename(cod_istat = comresid)

somma <- function(sintesi_sdo, variabili) {
  risultati <- aggregate(sintesi_sdo[ ,variabili], by = list(sintesi_sdo$cod_istat), FUN = sum)
  colnames(risultati)[1] <- "cod_istat"
  return(risultati)
}

variabili_somma <- c("ipertensione", "ipo_iper_tiroidismo", "asma",                 
                     "bpco", "diabete",              
                     "diabete_complicato", "cardiopatia_ischemica",
                     "scompenso_cardiaco", "demenze",              
                     "irc_non_dialitica")

risultati_somma <- somma(sintesi_sdo, variabili_somma)

db <- merge(db, risultati_somma, by = "cod_istat", all.x = T)

#Creo variabile con num sogg ricoverati per comune per patologia tra ps e ric_ordinario
calculate_counts <- function(data, var_name, ps_value, merge_data) {
  agg_data <- aggregate(data[[var_name]] ~ cod_istat, data = data, subset = ps == ps_value, FUN = sum)
  names(agg_data)[2] <- paste0("num_", var_name, "_ps", ps_value)
  merged_data <- merge(merge_data, agg_data, by = "cod_istat", all.x = T)
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
