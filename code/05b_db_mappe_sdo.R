# Codice per aggiungere al databse per comune (db) i dati sulle patologie presi dalle SDO ####

#Creo variabili con num sogg ricoverati per comune per patologia
sintesi_sdo <- import("data/SINTESI_SDO_2019.xlsx", sheet = "Sintesi") %>% 
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



# Aggiungo i DALY weight ####
dw <- import("data/daly_weight.xlsx")

# Create the diseases and weights dataframes
diseases <- c("ipertensione", "ipo_iper_tiroidismo", "asma", "bpco", "diabete", "diabete_complicato", 
              "cardiopatia_ischemica", "scompenso_cardiaco", "demenze", "irc_non_dialitica")

# Create the new columns in the "db" dataframe
for (disease in diseases) {
  weight <- dw$weight[dw$disease == disease]
  db[[paste0(disease, "_w")]] <- db[[disease]] * weight
}


# Get the column names with the suffix "_w"
w_columns <- grep("_w$", names(db), value = TRUE)

# Create a new column with the sum of all "_w" columns
db <- db %>% 
  mutate(ricoveri_w = rowSums(select(., all_of(w_columns)))) %>% 
  mutate(perc_ricoveri_w = ricoveri_w/popolazione*10000)

# Aggiungo giorni di degenza e valore rimborsi ricoveri #### 
deg_val <- import("data/Sintesi_degenze_valore_rev.xlsx")

diseases <- c("ipertensione", "ipo_iper_tiroidismo", "asma", "bpco", "diabete", "diabete_complicato", 
              "cardiopatia_ischemica", "scompenso_cardiaco", "demenze", "irc_non_dialitica",
              "ricoveri_totali")

# Create the new columns with degenza in the "db" dataframe
for (disease in diseases) {
  degenza <- deg_val$degenza[deg_val$disease == disease]
  db[[paste0(disease, "_d")]] <- db[[disease]] * degenza
}

# Create the new columns with "valore" in the "db" dataframe
for (disease in diseases) {
  valore <- deg_val$valore[deg_val$disease == disease]
  db[[paste0(disease, "_c")]] <- db[[disease]] * valore
}


# vedere se mi serve ancora 
db <- db %>% 
  mutate(ricoveri_pat_d = rowSums(select(., ends_with("_d"))) - ricoveri_totali_d,
         perc_ricoveri_d = ricoveri_pat_d / ricoveri_totali_d,
         ricoveri_pat_c = rowSums(select(., ends_with("_c")))-ricoveri_totali_c,
         perc_ricoveri_c = ricoveri_pat_c / ricoveri_totali_c
  )

