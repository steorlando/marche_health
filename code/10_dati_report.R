# Dati generali ####

tot_popolazione <- sum(db$popolazione)

media_comune <- tot_popolazione/nrow(db)

n_2000 <- db %>% 
  filter(popolazione < 2000) %>% 
  nrow()


# Percentuale anziani over 65


tot_perc65 <- tot_over65 / tot_popolazione

# anziani over 80
tot_over80 <- sum(db$over80)

# Comuni con più di 2000 anziani

n_com_2000_65 <- db %>% 
  filter(over65 > 2000) %>% 
  nrow()

# comuni con più del 25% della popolazione over 65
n_com_25_65 <- db %>% 
  filter(perc_65 > .2499) %>% 
  nrow()

# SAD ####

## comuni che hanno attivato il servizio
n_com_sad <- sum(db$sad_presenza)

# Spesa servizi territoriali

spesa_adi <- sum(db$adi_spesa_tot)
spesa_sad <- sum(db$sad_spesa_tot)
spesa_rsa <- sum(db$rsa_spesa_tot)
spesa_terr <- spesa_adi + spesa_sad + spesa_rsa

# Utenti serviti
utenti_adi <- sum(db$adi_utenti)
utenti_sad <- sum(db$sad_utenti)
utenti_rsa <- sum(db$rsa_utenti)
utenti_terr <- utenti_adi + utenti_sad + utenti_rsa

# Tabelle report ####
## I primi 15 comuni per numero di anziani ####

# Perform the operations
com_anziani <- db %>%
  # Filter the municipalities with population higher than 2000
  filter(popolazione > 2000) %>%
  # Select necessary columns
  select(territorio, popolazione, over65, perc_65) %>%
  # Arrange the data in descending order based on the number of people over 65
  arrange(desc(over65)) %>%
  # Get the top 15 records
  head(15) %>% 
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "over65", "popolazione", "perc_65")


## I primi 15 comuni per % di anziani ####

# Perform the operations
perc_anziani <- db %>%
  # Filter the municipalities with population higher than 2000
  filter(popolazione > 2000) %>%
  # Select necessary columns
  select(territorio, popolazione, over65, perc_65) %>%
  # Arrange the data in descending order based on the number of people over 65
  arrange(desc(perc_65)) %>%
  # Get the top 15 records
  head(15) %>% 
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "perc_65", "over65",  "popolazione")

# I primi 10 comuni per utenti SAD su popolazione
# Perform the operations
perc_sad <- db %>%
  # Filter the municipalities with population higher than 2000
  filter(popolazione > 2000) %>%
  # Select necessary columns
  select(territorio, popolazione, sad_utenti, over65, sad_spesa_tot, sad_spesa_utente, sad_spesa_anziano ) %>%
  mutate(perc_sad_utenti = sad_utenti / popolazione) %>% 
  # Arrange the data in descending order based on the number of people over 65
  arrange(desc(perc_sad_utenti)) %>%
  # Get the top 10 records
  head(10) %>% 
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "sad_utenti", "sad_spesa_tot", "sad_spesa_utente", "sad_spesa_anziano", "over65")

# I primi 10 comuni per MENO utenti SAD su popolazione (dove presente)
# Perform the operations
perc_sad_less <- db %>%
  # Filter the municipalities with population higher than 2000
  filter(popolazione > 2000) %>%
  filter(sad_utenti > 0) %>% 
  # Select necessary columns
  select(territorio, popolazione, sad_utenti, sad_spesa_tot, sad_spesa_utente, sad_spesa_anziano, over65 ) %>%
  mutate(perc_sad_utenti = sad_utenti / popolazione) %>% 
  # Arrange the data in descending order based on the number of people over 65
  arrange(perc_sad_utenti) %>%
  # Get the top 10 records
  head(10) %>% 
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "sad_utenti", "sad_spesa_tot", "sad_spesa_utente", "sad_spesa_anziano", "over65")

# I primi 15 comuni per spesa RSA per anziano
# Perform the operations
rsa_spesa <- db %>%
  # Filter the municipalities with population higher than 2000
  filter(popolazione > 2000) %>%
  # Select necessary columns
  select(territorio, popolazione, rsa_spesa_anziano, rsa_utenti, rsa_spesa_utente, rsa_spesa_tot, over65 ) %>%
  # Arrange the data in descending order based on the number of people over 65
  arrange(desc(rsa_spesa_anziano)) %>%
  # Get the top 10 records
  head(15) %>% 
  mutate(row_order = row_number()) %>% 
  select(row_order, territorio, rsa_spesa_anziano, rsa_utenti, rsa_spesa_utente, rsa_spesa_tot, over65)

# I primi 15 comuni per minore spesa RSA per anziano
# Perform the operations
rsa_spesa_less <- db %>%
  # Filter the municipalities with population higher than 2000
  filter(popolazione > 2000) %>%
  filter(rsa_utenti > 0) %>% 
  # Select necessary columns
  select(territorio, popolazione, rsa_spesa_anziano, rsa_utenti, rsa_spesa_utente, rsa_spesa_tot, over65 ) %>%
  # Arrange the data in descending order based on the number of people over 65
  arrange(rsa_spesa_anziano) %>%
  # Get the top 10 records
  head(15) %>% 
  mutate(row_order = row_number()) %>% 
  select(row_order, territorio, rsa_spesa_anziano, rsa_utenti, rsa_spesa_utente, rsa_spesa_tot, over65)


# Elenco ricoveri per patologia e provincia ####

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

library(purrr)

# Create a new column that is the sum of all columns ending with "_sum"
df_final <- df_final %>%
  mutate(total_sum = select(., ends_with("_sum")) %>% 
           reduce(`+`))

# Mariagrazia: tabella per malattia con numero ricoveri, proporzione sui ricoveri totali, 
#numero giorni, proporzione su giorni totali, costo generato, proporzione costo totale

db_tab <- db %>%
  select(c("ipertensione", "ipo_iper_tiroidismo", "asma", "bpco", "diabete", "diabete_complicato", "cardiopatia_ischemica", "scompenso_cardiaco", "demenze", "irc_non_dialitica",
           "ricoveri_totali", "ipertensione_d", "ipo_iper_tiroidismo_d", "asma_d", "bpco_d", "diabete_d", "diabete_complicato_d", "cardiopatia_ischemica_d", "scompenso_cardiaco_d",
           "demenze_d", "irc_non_dialitica_d","ricoveri_totali_d", "ipertensione_c", "ipo_iper_tiroidismo_c", "asma_c", "bpco_c", "diabete_c", "diabete_complicato_c", 
           "cardiopatia_ischemica_c", "scompenso_cardiaco_c", "demenze_c", "irc_non_dialitica_c", "ricoveri_totali_c")) 




# Comuni che si discostano dalla regressione

resid_plus <- db_resid_multi %>%
  # Filter the municipalities with population higher than 2000
  filter(resid > 0) %>% 
  # Select necessary columns
  select(cod_istat, territorio, popolazione, over65, perc_65, ricoveri_pat, perc_ricoveri ) %>%
  # Arrange the data in descending order based on the number of people over 65
  # Get the top 15 records
  head(15) %>% 
  mutate(row_order = row_number()) %>% 
  relocate(row_order, .before = cod_istat) 

resid_less <- db_resid_multi %>%
  # Filter the municipalities with population higher than 2000
  filter(resid < 0) %>%
  arrange(resid) %>% 
  # Select necessary columns
  select(cod_istat, territorio, popolazione, over65, perc_65, ricoveri_pat, perc_ricoveri ) %>%
  # Arrange the data in descending order based on the number of people over 65
  # Get the top 15 records
  head(15) %>% 
  mutate(row_order = row_number()) %>% 
  relocate(row_order, .before = cod_istat) 

db_plus <- resid_plus %>% 
  mutate(col = "red")

db_less <- resid_less %>% 
  mutate(col = "green")

db_resid <- rbind(db_plus, db_less) %>% select(-row_order)

# DB per la mappa in cui unisco i dati per ciascun comune con i confini dei comuni
db_map_res <- left_join(italy_comuni, db_resid, by = "cod_istat") 
db_map_res <- db_map_res %>% relocate(territorio, .before = cod_istat)


# Save image ####
save.image (file = "code/my_work_space.RData")
