# Dati generali ####

tot_popolazione <- sum(db$popolazione)

media_comune <- tot_popolazione/nrow(db)

n_2000 <- db %>% 
  filter(popolazione < 2000) %>% 
  nrow()

n_1000 <- db %>% 
  filter(popolazione < 1000) %>% 
  nrow()


# Percentuale anziani over 65

tot_perc65 <- tot_over65 / tot_popolazione

# anziani over 80
tot_over80 <- sum(db$over80)

tot_perc80 <- tot_over80 / tot_popolazione

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
n_com_adi <- sum(db$adi_presenza)

com_adi <- db %>% filter(adi_utenti > 0)

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

## Parametri

filter_pop <- 1000
tab_row <- 20

## I primi 20 comuni per numero di anziani ####

# Perform the operations
com_anziani <- db %>%
  # Filter the municipalities with population higher than 2000
  filter(popolazione > filter_pop) %>%
  # Select necessary columns
  select(territorio, popolazione, over65, perc_65) %>%
  # Arrange the data in descending order based on the number of people over 65
  arrange(desc(over65)) %>%
  # Get the top 15 records
  head(tab_row) %>% 
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "over65", "popolazione", "perc_65")


## I primi  comuni per % di anziani ####

# Perform the operations
perc_anziani <- db %>%
  # Filter the municipalities with population higher than 2000
  filter(popolazione > filter_pop) %>%
  # Select necessary columns
  select(territorio, popolazione, over65, perc_65) %>%
  # Arrange the data in descending order based on the number of people over 65
  arrange(desc(perc_65)) %>%
  # Get the top 15 records
  head(tab_row) %>% 
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "perc_65", "over65",  "popolazione")

### Tabella completa per appendice ####
perc_anziani_full <- db %>%
  # Filter the municipalities with population higher than 2000
  # Select necessary columns
  select(territorio, popolazione, over65, perc_65) %>%
  # Arrange the data in descending order based on the number of people over 65
  arrange(desc(perc_65)) %>%
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "perc_65", "over65",  "popolazione")

## I primi 10 comuni per utenti SAD su popolazione ####
# Perform the operations
perc_sad <- db %>%
  # Filter the municipalities with population higher than 2000
  filter(popolazione > filter_pop) %>%
  # Select necessary columns
  select(territorio, popolazione, sad_utenti, over65, sad_spesa_tot, sad_spesa_utente, sad_spesa_anziano ) %>%
  mutate(sad_spesa_utente = floor(sad_spesa_utente),
         sad_spesa_anziano = floor(sad_spesa_anziano)) %>% 
  mutate(perc_sad_utenti = sad_utenti / popolazione) %>% 
  # Arrange the data in descending order based on the number of people over 65
  arrange(desc(perc_sad_utenti)) %>%
  # Get the top 10 records
  head(tab_row) %>% 
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "sad_utenti", "sad_spesa_tot", "sad_spesa_utente", "sad_spesa_anziano", "over65")

### Tabella completa per appendice ####
perc_sad_full <- db %>%
  # Filter the municipalities with SAD active
  filter(sad_utenti > 0) %>% 
  # Select necessary columns
  select(territorio, popolazione, sad_utenti, over65, sad_spesa_tot, sad_spesa_utente, sad_spesa_anziano ) %>%
  mutate(sad_spesa_utente = floor(sad_spesa_utente),
         sad_spesa_anziano = floor(sad_spesa_anziano)) %>% 
  mutate(perc_sad_utenti = sad_utenti / popolazione) %>% 
  # Arrange the data in descending order based on the number of people over 65
  arrange(desc(perc_sad_utenti)) %>%
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "sad_utenti", "sad_spesa_tot", "sad_spesa_utente", "sad_spesa_anziano", "over65")


## I primi 10 comuni per MENO utenti SAD su popolazione (dove presente) ####
# Perform the operations
perc_sad_less <- db %>%
  # Filter the municipalities with population higher than 2000
  filter(popolazione > filter_pop) %>%
  filter(sad_utenti > 0) %>% 
  # Select necessary columns
  select(territorio, popolazione, sad_utenti, sad_spesa_tot, sad_spesa_utente, sad_spesa_anziano, over65 ) %>%
  mutate(perc_sad_utenti = sad_utenti / popolazione) %>% 
  mutate(sad_spesa_utente = floor(sad_spesa_utente),
         sad_spesa_anziano = floor(sad_spesa_anziano)) %>% 
  # Arrange the data in descending order based on the number of people over 65
  arrange(perc_sad_utenti) %>%
  # Get the top 10 records
  head(tab_row) %>% 
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "sad_utenti", "sad_spesa_tot", "sad_spesa_utente", "sad_spesa_anziano", "over65")

## I primi tab_row comuni per spesa RSA per anziano ####
# Perform the operations
rsa_spesa <- db %>%
  # Filter the municipalities with population higher than filter_pop
  filter(popolazione > filter_pop) %>%
  # Select necessary columns
  select(territorio, popolazione, rsa_spesa_anziano, rsa_utenti, rsa_spesa_utente, rsa_spesa_tot, over65 ) %>%
  mutate(rsa_spesa_utente = floor(rsa_spesa_utente),
         rsa_spesa_anziano = floor(rsa_spesa_anziano)) %>% 
  # Arrange the data in descending order based on the number of people over 65
  arrange(desc(rsa_spesa_anziano)) %>%
  # Get the top 10 records
  head(tab_row) %>% 
  mutate(row_order = row_number()) %>% 
  select(row_order, territorio, rsa_spesa_anziano, rsa_utenti, rsa_spesa_utente, rsa_spesa_tot, over65)

### Tabella completa per appendice ####
rsa_spesa_full <- db %>%
  filter(rsa_utenti > 0) %>% 
  # Select necessary columns
  select(territorio, popolazione, rsa_spesa_anziano, rsa_utenti, rsa_spesa_utente, rsa_spesa_tot, over65 ) %>%
  mutate(rsa_spesa_utente = floor(rsa_spesa_utente),
         rsa_spesa_anziano = floor(rsa_spesa_anziano)) %>% 
  # Arrange the data in descending order based on the number of people over 65
  arrange(desc(rsa_spesa_anziano)) %>%
  mutate(row_order = row_number()) %>% 
  select(row_order, territorio, rsa_spesa_anziano, rsa_utenti, rsa_spesa_utente, rsa_spesa_tot, over65)


# I primi tab_row comuni per minore spesa RSA per anziano
# Perform the operations
rsa_spesa_less <- db %>%
  # Filter the municipalities with population higher than filter_pop
  filter(popolazione > filter_pop) %>%
  filter(rsa_utenti > 0) %>% 
  # Select necessary columns
  select(territorio, popolazione, rsa_spesa_anziano, rsa_utenti, rsa_spesa_utente, rsa_spesa_tot, over65 ) %>%
  mutate(rsa_spesa_utente = floor(rsa_spesa_utente),
         rsa_spesa_anziano = floor(rsa_spesa_anziano)) %>% 
  # Arrange the data in descending order based on the number of people over 65
  arrange(rsa_spesa_anziano) %>%
  # Get the top 10 records
  head(tab_row) %>% 
  mutate(row_order = row_number()) %>% 
  select(row_order, territorio, rsa_spesa_anziano, rsa_utenti, rsa_spesa_utente, rsa_spesa_tot, over65)


# Elenco ricoveri per patologia e provincia ####

# List of diseases
diseases <- c("Ipertensione", "Asma", "BPCO", "Diabete", 
              "Diabete complicato", "Cardiopatia ischemica", "Scompenso cardiaco", 
              "Demenze", "IRC non dialitica")

db_m <- db %>% 
  #select(-ipo_iper_tiroidismo) %>% 
  rename(Ipertensione = ipertensione,
         Asma = asma,
         BPCO = bpco,
         Diabete = diabete,
         'Diabete complicato' = diabete_complicato,
         'Cardiopatia ischemica' = cardiopatia_ischemica,
         'Scompenso cardiaco' = scompenso_cardiaco,
         Demenze = demenze,
         'IRC non dialitica' = irc_non_dialitica
  )

# Create a new dataframe with only the necessary columns
df_diseases <- db_m %>%
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

# Elenco ricoveri per patologia con giorni e costi ####


# Create a new dataframe with only the necessary columns
df_diseases <- db_m %>%
  select(all_of(diseases))

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by disease
df_summary <- df_long %>%
  group_by(disease) %>%
  summarise(ricoveri = sum(value, na.rm = TRUE))

altre_patologie <- sum(db$ricoveri_totali) - sum(df_summary$ricoveri)

new_row <- data.frame(disease = "Altre patologie", ricoveri = altre_patologie)
df_summary <- rbind(df_summary, new_row)

## Elenco giorni di degenza per patologia  ####

# List of diseases
# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(ends_with("_d"))

names(df_diseases) <- sub("_d$", "", names(df_diseases))

df_diseases <- df_diseases %>% 
  #select(-ipo_iper_tiroidismo) %>% 
  rename(Ipertensione = ipertensione,
         Asma = asma,
         BPCO = bpco,
         Diabete = diabete,
         'Diabete complicato' = diabete_complicato,
         'Cardiopatia ischemica' = cardiopatia_ischemica,
         'Scompenso cardiaco' = scompenso_cardiaco,
         Demenze = demenze,
         'IRC non dialitica' = irc_non_dialitica
  )

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by disease
df_summary_d <- df_long %>%
  group_by(disease) %>%
  summarise(giorni_degenza = sum(value, na.rm = TRUE))

altre_patologie <- sum(db$ricoveri_totali_d) - sum(df_summary_d$giorni_degenza)
new_row <- data.frame(disease = "Altre patologie", giorni_degenza = altre_patologie)
df_summary_d <- rbind(df_summary_d, new_row)

## Elenco costo ricoveri per patologia  ####

diseases <- c("Ipertensione", "Asma", "BPCO", "Diabete", 
              "Diabete complicato", "Cardiopatia ischemica", "Scompenso cardiaco", 
              "Demenze", "IRC non dialitica")

# List of diseases
# Create a new dataframe with only the necessary columns
df_diseases <- db %>%
  select(ends_with("_c"))

names(df_diseases) <- sub("_c$", "", names(df_diseases))

df_diseases <- df_diseases %>% 
 # select(-ipo_iper_tiroidismo) %>% 
  rename(Ipertensione = ipertensione,
         Asma = asma,
         BPCO = bpco,
         Diabete = diabete,
         'Diabete complicato' = diabete_complicato,
         'Cardiopatia ischemica' = cardiopatia_ischemica,
         'Scompenso cardiaco' = scompenso_cardiaco,
         Demenze = demenze,
         'IRC non dialitica' = irc_non_dialitica
         )

# Gather the diseases into a single column
df_long <- df_diseases %>%
  pivot_longer(cols = all_of(diseases), names_to = "disease", values_to = "value")

# Summarize the data by disease
df_summary_c <- df_long %>%
  group_by(disease) %>%
  summarise(costo_ricoveri = sum(value, na.rm = TRUE))

altre_patologie <- sum(db$ricoveri_totali_c) - sum(df_summary_c$costo_ricoveri)
new_row <- data.frame(disease = "Altre patologie", costo_ricoveri = altre_patologie)
df_summary_c <- rbind(df_summary_c, new_row)

malattie <- left_join(df_summary, df_summary_d)
malattie_num <- left_join(malattie, df_summary_c)

altri_ric <- malattie_num %>% filter(disease == "Altre patologie") %>% pull(ricoveri)
altri_cos <- malattie_num %>% filter(disease == "Altre patologie") %>% pull(costo_ricoveri)
costo_ncd <- sum(malattie_num$costo_ricoveri) - altri_cos

perc_ricoveri_ncd <- percent((sum(malattie_num$ricoveri) - altri_ric) / sum(malattie_num$ricoveri), accuracy = 0.01)
perc_costo_ncd <- percent((sum(malattie_num$costo_ricoveri) - altri_cos) / sum(malattie_num$costo_ricoveri), accuracy = 0.01)

malattie <- malattie_num %>% 
  mutate(giorni_degenza = round(giorni_degenza, digits = 0),
         costo_ricoveri = round(costo_ricoveri/1000000, digits = 2)) %>% 
  adorn_totals(where = c("row")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 2) %>% 
  adorn_ns(position = "front")

# Comuni che si discostano dalla regressione con perc ricoveri ####

resid_plus <- db_resid_multi_p %>%
  # Filter the municipalities with population higher than filter_pop
  filter(resid > 0) %>% 
  # Select necessary columns
  select(cod_istat, territorio, popolazione, over65, perc_65, ricoveri_pat, perc_ricoveri ) %>%
  # Get the top tab_row records
  head(tab_row) %>% 
  mutate(row_order = row_number()) %>% 
  relocate(row_order, .before = cod_istat) 

resid_less <- db_resid_multi_p %>%
  # Filter the municipalities with population higher than filter_pop
  filter(resid < 0) %>%
  arrange(resid) %>% 
  # Select necessary columns
  select(cod_istat, territorio, popolazione, over65, perc_65, ricoveri_pat, perc_ricoveri ) %>%
  # Arrange the data in descending order based on the number of people over 65
  # Get the top tab_row records
  head(tab_row) %>% 
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

# Comuni che si discostano dalla regressione con COSTO ricoveri ####

resid_plus_c <- db_resid_multi_c %>%
  # Filter the municipalities with population higher than filter_pop
  filter(resid > 0) %>% 
  # Select necessary columns
  select(cod_istat, territorio, popolazione, over65, perc_65, ricoveri_pat, perc_ricoveri_c_pop) %>%
  mutate(perc_65 = percent(perc_65, accuracy = 0.1),
         perc_ricoveri_c_pop = round(perc_ricoveri_c_pop)) %>% 
  # Arrange the data in descending order based on the number of people over 65
  # Get the top tab_row records
  head(tab_row) %>% 
  mutate(row_order = row_number()) %>% 
  relocate(row_order, .before = cod_istat) 

resid_less_c <- db_resid_multi_c %>%
  # Filter the municipalities with population higher than filter_pop
  filter(resid < 0) %>%
  arrange(resid) %>% 
  # Select necessary columns
  select(cod_istat, territorio, popolazione, over65, perc_65, ricoveri_pat, perc_ricoveri_c_pop ) %>%
  mutate(perc_65 = percent(perc_65, accuracy = 0.1),
         perc_ricoveri_c_pop = round(perc_ricoveri_c_pop)) %>% 
  # Arrange the data in descending order based on the number of people over 65
  # Get the top tab_row records
  head(tab_row) %>% 
  mutate(row_order = row_number()) %>% 
  relocate(row_order, .before = cod_istat) 

db_plus_c <- resid_plus_c %>% 
  mutate(col = "red")

db_less_c <- resid_less_c %>% 
  mutate(col = "green")

db_resid_c <- rbind(db_plus_c, db_less_c) %>% select(-row_order)

names(db)
### Tabella spesa NCDS ospedale per comune per appendice ####
ncd_osp_full <- db %>%
  # Filter the municipalities with population higher than filter_pop
  # Select necessary columns
  select(territorio, popolazione, over65, perc_65, ricoveri_pat, ricoveri_totali_c, perc_ricoveri_c_pop) %>%
  arrange(desc(perc_ricoveri_c_pop)) %>% 
  mutate(perc_65 = percent(perc_65, accuracy = 0.1),
         perc_ricoveri_c_pop = round(perc_ricoveri_c_pop),
         ricoveri_totali_c = round(ricoveri_totali_c)) %>% 
  # Arrange the data in descending order based on the number of people over 65
  # Get the top tab_row records
  mutate(row_order = row_number()) %>% 
  relocate(row_order, .before = territorio) 

# DB per la mappa in cui unisco i dati per ciascun comune con i confini dei comuni
db_map_res_c <- left_join(italy_comuni, db_resid_c, by = "cod_istat") 
db_map_res_c <- db_map_res_c %>% relocate(territorio, .before = cod_istat)


# Calcolo costi farmaceutica ####

costi_farma <- farma %>% 
  group_by(malattia, tipo) %>% 
  summarise(spesa = round(sum(costo)/1000000, digits = 2)) %>% 
  pivot_wider(names_from = tipo, values_from = spesa) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 2) %>% 
  adorn_ns(position = "front")



# Save image ####
save.image (file = "code/my_work_space.RData")
