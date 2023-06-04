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


# Save image ####
save.image (file = "code/my_work_space.RData")
