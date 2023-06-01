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

