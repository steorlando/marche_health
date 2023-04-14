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

#Percentuale over (sia 65 che 80) su totale popolazione over65 e over80 di tutta la regione
 

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


# DB per la mappa in cui unisco i dati per ciascun comune con i confini dei comuni
marche_comuni_mappa <- left_join(italy_comuni, comuni_map, by = "cod_istat")


