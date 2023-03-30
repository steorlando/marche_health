# Parametri di base 
lim_eta <- 65
lim_eta2 <- 80
lim_giovani <- 14

# Popolazione over limite basso
dato <- residenti_eta %>% 
  filter(eta > lim_eta) %>% 
  group_by(cod_istat) %>% 
  summarise(pop_lim = sum(value))

comuni_map <- left_join(comuni, dato, by = "cod_istat")

# Popolazione over limite alto
dato <- residenti_eta %>% 
  filter(eta > lim_eta2) %>% 
  group_by(cod_istat) %>% 
  summarise(pop_lim2 = sum(value))

comuni_map <- left_join(comuni_map, dato, by = "cod_istat")

# Percentuale over limit
comuni_map <- comuni_map %>% 
  mutate(perc_lim = pop_lim/popolazione,
         perc_lim2 = pop_lim2/popolazione)

# Aggiungere altri dati per ciascun comune


# DB per la mappa in cui unisco i dati per ciascun comune con i confini dei comuni
marche_comuni_mappa <- left_join(italy_comuni, comuni_map, by = "cod_istat")


