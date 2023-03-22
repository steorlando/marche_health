# Parametri 
lim_eta <- 65
lim_eta2 <- 80

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

# DB per la mappa
marche_comuni_pop <- left_join(italy_comuni, comuni_map, by = "cod_istat")


