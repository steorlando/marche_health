comuni_cratere <- import("data/comuni_cratere.xlsx")

comuni_cratere <- comuni_cratere %>% 
  mutate(cratere = TRUE) %>% 
  rename(territorio = nom_comune)

db_crat <- left_join(db, comuni_cratere)
db_cod <- db %>% 
  select(territorio, cod_istat)

db_crat1 <- left_join(comuni_cratere, db_cod)


db_crat <- db_crat %>% 
  filter(cratere == TRUE)

cratere_anz <- sum(db_crat$over65)
cratere_pop <- sum(db_crat$popolazione)
cratere_prop_anz <- cratere_anz/cratere_pop
