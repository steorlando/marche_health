comuni_snai <- import("data/snai.xlsx")

comuni_snai <- comuni_snai %>% 
  mutate(snai = TRUE) %>% 
  rename(territorio = Comuni) %>% 
  select(territorio, snai)

db <- left_join(db, comuni_snai)

db_snai <- db %>% 
  filter(snai == TRUE)

# tabella in excel completa####
completa <- db %>%
  # Filter the municipalities with population higher than 2000
  # Select necessary columns
  select(territorio, popolazione, over65, perc_65, over80, perc_80) %>%
  # Arrange the data in descending order based on the number of people over 65
  arrange(territorio) %>%
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "popolazione",  "over65", "perc_65", "over80", "perc_80" ) %>% 
  mutate(perc_65 = round(perc_65, digits = 3)) %>% 
  mutate(perc_80 = round(perc_80, digits = 3))

completa <- completa %>% 
  rename(Comune = territorio,
         Popolazione = popolazione,
         'Anziani over 65' = over65,
         'Proporzione over 65' = perc_65,
         'Anziani over 80' = over80,
         'Proporzione over 80' = perc_80)

export(completa, "output/comuni.xlsx")

# tabella in excel SNAI####
snai <- db_snai %>%
  # Filter the municipalities with population higher than 2000
  # Select necessary columns
  select(territorio, popolazione, over65, perc_65, over80, perc_80) %>%
  # Arrange the data in descending order based on the number of people over 65
  arrange(territorio) %>%
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "popolazione",  "over65", "perc_65", "over80", "perc_80" ) %>% 
  mutate(perc_65 = round(perc_65, digits = 3)) %>% 
  mutate(perc_80 = round(perc_80, digits = 3))

snai <- snai %>% 
  rename(Comune = territorio,
         Popolazione = popolazione,
         'Anziani over 65' = over65,
         'Proporzione over 65' = perc_65,
         'Anziani over 80' = over80,
         'Proporzione over 80' = perc_80)

export(snai, "output/snai.xlsx")

