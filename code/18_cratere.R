comuni_cratere <- import("data/comuni_cratere.xlsx")

comuni_cratere <- comuni_cratere %>% 
  mutate(cratere = TRUE) %>% 
  rename(territorio = nom_comune)

db_crat <- left_join(db, comuni_cratere)

db_crat <- db_crat %>% 
  filter(cratere == TRUE)

cratere_anz <- sum(db_crat$over65)
cratere_pop <- sum(db_crat$popolazione)
cratere_prop_anz <- cratere_anz/cratere_pop
cratere_anz80 <- sum(db_crat$over80)
cratere_prop_anz80 <- cratere_anz80/cratere_pop


### Tabella completa per appendice ####
perc_anziani_crat <- db_crat %>%
  # Filter the municipalities with population higher than 2000
  # Select necessary columns
  select(territorio, popolazione, over65, perc_65, over80, perc_80) %>%
  # Arrange the data in descending order based on the number of people over 65
  arrange(territorio) %>%
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "popolazione",  "over65", "perc_65", "over80", "perc_80" ) %>% 
  mutate(perc_65 = percent(perc_65, accuracy = 0.1, decimal.mark = ",")) %>% 
  mutate(perc_80 = percent(perc_80, accuracy = 0.1, decimal.mark = ",")) 

tot_crat <- c("Total", "-", cratere_pop, cratere_anz, percent(cratere_prop_anz, accuracy = 0.1, decimal.mark = ","),
              cratere_anz80, percent(cratere_prop_anz80, accuracy = 0.1, decimal.mark = ","))

perc_anziani_crat <- rbind(perc_anziani_crat, tot_crat)

perc_anziani_crat <- perc_anziani_crat %>% 
  mutate(popolazione = as.numeric(popolazione),
         over65 = as.numeric(over65),
         over80 = as.numeric(over80))


tab_c <- flextable(perc_anziani_crat)


# Modify the header names
tab_c <- set_header_labels(tab_c, 
                         row_order = "N.",
                         territorio = "Comune", 
                         popolazione = "Popolazione Totale", 
                         over65 = "Anziani over 65", 
                         perc_65 = "Proporzione over 65",
                         over80 = "Anziani over 80", 
                         perc_80 = "Proporzione over 80")

#Add the caption
tab_c <- set_caption(tab_c, caption = "I comuni delle cratere del terremoto")

# Adjust the table
tab_c <- tab_c %>%  
  theme_zebra() %>% 
  autofit() %>% 
  fontsize(size = 9, part = "body") %>% 
  padding(padding.top = 0, padding.bottom = 0.5, part = "all") %>% 
  colformat_num(j = 3:7, big.mark = ".")

save_as_docx(tab_c, path = "output/cratere.docx")

# in excel ####
perc_anziani_crat_exp <- db_crat %>%
# Filter the municipalities with population higher than 2000
# Select necessary columns
select(territorio, popolazione, over65, perc_65, over80, perc_80) %>%
  # Arrange the data in descending order based on the number of people over 65
  arrange(territorio) %>%
  mutate(row_order = row_number()) %>% 
  select("row_order", "territorio", "popolazione",  "over65", "perc_65", "over80", "perc_80" ) %>% 
  mutate(perc_65 = round(perc_65, digits = 3)) %>% 
  mutate(perc_80 = round(perc_80, digits = 3))

perc_anziani_crat_exp <- perc_anziani_crat_exp %>% 
  rename(Comune = territorio,
         Popolazione = popolazione,
         'Anziani over 65' = over65,
         'Proporzione over 65' = perc_65,
         'Anziani over 80' = over80,
         'Proporzione over 80' = perc_80)

export(perc_anziani_crat_exp, "output/cratere.xlsx")

