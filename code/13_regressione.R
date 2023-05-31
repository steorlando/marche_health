db_r <- db %>% 
  filter(popolazione > 2000 )

p <- ggplot(db_r, aes(x=perc_65, y=perc_ricoveri)) + 
  geom_point() + 
  geom_smooth(method=lm, se=TRUE, color="red", fill="pink") + # Aggiunge una linea di regressione con intervallo di confidenza
  theme_minimal() +
  labs(x = "Percentuale di persone over 65", y = "Percentuale di ricoveri",
       title = "Scatter plot con linea di regressione")
p


# esegui la regressione lineare
model <- lm(perc_ricoveri ~ perc_80, data = db_r)

# crea la tabella di riepilogo
tbl_regression <- tbl_regression(model)

# stampa la tabella
print(tbl_regression)