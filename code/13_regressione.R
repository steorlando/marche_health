# Regressione usando perc_ricoveri ####
db_r <- db %>% 
  filter(popolazione > 2000) %>% 
  mutate(reddito_adj = reddito_2019/100)

p <- ggplot(db_r, aes(x=perc_65, y=perc_ricoveri)) + 
  geom_point() + 
  geom_smooth(method=lm, se=TRUE, color="red", fill="pink") + # Aggiunge una linea di regressione con intervallo di confidenza
  theme_minimal() +
  labs(x = "Percentuale di persone over 65", y = "Percentuale di ricoveri",
       title = "Scatter plot con linea di regressione")
p

# esegui la regressione lineare
model <- lm(perc_ricoveri ~ perc_65, data = db_r)


# crea la tabella di riepilogo
tbl_regression <- tbl_regression(model) %>% 
  add_glance_source_note(    
    include = c(r.squared, adj.r.squared, AIC, nobs)
  )

# stampa la tabella

# analisi univariata
db_r1 <- db_r %>% 
  dplyr::select(perc_ricoveri, perc_65, reddito_adj , totale_stranieri , istruzione_bassa) 

univariata <- tbl_uvregression(data = db_r1,
                               method = lm,
                               y = perc_ricoveri)


# esegui la regressione lineare multivariabile
model_multi <- lm(perc_ricoveri ~ perc_65 + reddito_adj, data = db_r)

tbl_regression_multi <- tbl_regression(model_multi) %>% 
  add_glance_source_note(    
    include = c(r.squared, adj.r.squared, AIC, nobs)
  )



# Compute the residuals from the model
resid <- residuals(model_multi)

# Add residuals to the original data frame
db_r_with_resid <- cbind(db_r, resid)

# Sort the data frame by the absolute value of the residuals, in descending order
db_resid_multi <- db_r_with_resid[order(abs(db_r_with_resid$resid), decreasing = TRUE),]


library(lmtest)
bptest(model_multi)

library(car)
ncvTest(model_multi)

plot(predict(model_multi), residuals(model_multi),
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, lty = 2)

# Regressione usando i ricoveri pesati ####
db_r <- db %>% 
  filter(popolazione > 2000) %>% 
  mutate(reddito_adj = reddito_2019/100)

p <- ggplot(db_r, aes(x=perc_65, y=perc_ricoveri_w)) + 
  geom_point() + 
  geom_smooth(method=lm, se=TRUE, color="red", fill="pink") + # Aggiunge una linea di regressione con intervallo di confidenza
  theme_minimal() +
  labs(x = "Percentuale di persone over 65", y = "Percentuale di ricoveri",
       title = "Scatter plot con linea di regressione")


library(broom)
library(gtsummary)

# Perform the linear regression
model <- lm(perc_ricoveri_w ~ perc_65, data = db_r)

# Create the summary table
tbl_regression <- tbl_regression(model) 

# crea la tabella di riepilogo
tbl_regression <- tbl_regression(model) %>% 
  add_glance_source_note(    
    include = c(r.squared, adj.r.squared, AIC, nobs)
  )




# analisi univariata
db_r1 <- db_r %>% 
  dplyr::select(perc_ricoveri_w, perc_65, reddito_adj , totale_stranieri , istruzione_bassa) 

univariata <- tbl_uvregression(data = db_r1,
                               method = lm,
                               y = perc_ricoveri_w)


# Compute the residuals from the model
resid <- residuals(model)

# Add residuals to the original data frame
db_r_with_resid <- cbind(db_r, resid)

# Sort the data frame by the absolute value of the residuals, in descending order
db_r_with_resid <- db_r_with_resid[order(abs(db_r_with_resid$resid), decreasing = TRUE),]


library(lmtest)
bptest(model_multi)

library(car)
ncvTest(model_multi)

p_resid <- plot(predict(model_multi), residuals(model_multi),
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, lty = 2)

