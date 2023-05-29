#Aggiungo nel db la variabile reddito 
reddito <- import("data/socio-demo/I invio Richiesta Sanita_09_02_23.xlsx",
                  sheet = "dati irpef 2015 2020") %>% 
  clean_names()

db <- left_join(db, reddito[ , c("cod_istat", "reddito_2019")], by = "cod_istat")

