#Aggiungo nel db la variabile reddito 
reddito <- import("data/socio-demo/I invio Richiesta Sanita_09_02_23.xlsx",
                  sheet = "dati irpef 2015 2020") %>% 
  clean_names()

db <- left_join(db, reddito[ , c("cod_istat", "reddito_2019")], by = "cod_istat")

#Inserisco stranieri 
stranieri <- read_excel("data/socio-demo/I invio Richiesta Sanita_09_02_23.xlsx",
                        sheet = "stranieri 2022") 
stranieri <- stranieri %>%
  mutate(`65-74` = as.numeric(`65-74`),
         `75 e più` = as.numeric(`75 e più`)) %>%
  mutate(stranieri_over_65 = `65-74` + `75 e più`)

stranieri_db <- dplyr::select(stranieri, cod_istat, totale, stranieri_over_65)

db <- left_join(db, stranieri_db, by = "cod_istat")

db <- db %>%
  rename(totale_stranieri = totale)

#Creo variabile istruzione in cui è riportato la % di persone con licenza media in giù
istruzione <- import("data/socio-demo/istruzione_2019.xlsx") %>%
  clean_names()


istruzione <- istruzione %>%
  mutate(istruzione_bassa = (nessun_titolo_di_studio + licenza_di_scuola_elementare + licenza_di_scuola_media_inferiore_o_di_avviamento_professionale) / totale)

istruzione_bassa <- dplyr::select(istruzione, cod_istat, istruzione_bassa)


db <- left_join(db, istruzione_bassa, by = "cod_istat")
