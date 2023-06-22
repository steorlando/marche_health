path_to_data <- file.path("..", "marche_out_git", "farmaceutica")
file_name <- "Farmaceutica_FlussoF_2019.csv"
file_path <- file.path(path_to_data, file_name)

dati_farmaceutici_flusso <- import(file_path) %>% clean_names()


file_name <- "Farmaceutica_Territoriale_2019.csv"
file_path <- file.path(path_to_data, file_name)

dati_farmaceutici_territoriale <- import(file_path) %>% clean_names()

farma_terr <- dati_farmaceutici_territoriale %>% 
  mutate(quantita = as.numeric(quantita),
         prezzo = as.numeric(prezzo)) %>% 
  mutate(costo = quantita * prezzo) %>% 
  select(atc, costo) %>% 
  mutate(tipo = "territorio")

farma_osp <- dati_farmaceutici_flusso %>% 
  select(atc, costo = imp_tot) %>% 
  mutate(tipo = "ospedale")

farma <- rbind(farma_terr, farma_osp)



# Loading necessary libraries
library(dplyr)
library(stringr)

# Creating the new column
farma <- farma %>%
  mutate(
    malattia = case_when(
      str_detect(atc, "^(C02AC01|C02CA04|C03|C07|C08C|C09|C10BX03|C10BX04|C10BX06|C10BX07|C10BX09|C10BX10|C10BX11|C10BX12|C10BX13|C10BX14|C10BX15|C02AC05|C02AB01)") ~ "Ipertensione",
      str_detect(atc, "^R03") ~ "Asma and BPCO",
      str_detect(atc, "^A10B") ~ "Diabete",
      str_detect(atc, "^(N03AX16|N03AX12)") ~ "Diabete complicato",
      str_detect(atc, "^(C01DA|C10BX01|C10BX02|C10BX04|C10BX05|C10BX06|C10BX08|C10BX12)") ~ "Cardiopatia Ischemica",
      str_detect(atc, "^(C07AG02|C07AB02|C07AB07|C03CA|C03CB|C03EB|C09A|C09C)") ~ "Scompenso cardiaco",
      str_detect(atc, "^(V03AE01|V03AE02|V03AE03|H05BX01|H05BX02)") ~ "IRC non dialitica",
      TRUE ~ "Altre patologie" # This line is for handling cases where "atc" doesn't match any of the above
    )
  )

farma <- farma %>% 
  filter(!is.na(costo))

rm(dati_farmaceutici_flusso,dati_farmaceutici_territoriale,farma_osp,farma_terr)
