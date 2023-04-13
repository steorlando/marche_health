path_to_data <- file.path("..", "marche_out_git", "sdo")
file_name <- "SDO_Ricoveri_2019_2021.csv"
file_path <- file.path(path_to_data, file_name)

dati_sdo <- import(file_path) %>% clean_names()



skim(dati_sdo)
names(dati_sdo)
