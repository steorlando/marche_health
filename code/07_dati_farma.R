path_to_data <- file.path("..", "marche_out_git", "farmaceutica")
file_name <- "Farmaceutica_FlussoF_2020.csv"
file_path <- file.path(path_to_data, file_name)

dati_farmaceutici_flusso <- import(file_path) %>% clean_names()


file_name <- "Farmaceutica_Territoriale_2020.csv"
file_path <- file.path(path_to_data, file_name)

dati_farmaceutici_territoriale <- import(file_path) %>% clean_names()


skim(dati_farmaceutici_territoriale)
names(dati_farmaceutici_territoriale)
