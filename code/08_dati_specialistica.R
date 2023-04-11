path_to_data <- file.path("..", "marche_out_git", "ambulatoriale")
file_name <- "SPEC_AMB_1_trim_2020.csv"
file_path <- file.path(path_to_data, file_name)

dati_spec_amb_20_1 <- import(file_path) %>% clean_names()

skim(dati_spec_amb_20_1)
names(dati_spec_amb_20_1)

