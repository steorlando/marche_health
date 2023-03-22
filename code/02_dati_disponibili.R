# importo registro

# Dati sulle SDO, da cui capire la spesa ospedaliera
sdo <-import("data/SDO_Ricoveri_2019_2021.csv")

# Spesa ambulatoriale 
amb_1_trim <- import("data/SPEC_AMB_1_trim_2019.csv")

#Spesa faramceutica
farma <- import("data/farmaceutica/Farmaceutica_FlussoF_2019.csv")
farma_terr <- import("data/farmaceutica/Farmaceutica_Territoriale_2019.csv")

# Per questi dati mi conviene capire quali annalisi fa mennini e se mi può 
# Fornire lui quello che mi serve 