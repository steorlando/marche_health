
# Anziani sopra il primo limite
creo_mappa(dato = "over65", titolo = paste0("Numero anziani sopra ",lim_eta," anni"))

# Anziani sopra il secondo limite
creo_mappa(dato = "over80", titolo = paste0("Numero anziani sopra ",lim_eta2," anni"))

# Proporzione pop sopra il primo limite
creo_mappa(dato = "perc_65", titolo = paste0("Proporzione anziani sopra ",lim_eta," anni su popolazione totale"))

# Proporzione pop sopra il primo limite
creo_mappa(dato = "perc_80", titolo = paste0("Proporzione anziani sopra ",lim_eta2," anni su popolazione totale"))

# Indice di invecchiamento
creo_mappa(dato = "indice_inv", titolo = paste0("Indice di invecchiamento"))


# Stesse mappe con proporzione ma solo per comuni con popolazione oltre una certa soglia

