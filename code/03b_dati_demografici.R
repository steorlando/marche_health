# questi i residenti per comune per età solo delle marche
residenti_eta_or <- import("data/socio-demo/pop_eta_marche_2020.csv") %>%  clean_names() # caricati dati istat demografici

# Pulizia e selezione dati demografici istat
residenti_eta <- residenti_eta_or %>% 
  filter(!grepl("IT", itter107)) %>% # tolgo i dati generali
  filter(statciv2 == "99") %>% # includo tutti gli stati civili
  filter(sexistat1 == "9")
#  mutate(eta = as.numeric(gsub("Y", "", eta1)))

comuni <- residenti_eta %>%
  filter(eta1 == "TOTAL") %>% 
  select(cod_istat = itter107, territorio, popolazione = value)

# Le province sono 5: Ancona, Ascoli Piceno, Fermo, Macerata, Pesaro e Urbino
# A seconda delle prime 3 cifre del codice comune posso attribuire la provincia 

# aggiungo la provincia
comuni <- comuni %>% 
  mutate(provincia = case_when(
    startsWith(cod_istat, "042") ~ "Ancona",
    startsWith(cod_istat, "044") ~ "Ascoli Piceno",
    startsWith(cod_istat, "109") ~ "Fermo",
    startsWith(cod_istat, "043") ~ "Macerata",
    startsWith(cod_istat, "041") ~ "Pesaro e Urbino"
  )) %>% 
  relocate(provincia, .after = cod_istat)

# sistemo bene le età
residenti_eta <- residenti_eta %>% 
  filter(!eta1 == "TOTAL") %>% 
  mutate(eta1 = ifelse(eta1 == "Y_GE100", "Y100", eta1)) %>% 
  mutate(eta = as.numeric(gsub("Y", "", eta1))) %>% 
  select(cod_istat = itter107,
         eta,
         value)


