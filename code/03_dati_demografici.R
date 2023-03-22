# Dati ISTAT popolazione residente per comune 
residenti_comune_or <- import("data/socio-demo/pop_res_istat.csv")
residenti_comune <- residenti_comune_or %>% clean_names()

# questi sono i residenti per comune di tutta italia al 2020
residenti_comune <- residenti_comune %>% 
  filter(!grepl("IT", itter107)) %>% #tolgo i dati nazionali 
  filter(tipo_dato15 == "BEG" & 
           time == "2020" &
           sexistat1 == 9)

# questi i residenti per comune per età solo delle marche
residenti_eta_or <- import("data/socio-demo/pop_eta_marche_2020.csv")
residenti_eta <- residenti_eta_or %>% clean_names()

residenti_eta1 <- residenti_eta %>% 
  filter(!grepl("IT", itter107)) %>% 
  filter(statciv2 == "99")

comuni <- residenti_eta1 %>% 
  group_by(itter107) %>% 
  summarise(nome = first(territorio))

# Le province sono 5: Ancona, Ascoli Piceno, Fermo, Macerata, Pesaro e Urbino
# A seconda delle prime 3 cifre del codice comune posso attribuire la provincia 
