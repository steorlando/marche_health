# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")
if (!require("tmap")) install.packages("tmap")

# Load contributed packages with pacman
pacman::p_load(magrittr,      # Pipes
               pacman,        # Load/unload packages
               rio,           # Import/export data
               tidyverse,
               flextable,
               janitor,
               here,
               lubridate,
               knitr,
               sjmisc,
               sf,             # per importare dati geospaziali
               tmap,           # per generare mappe
               skimr,
               sjlabelled,
               leaflet,
               scales,
               gtsummary,
               shiny,
               broom,
               purrr,
               ggrepel,
               stringr
               )        

rm(list = ls())

#Sys.setlocale("LC_NUMERIC", "it_IT")



source(here::here("code","06_funzioni_per_mappe.R")) # ci lavora stefano
source(here::here("code","03b_dati_demografici.R"))
source(here::here("code","05_db_mappe.R")) # codice per preaprare il db principale
source(here::here("code","05b_db_mappe_sdo.R")) # sistemo i dati elaborati da paolo su SDO
source(here::here("code","05c_db_mappe_socio.R"))
source(here::here("code","13_regressione.R"))
source(here::here("code","07_dati_farma.R"))
source(here::here("code","10_dati_report.R")) 


db_tab <- db %>% 
  select(territorio,
         popolazione,
         reddito_2019,
         over65,
         over80,
         perc_65,
         perc_80,
         adi_utenti,
         adi_spesa_tot,
         adi_spesa_utente,
         adi_spesa_anziano,
         sad_utenti,
         sad_spesa_tot,
         sad_spesa_utente,
         sad_spesa_anziano,
         rsa_utenti,
         rsa_spesa_tot,
         rsa_spesa_utente,
         rsa_spesa_anziano,
         spesa_tot,
         ricoveri_pat,
         perc_ricoveri,
         ricoveri_pat_c,
         perc_ricoveri_c
  )

# Ottenere i nomi dei campi numerici con valori massimi inferiori a 1
campi_da_modificare <- names(db_tab)[sapply(db_tab, is.numeric) & sapply(db_tab, function(x) max(x, na.rm = TRUE) < 1)]

# Utilizzare la funzione sapply per applicare una funzione a ciascuno dei campi specificati
db_tab[, campi_da_modificare] <- sapply(db_tab[, campi_da_modificare], function(x) round(x*100, 2))


export(db_tab, "code/db_tab.csv")

# Save image ####
#save.image (file = "code/my_work_space.RData")

# Sys.setlocale("LC_NUMERIC", "C") # quando faccio il report lo cambio in italiano. Questo è per tornare all'inglese. 

