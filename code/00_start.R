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
               broom
               )        

rm(list = ls())

#Sys.setlocale("LC_NUMERIC", "it_IT")



source(here::here("code","06_funzioni_per_mappe.R")) # ci lavora stefano
source(here::here("code","03b_dati_demografici.R"))
source(here::here("code","05_db_mappe.R")) # codice per preaprare il db principale
source(here::here("code","05b_db_mappe_sdo.R")) # sistemo i dati elaborati da paolo su SDO
source(here::here("code","05c_db_mappe_socio.R"))
source(here::here("code","10_dati_report.R")) 


# Save image ####
#save.image (file = "code/my_work_space.RData")

#Sys.setlocale("LC_NUMERIC", "C") # quando faccio il report lo cambio in italiano. Questo è per tornare all'inglese. 

