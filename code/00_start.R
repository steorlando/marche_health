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
               tmap           # per generare mappe
               )           

rm(list = ls())


source(here::here("code","06_mappe_base.R")) 
source(here::here("code","03b_dati_demografici.R"))
source(here::here("code","05_db_mappe.R"))


