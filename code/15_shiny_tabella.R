# App con due mappe a confronto ####

library(tidyverse)
library(magrittr)
library(dplyr)
library(rio)

db_tab <- import("db_tab.csv")



# definisci un vettore con le etichette delle colonne
col_labels <- c(
  "territorio" = "Comune",
  "popolazione" = "Popolazione",
  "reddito_2019" = "Reddito 2019",
  "over65" = "Anziani Over 65",
  "over80" = "Anziani Over 80",
  "perc_65" = "Percentuale Anziani Over 65",
  "perc_80" = "Percentuale Anziani Over 80",
  "adi_utenti" = "Utenti ADI",
  "adi_spesa_tot" = "Spesa Totale ADI",
  "adi_spesa_utente" = "Spesa per Utente ADI",
  "adi_spesa_anziano" = "Spesa per Anziano ADI",
  "sad_utenti" = "Utenti SAD",
  "sad_spesa_tot" = "Spesa Totale SAD",
  "sad_spesa_utente" = "Spesa per Utente SAD",
  "sad_spesa_anziano" = "Spesa per Anziano SAD",
  "rsa_utenti" = "Utenti RSA",
  "rsa_spesa_tot" = "Spesa Totale RSA",
  "rsa_spesa_utente" = "Spesa per Utente RSA",
  "rsa_spesa_anziano" = "Spesa per Anziano RSA",
  "spesa_tot" = "Spesa sanità territoriale",
  "ricoveri_pat" = "Ricoveri associati a NCDs",
  "perc_ricoveri" = "Percentuale di Ricoveri associati a NCDs",
  "ricoveri_pat_c" = "Costo Ricoveri associati a NCDs",
  "perc_ricoveri_c" = "Percentuale di spesa Ricoveri associati a NCDs"
)


library(shiny)
library(DT)

# Interfaccia Utente
ui <- fluidPage(
  titlePanel("Visualizzazione dati dei Comuni della Regione Marche"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("show_vars", 
                         "Colonne da visualizzare", 
                         choices = col_labels,
                         selected = col_labels[c("territorio", "popolazione", "over65", "spesa_tot", "perc_ricoveri", "perc_ricoveri_c")]),
      textInput("search_box", "Cerca comune", value = "")
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)


# Server
server <- function(input, output) {
  output$table <- renderDT({
    df_filtered <- db_tab[grepl(input$search_box, db_tab$territorio, ignore.case = TRUE),]
    df_filtered <- df_filtered[order(-df_filtered$popolazione), ]
    
    # Mappa le etichette delle colonne selezionate ai nomi originali delle colonne
    selected_vars <- names(col_labels)[col_labels %in% input$show_vars]
    df_filtered <- df_filtered[, selected_vars, drop = FALSE]
    
    # Cambia i nomi delle colonne usando le etichette
    names(df_filtered) <- col_labels[names(df_filtered)]
    
    datatable(df_filtered,
              options = list(pageLength = 25, autoWidth = TRUE),
              rownames = FALSE)
  })
}

# Crea l'app Shiny
shinyApp(ui = ui, server = server)
