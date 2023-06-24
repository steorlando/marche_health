# App con due mappe a confronto ####
load("my_work_space.RData")

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
