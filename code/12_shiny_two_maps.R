# App con due mappe a confronto ####
load("my_work_space.RData")

db_map$None <- 0 

library(shiny)
library(tmap)

variabili_col <- c("% anziani over 65" = "perc_65",
                   "% anziani over 80" = "perc_80", 
                   "Ricoveri associati a NCDs" = "ricoveri_pat",
                   "% ricoveri associati a NCDs" = "perc_ricoveri",
                   "Costo ricoveri NCDs" = "ricoveri_pat_c", 
                   "Propozione costo ricoveri NCDs" = "perc_ricoveri_c",
                   "Spesa RSA" = "rsa_spesa_tot",
                   "Spesa per anziano" = "spesa_citt65",
                   "Spesa per utente" = "spesa_utente",
                   "Ricoveri NCDs pesati per disabilità" = "ricoveri_w",
                   "Peso in salute di ricoveri NCDs a cittadino" = "perc_ricoveri_w",
                   "Num utenti ADI" = "adi_utenti",
                   "Num utenti SAD" = "sad_utenti"
)

variabili_bubble <- c("None",
                      "Popolazione" = "popolazione",
                      "Anziani > 65" = "over65",
                      "Ricoveri associati a NCDs" = "ricoveri_pat",
                      "Costo ricoveri NCDs" = "ricoveri_pat_c", 
                      "Num utenti ADI" = "adi_utenti",
                      "Num utenti SAD" = "sad_utenti",
                      "Anziani in RSA" = "rsa_utenti",
                      "Spesa RSA" = "rsa_spesa_tot",
                      "Spesa per anziano" = "spesa_citt65",
                      "Spesa per utente" = "spesa_utente",
                      "Ricoveri NCDs pesati per disabilità" = "ricoveri_w",
                      "Peso in salute di ricoveri NCDs a cittadino" = "perc_ricoveri_w")

variabili_pop <- c("% anziani over 65" = "perc_65",
                   "% anziani over 80" = "perc_80", 
                   "Ricoveri associati a NCDs" = "ricoveri_pat",
                   "% ricoveri associati a NCDs" = "perc_ricoveri",
                   "Costo ricoveri NCDs" = "ricoveri_pat_c", 
                   "Propozione costo ricoveri NCDs" = "perc_ricoveri_c",
                   "Popolazione" = "popolazione",
                   "Anziani > 65" = "over65",
                   "Anziani in RSA" = "rsa_utenti",
                   "Spesa RSA" = "rsa_spesa_tot",
                   "Spesa per anziano" = "spesa_citt65",
                   "Spesa per utente" = "spesa_utente",
                   "Peso in salute di ricoveri NCDs a cittadino" = "perc_ricoveri_w")


# Define UI
ui <- fluidPage(
  titlePanel("Comuni della regione Marche"),
  em("Versione mappa aggiornata il 29 maggio"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gradient_var1", 
                  label = "Variabile da mappare (colore) Mappa 1:", 
                  choices = variabili_col, 
                  selected = "perc_65"),
      selectInput("bubble_var1", 
                  label = "Variabile bubbles Mappa 1:", 
                  choices = variabili_bubble,  
                  selected = "None"),
      selectInput("gradient_var2", 
                  label = "Variabile da mappare (colore) Mappa 2:", 
                  choices = variabili_col,  
                  selected = "perc_ricoveri"),
      selectInput("bubble_var2", 
                  label = "Variabile bubbles Mappa 1:", 
                  choices = variabili_bubble,  
                  selected = "None"),
      selectInput("popup_vars", 
                  label = "Variabili per popup:", 
                  choices = variabili_pop, 
                  selected = c("popolazione", "over65", "ricoveri_pat", "perc_ricoveri"), 
                  multiple = TRUE),
      sliderInput("population_range", 
                  label = "Popolosità dei comuni da visualizzare:", 
                  min = 0, 
                  max = max(db$popolazione, na.rm = TRUE), 
                  value = c(2000, max(db$popolazione, na.rm = TRUE)))
    ),
    
    mainPanel(
      verticalLayout(
        tmapOutput("map1"),
        tmapOutput("map2")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$map1 <- renderTmap({
    tmap_mode("view")
    db_map_filtered <- db_map[db_map$popolazione >= input$population_range[1] & db_map$popolazione <= input$population_range[2], ]
    tm_shape(db_map_filtered) +
      tm_polygons(input$gradient_var1, 
                  style = "equal", 
                  palette = "Blues", 
                  border.lwd = 0.5,
                  popup.vars = input$popup_vars) +
      tm_shape(db_map_filtered) +
      tm_bubbles(input$bubble_var1, col = "red", size = input$bubble_var1, scale = 1.5, alpha = 0.5) +
      tm_shape(italy_province) +
      tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) +
      tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5)+
      tm_view(view.legend.position = c("left", "bottom"))
  })
  
  output$map2 <- renderTmap({
    tmap_mode("view")
    db_map_filtered <- db_map[db_map$popolazione >= input$population_range[1] & db_map$popolazione <= input$population_range[2], ]
    tm_shape(db_map_filtered) +
      tm_polygons(input$gradient_var2, 
                  style = "equal", 
                  palette = "Greens", 
                  border.lwd = 0.5,
                  popup.vars = input$popup_vars) +
      tm_shape(db_map_filtered) +
      tm_bubbles(input$bubble_var2, col = "red", size = input$bubble_var2, scale = 1.5, alpha = 0.5) +
      tm_shape(italy_province) +
      tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) +
      tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5) +
      tm_view(view.legend.position = c("left", "bottom"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
