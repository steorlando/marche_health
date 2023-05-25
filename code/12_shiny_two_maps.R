# App con due mappe a confronto ####
load("my_work_space.RData")

db_map <- db
db_map$None <- 0 

library(shiny)
library(tmap)

# Define UI
ui <- fluidPage(
  titlePanel("Comuni della regione Marche"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gradient_var1", 
                  label = "Variabile da mappare (colore) Mappa 1:", 
                  choices = c("% anziani over 65" = "perc_65",
                              "% anziani over 80" = "perc_80", 
                              "Ricoveri associati a NCDs" = "ricoveri_pat",
                              "% ricoveri associati a NCDs" = "perc_ricoveri"), 
                  selected = "perc_65"),
      selectInput("bubble_var1", 
                  label = "Variabile bubbles Mappa 1:", 
                  choices = c("None",
                              "Popolazione" = "popolazione",
                              "Anziani > 65" = "over65",
                              "Ricoveri associati a NCDs" = "ricoveri_pat",
                              "Num utenti ADI" = "adi_utenti",
                              "Num utenti SAD" = "sad_utenti"),  
                  selected = "None"),
      selectInput("gradient_var2", 
                  label = "Variabile da mappare (colore) Mappa 2:", 
                  choices = c("% anziani over 65" = "perc_65",
                              "% anziani over 80" = "perc_80", 
                              "Ricoveri associati a NCDs" = "ricoveri_pat",
                              "% ricoveri associati a NCDs" = "perc_ricoveri"),  
                  selected = "perc_ricoveri"),
      selectInput("bubble_var2", 
                  label = "Variabile bubbles Mappa 1:", 
                  choices = c("None",
                              "Popolazione" = "popolazione",
                              "Anziani > 65" = "over65",
                              "Ricoveri associati a NCDs" = "ricoveri_pat",
                              "Num utenti ADI" = "adi_utenti",
                              "Num utenti SAD" = "sad_utenti"),  
                  selected = "None"),
      selectInput("popup_vars", 
                  label = "Variabili per popup:", 
                  choices = c("% anziani over 65" = "perc_65",
                              "% anziani over 80" = "perc_80", 
                              "Ricoveri associati a NCDs" = "ricoveri_pat",
                              "% ricoveri associati a NCDs" = "perc_ricoveri",
                              "Popolazione" = "popolazione",
                              "Anziani > 65" = "over65"), 
                  selected = c("popolazione", "over65", "ricoveri_pat", "perc_ricoveri" ), 
                  multiple = TRUE)
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
    tm_shape(db_map) +
      tm_polygons(input$gradient_var1, 
                  style = "equal", 
                  palette = "Blues", 
                  #title = paste0("Confronto variabili"),
                  border.lwd = 0.5,
                  popup.vars = input$popup_vars) +
      tm_shape(db_map) +
      tm_bubbles(input$bubble_var1, col = "red", size = input$bubble_var1, scale = 1.5, alpha = 0.5) +
      tm_shape(italy_province) +
      tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) +
      tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5)+
      tm_view(view.legend.position = c("left", "bottom"))
  })
  
  output$map2 <- renderTmap({
    tmap_mode("view")
    tm_shape(db_map) +
      tm_polygons(input$gradient_var2, 
                  style = "equal", 
                  palette = "Blues", 
                 # title = paste0("Percentuale ", input$gradient_var2, " su popolazione"),
                  border.lwd = 0.5,
                  popup.vars = input$popup_vars) +
      tm_shape(db_map) +
      tm_bubbles(input$bubble_var2, col = "red", size = input$bubble_var2, scale = 1.5, alpha = 0.5) +
      tm_shape(italy_province) +
      tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) +
      tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5) +
      tm_view(view.legend.position = c("left", "bottom"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


