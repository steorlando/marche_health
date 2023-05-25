# App con due mappe a confronto ####
library(shiny)
library(tmap)

# Define UI
ui <- fluidPage(
  titlePanel("Comuni della regione Marche"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gradient_var1", 
                  label = "Variable for color gradient for Map 1:", 
                  choices = c("% anziani over 65" = "perc_65",
                              "% anziani over 80" = "perc_80", "ricoveri_totali", "perc_ricoveri"), 
                  selected = "perc_65"),
      selectInput("bubble_var1", 
                  label = "Variable for bubble size for Map 1:", 
                  choices = c("None",
                              "Num utenti ADI" =  "adi_utenti", "sad_utenti"), 
                  selected = "None"),
      selectInput("gradient_var2", 
                  label = "Variable for color gradient for Map 2:", 
                  choices = c("% anziani over 65" = "perc_65",
                              "% anziani over 80" = "perc_80", "ricoveri_totali", "perc_ricoveri"), 
                  selected = "perc_65"),
      selectInput("bubble_var2", 
                  label = "Variable for bubble size for Map 2:", 
                  choices = c("None",
                              "Num utenti ADI" =  "adi_utenti", "sad_utenti"), 
                  selected = "None"),
      selectInput("popup_vars", 
                  label = "Variables for popup:", 
                  choices = c("% over 65" = "prop65_map", "popolazione", "over65", "adi_utenti", "sad_utenti"), 
                  selected = c("% over 65" = "prop65_map", "popolazione"), 
                  multiple = TRUE)
    ),
    
    mainPanel(
      splitLayout(
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
                  style = "pretty", 
                  palette = "Blues", 
                  title = paste0("Percentuale ", input$gradient_var1, " su popolazione"),
                  border.lwd = 0.5,
                  popup.vars = input$popup_vars) +
      tm_shape(db_map) +
      tm_bubbles(input$bubble_var1, col = "red", size = input$bubble_var1, scale = 1.5, alpha = 0.5) +
      tm_shape(italy_province) +
      tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) +
      tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5)
  })
  
  output$map2 <- renderTmap({
    tmap_mode("view")
    tm_shape(db_map) +
      tm_polygons(input$gradient_var2, 
                  style = "pretty", 
                  palette = "Blues", 
                  title = paste0("Percentuale ", input$gradient_var2, " su popolazione"),
                  border.lwd = 0.5,
                  popup.vars = input$popup_vars) +
      tm_shape(db_map) +
      tm_bubbles(input$bubble_var2, col = "red", size = input$bubble_var2, scale = 1.5, alpha = 0.5) +
      tm_shape(italy_province) +
      tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) +
      tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


