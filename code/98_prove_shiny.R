library(sf)
db <- sf::st_read("db.geojson")

db_map <- db
db_map$None <- 0 

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Comuni della regione Marche"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gradient_var1", 
                  label = "Variable for color gradient for Map 1:", 
                  choices = c("% anziani over 65" = "perc_65",
                              "% anziani over 80" = "perc_80", 
                              "Ricoveri associati a NCDs" = "ricoveri_pat",
                              "% ricoveri associati a NCDs" = "perc_ricoveri"), 
                  selected = "perc_65"),
      selectInput("bubble_var1", 
                  label = "Variable for bubble size for Map 1:", 
                  choices = c("None",
                              "Popolazione" = "popolazione",
                              "Anziani > 65" = "over65",
                              "Ricoveri associati a NCDs" = "ricoveri_pat",
                              "Num utenti ADI" = "adi_utenti",
                              "Num utenti SAD" = "sad_utenti"),  
                  selected = "None"),
      selectInput("gradient_var2", 
                  label = "Variable for color gradient for Map 2:", 
                  choices = c("% anziani over 65" = "perc_65",
                              "% anziani over 80" = "perc_80", 
                              "Ricoveri associati a NCDs" = "ricoveri_pat",
                              "% ricoveri associati a NCDs" = "perc_ricoveri"),  
                  selected = "perc_ricoveri"),
      selectInput("bubble_var2", 
                  label = "Variable for bubble size for Map 2:", 
                  choices = c("None",
                              "Popolazione" = "popolazione",
                              "Anziani > 65" = "over65",
                              "Ricoveri associati a NCDs" = "ricoveri_pat",
                              "Num utenti ADI" = "adi_utenti",
                              "Num utenti SAD" = "sad_utenti"),  
                  selected = "None"),
      selectInput("popup_vars", 
                  label = "Variables for popup:", 
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
        tmap::tmapOutput("map1"),
        tmap::tmapOutput("map2")
      )
    )
  )
)
  
  # Define server logic
  server <- function(input, output) {
    output$map1 <- renderTmap({
      tmap::tmap_mode("view")
      tmap::tm_shape(db_map) +
        tmap::tm_polygons(input$gradient_var1, 
                          style = "equal", 
                          palette = "Blues", 
                          border.lwd = 0.5,
                          popup.vars = input$popup_vars) +
        tmap::tm_shape(db_map) +
        tmap::tm_bubbles(input$bubble_var1, col = "red", size = input$bubble_var1, scale = 1.5, alpha = 0.5) +
        tmap::tm_shape(italy_province) +
        tmap::tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) +
        tmap::tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5)+
        tmap::tm_view(view.legend.position = c("left", "bottom"))
    })
    
    output$map2 <- renderTmap({
      tmap::tmap_mode("view")
      tmap::tm_shape(db_map) +
        tmap::tm_polygons(input$gradient_var2, 
                          style = "equal", 
                          palette = "Blues", 
                          border.lwd = 0.5,
                          popup.vars = input$popup_vars) +
        tmap::tm_shape(db_map) +
        tmap::tm_bubbles(input$bubble_var2, col = "red", size = input$bubble_var2, scale = 1.5, alpha = 0.5) +
        tmap::tm_shape(italy_province) +
        tmap::tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) +
        tmap::tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5) +
        tmap::tm_view(view.legend.position = c("left", "bottom"))
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  