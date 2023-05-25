db_map <- db %>% 
  mutate(None = 0)

library(shiny)
library(tmap)

# Define UI
ui <- fluidPage(
  titlePanel("Comuni della regione Marche"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gradient_var", 
                  label = "Variable for color gradient:", 
                  choices = c("% anziani over 65" = "perc_65",
                              "% anziani over 80" = "perc_80", "ricoveri_totali", "perc_ricoveri"), 
                  selected = "perc_65"),
      selectInput("bubble_var", 
                  label = "Variable for bubble size:", 
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
      tmapOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$map <- renderTmap({
    tmap_mode("view")
    tm_shape(db_map) +
      tm_polygons(input$gradient_var, 
                  style = "pretty", 
                  palette = "Blues", 
                  title = paste0("Percentuale ", input$gradient_var, " su popolazione"),
                  border.lwd = 0.5,
                  popup.vars = input$popup_vars) +
      tm_shape(db_map) +
      tm_bubbles(input$bubble_var, col = "red", size = input$bubble_var, scale = 1.5, alpha = 0.5) +
      tm_shape(italy_province) +
      tm_borders(lwd = 1.5, col = "darkgreen", alpha = 0.5) +
      tm_layout(main.title = "Comuni della regione Marche", main.title.size = 1.5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

