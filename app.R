library(shiny)

library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)



spatial_data = readRDS("Spatial_data.RDS")

spatial_data <- st_transform(spatial_data, crs = 4326)

spatial_data <- spatial_data %>% filter(grepl("^77", spatial_data$commune)) #A RETIRER

#coords <- st_coordinates(spatial_data)

#coords_df <- data.frame(lon = coords[,1], lat = coords[,2])
#coords_df <- head(coords_df, 8)

ui <- fluidPage(
  
  # Application title
  titlePanel("Ridership in Île-de-France Rail Network dashboard"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("type_arret",
                  label = "Sélectionner un type d'arrêt : ",
                  choices = c("Arrêt de bus", "Station de métro", "Arrêt de tram", "Station ferrée / Val"),
      ),
      dateRangeInput("period_ref",
                     label = "Période référence :",
                     start  = "2018-01-01",
                     end    = "2023-12-31",
                     min    = "2018-01-01",
                     max    = "2023-12-31",
                     format = "mm/dd/yy",
                     separator = " / "),
      
      dateRangeInput(
        "compare_period",
        label = "Periode à comparer :",
        start = "2018-01-01",
        end    = "2023-12-31",
        min    = "2018-01-01",
        max    = "2023-12-31",
        format = "mm/dd/yy",
        separator = " / " 
      ),
      
      
      
    ),
    
    #actionButton("compare", label ="Compare"),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      h1(textOutput("infos")),leafletOutput("map"),verbatimTextOutput("click_info") 
    )
  )
)

# Serveur
server <- function(input, output) {
  # Créer une sortie basée sur l'entrée utilisateur
  output$result <- renderText({
    paste("Vous avez choisi le nombre :", input$num)
  })
  
  filtered_data <- reactive({
    type_arret_select <- input$type_arret
    
    filtered = spatial_data %>% filter(spatial_data$type_arret == type_arret_select)
    
    return(filtered)
  })
  
  output$map = renderLeaflet({
    
    data = filtered_data()
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        weight = 4,
        color = "blue",
        label = data$nom,
        layerId = data$nom
        
      )
     
  })
  
  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    
    message <- paste("Vous avez cliqué sur :", click$id)
    
    output$click_info <- renderText(
      {
        message
      }
    )
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)

