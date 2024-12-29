library(shiny)

library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)

#unique(substr(SPATIAL_DATA$commune,1,2)) = departement unique 
# -> "95" "78" "77" "94" "91" "75" "93" "92" "60" "45" "27" "28" "89" "10" "51" "02"
# 60 45 27 28 89 10 51 02 = pas en ile de france -> retirer de spatial data (on peut continuer avec en attendant)
# Faire une liste de choix entre ces départements ?


spatial_data = readRDS("Spatial_data.RDS")

annual_profil_fer_2022 = readRDS("Annual_Profil_Fer_2022.RDS")
annual_nb_fer_2022 = readRDS("Annual_Nb_Fer_2022.RDS")

spatial_data <- st_transform(spatial_data, crs = 4326)

#spatial_data <- spatial_data %>% filter(grepl("^77", spatial_data$commune)) #A RETIRER


#coords <- st_coordinates(spatial_data)

#coords_df <- data.frame(lon = coords[,1], lat = coords[,2])
#coords_df <- head(coords_df, 8)

ui <- fluidPage(
  # Titre principal au-dessus de la barre
  titlePanel("Fréquentation Réseau ferré - Île-de-France"),
  
  navbarPage(
    title = NULL,
    tabPanel("Tendances par arrêt",
      sidebarLayout(
        sidebarPanel(
          selectInput("dep",
                      label = "Sélectionner un département : ",
                      choices = c("75", "77", "78", "95", "94", "93", "92", "91")),
          
          selectInput("type_arret",
                      label = "Sélectionner un type d'arrêt : ",
                      choices = c("Station ferrée / Val", "Station de métro", "Arrêt de tram","Arrêt de bus")),
        ),
        mainPanel(
          h1(textOutput("infos")),
          leafletOutput("map"),
          verbatimTextOutput("click_info")
        )
      )
    ),
    tabPanel("Tendances",
             dateRangeInput("period_ref",
                            label = "Période référence :",
                            start = "2018-01-01",
                            end = "2023-12-31",
                            min = "2018-01-01",
                            max = "2023-12-31",
                            format = "mm/dd/yy",
                            separator = " / "),
             
             dateRangeInput("compare_period",
                            label = "Période à comparer :",
                            start = "2018-01-01",
                            end = "2023-12-31",
                            min = "2018-01-01",
                            max = "2023-12-31",
                            format = "mm/dd/yy",
                            separator = " / "),
             verticalLayout(
               actionButton("graph_1", "Graph 1", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px;"),
               actionButton("graph_2", "Graph 2", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px;"),
               actionButton("graph_3", "Graph 3", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px;"),
               actionButton("graph_4", "Graph 4", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px;"),
               actionButton("graph_5", "Graph 5", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px;"),
             )
             ),
    tabPanel("Tendances annuelles")
  )
)


# Serveur
server <- function(input, output) {
  filtered_data <- reactive({
    type_arret_select <- input$type_arret
    dep_select <- input$dep
    
    filtered <- spatial_data %>% filter(substr(spatial_data$commune, 1, 2) == dep_select)
    final_data <- filtered %>% filter(filtered$type_arret == type_arret_select)

    return(final_data)
  })
  
  output$map = renderLeaflet({
    
    data = filtered_data()
    
    if (nrow(data) == 0) {
      leaflet() %>%
        addTiles() %>%
        addPopups(
          lng = 2.3522, lat = 48.8566,
          popup = "Aucun arrêt de ce type dans ce département"
        )
    }
    else {
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(
          weight = 4,
          color = "blue",
          label = data$nom,
          layerId = data$nom
          
        )
    }
     
  })
  
  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    
    spatial_data_filtered <- spatial_data %>% filter(nom == click$id)
    
    heure_pointe <- left_join(spatial_data_filtered, annual_profil_fer_2022, by = c("ID_REFA_LDA" = "ID_REFA_LDA")) %>%
      group_by(TRNC_HORR_60) %>%
      summarise(total = sum(pourc_validations)) %>%
      arrange(desc(total)) %>%
      head(1)
    
    categorie_dominante <- left_join(spatial_data_filtered, annual_nb_fer_2022, by = c("ID_REFA_LDA" = "ID_REFA_LDA")) %>%
      group_by(CATEGORIE_TITRE) %>%
      summarise(total = sum(NB_VALD)) %>%
      arrange(desc(total)) %>%
      head(1)
    
    message <- paste("Vous avez cliqué sur : ", click$id, 
                     "\n Heure de pointe : ", heure_pointe$TRNC_HORR_60,
                     "\n Catégorie dominante : ", categorie_dominante$CATEGORIE_TITRE)
    
    output$click_info <- renderText(
      {
        message
      }
    )
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)

