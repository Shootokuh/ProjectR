library(shiny)
library(dplyr)
library(leaflet)



allDataFrameNB <- readRDS("allDataFrameNB.RDS")
allDataFrameProfile <- readRDS("allDataFrameProfile.RDS")
SPATIAL_DATA <- readRDS("Spatial_Data.RDS")
SPATIAL_DATA <- st_transform(SPATIAL_DATA, crs = 4326)

################################################################################
# fonction                                                                     #
################################################################################


generate_nb_valid_day_period <- function(period) {
  dataPerDayPeriod <- allDataFrameNB %>% 
    filter(as.Date(JOUR) >= as.Date(period[1]) & as.Date(JOUR) <= as.Date(period[2])) %>%
    group_by(weekday) %>% summarise(nb_valid = sum(NB_VALD))
  dataPerDayPeriod <- dataPerDayPeriod[order(dataPerDayPeriod$weekday), ]
  
  return(dataPerDayPeriod)
}

generate_categorie_titre_period <- function(period) {
  utilisationTitrePeriod <- allDataFrameNB %>% 
    filter(as.Date(JOUR) >= as.Date(period[1]) & as.Date(JOUR) <= as.Date(period[2]))%>%
    group_by(CATEGORIE_TITRE, WEEK) %>% 
    summarise(nb_valid = mean(NB_VALD))
  
  return(utilisationTitrePeriod)
}

generate_nb_valid_season <- function(period) {
  dataPerSaison <- allDataFrameNB %>% 
    filter(as.Date(JOUR) >= as.Date(period[1]) & as.Date(JOUR) <= as.Date(period[2])) %>% 
    group_by(saison) %>% 
    summarise(nb_valid = sum(NB_VALD))
  
  return(dataPerSaison)
}

create_plots_nb_valid_day <- function(dataRef, dataCompare, period_ref, period_compare) {
  plots_nb_valid_day <- list()
  max_y_nb_valid_day <- max(dataRef$nb_valid, dataCompare$nb_valid)
  
  p <- ggplot(dataRef, aes(x=weekday, y=nb_valid))+
    geom_col(fill = "#0073C2FF") + 
    geom_text(aes(label = nb_valid), vjust = 1.6, color="white") + 
    scale_y_continuous(
      limits = c(0, max_y_nb_valid_day),
      breaks = seq(0, max_y_nb_valid_day, by = max_y_nb_valid_day / 10) 
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(
      title = paste0("Période référence du ", period_ref[1], " au ", period_ref[2])
    )
  plots_nb_valid_day <- append(plots_nb_valid_day, list(p))
  
  p <- ggplot(dataCompare, aes(x=weekday, y=nb_valid))+
    geom_col(fill = "#0073C2FF") + 
    geom_text(aes(label = nb_valid), vjust = 1.6, color="white") + 
    scale_y_continuous(
      limits = c(0, max_y_nb_valid_day),
      breaks = seq(0, max_y_nb_valid_day, by = max_y_nb_valid_day / 10) 
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(
      title = paste0("Période à comparer du ", period_compare[1], " au ", period_compare[2])
    )
  plots_nb_valid_day <- append(plots_nb_valid_day, list(p))
}

create_plots_categorie_titre <- function(dataRef, dataCompare, period_ref, period_compare) {
  plots_categorie_titre <- list()
  max_y_plots_categorie_titre <- max(dataRef$nb_valid, dataCompare$nb_valid)
  
  p <- ggplot(dataRef, aes(x = WEEK, y = nb_valid)) +
    geom_line(aes(color = CATEGORIE_TITRE)) +
    scale_y_continuous(
      limits = c(0, max_y_plots_categorie_titre),
      breaks = seq(0, max_y_plots_categorie_titre, by = max_y_plots_categorie_titre / 10) 
    ) +
    theme_light() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(
      title = paste0("Période référence du ", period_ref[1], " au ", period_ref[2])
    )
  plots_categorie_titre <- append(plots_categorie_titre, list(p))
  
  p <- ggplot(dataCompare, aes(x = WEEK, y = nb_valid)) +
    geom_line(aes(color = CATEGORIE_TITRE)) +
    scale_y_continuous(
      limits = c(0, max_y_plots_categorie_titre) ,
      breaks = seq(0, max_y_plots_categorie_titre, by = max_y_plots_categorie_titre / 10) 
    ) +
    theme_light() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(
      title = paste0("Période à comparer du ", period_compare[1], " au ", period_compare[2])
    )
  plots_categorie_titre <- append(plots_categorie_titre, list(p))
}

create_plots_nb_valid_season <- function(dataRef, dataCompare, period_ref, period_compare) {
  plots_nb_valid_season <- list()
  max_y_nb_valid_season <- max(dataRef$nb_valid, dataCompare$nb_valid)
  
  p <- ggplot(dataRef, aes(x=saison, y=nb_valid))+
    geom_col(fill = "#0073C2FF") +
    geom_text(aes(label = nb_valid), vjust = 1.6, color="white") + 
    scale_y_continuous(
      limits = c(0, max_y_nb_valid_season),
      breaks = seq(0, max_y_nb_valid_season, by = max_y_nb_valid_season / 10) 
    ) +
    theme_minimal() + 
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(
      title = paste0("Période référence du ", period_ref[1], " au ", period_ref[2])
    )
  plots_nb_valid_season <- append(plots_nb_valid_season, list(p))
  
  p <- ggplot(dataCompare, aes(x=saison, y=nb_valid))+
    geom_col(fill = "#0073C2FF") +
    geom_text(aes(label = nb_valid), vjust = 1.6, color="white") + 
    scale_y_continuous(
      limits = c(0, max_y_nb_valid_season),
      breaks = seq(0, max_y_nb_valid_season, by = max_y_nb_valid_season / 10) 
    ) +
    theme_minimal() + 
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(
      title = paste0("Période à comparer du ", period_compare[1], " au ", period_compare[2])
    )
  plots_nb_valid_season <- append(plots_nb_valid_season, list(p))
}

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
                             choices = c("Station ferrée / Val", "Station de métro", "Arrêt de tram")),
               ),
               mainPanel(
                 fluidRow(
                   column(12, h1(textOutput("infos"), style = "text-align: center; margin-bottom: 20px;"))
                 ),
                 fluidRow(
                   column(12, leafletOutput("map", height = "350px"))
                 ),
                 fluidRow(
                   column(12, verbatimTextOutput("click_info"))
                 )
               )
             )
    ),
    tabPanel("Comparaison des Périodes",
             flowLayout(
               dateRangeInput("period_ref",
                              label = "Période référence :",
                              start = "2018-01-01",
                              end = "2023-06-30",
                              min = "2018-01-01",
                              max = "2023-06-30",
                              format = "mm/dd/yy",
                              separator = " / "),
               
               dateRangeInput("period_compare",
                              label = "Période à comparer :",
                              start = "2018-01-01",
                              end = "2023-06-30",
                              min = "2018-01-01",
                              max = "2023-06-30",
                              format = "mm/dd/yy",
                              separator = " / ")),
             
             fluidRow(
               column(4,
                      verticalLayout(
                        actionButton("graph_2", "Nombre de validations par jour", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px; width: 485px;"),
                        actionButton("graph_3", "Type d'utilisation de titre par semaine en moyenne", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px;"),
                        actionButton("graph_4", "Nombre de validations par saison", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px; width: 485px;"),
                      )
               ),
             ),
             fluidRow(
               column(12,
                      plotOutput("graph_ref", height = "500px", width = "100%")
               )
             )
    ),
    tags$head(
      tags$style(HTML("
      #gridPlot, #gridPlotWeekMean {
        max-width: 100%;
      }
      .btn {
        font-size: 20px;
      }
    "))
    )
  )
)

#  Serveur
server <- function(input, output) {
################################################################################
# Filtrage des données reactives                                               #
################################################################################
   filtered_data <- reactive({
     dep_select <- input$dep
     type_arret_select <- input$type_arret
     
     SPATIAL_DATA %>%
       filter(substr(commune, 1, 2) == dep_select, type_arret == type_arret_select)
   })
  
################################################################################
# Gestion des données après clic sur la carte                                  #
################################################################################
   click_data <- reactive({
     click <- input$map_shape_click
     if (is.null(click)) return(NULL)
    
     id_refa_click <- SPATIAL_DATA %>%
       filter(nom == click$id) %>%
       pull(ID_REFA_LDA)
    
     dataProfile <- allDataFrameProfile %>% filter(ID_REFA_LDA == id_refa_click)
     print(id_refa_click)
     dataNB <- allDataFrameNB %>% filter(ID_REFA_LDA == id_refa_click)
    
     heure_pointe <- dataProfile %>%
       group_by(TRNC_HORR_60) %>%
       summarise(total = sum(pourc_validations), .groups = "drop") %>%
       slice_max(total, n = 1)
    
     categorie_dominante <- dataNB %>%
       group_by(CATEGORIE_TITRE) %>%
       summarise(total = sum(NB_VALD), .groups = "drop") %>%
       slice_max(total, n = 1)
    
     valid_mean <- dataNB %>% summarise(nb_valid = mean(NB_VALD, na.rm = TRUE))
    
     list(
       heure_pointe = heure_pointe,
       categorie_dominante = categorie_dominante,
       click_id = click$id,
       valid_mean = valid_mean
     )
   })
  
################################################################################
# Affichage de la carte Leaflet                                                #
################################################################################
   output$map <- renderLeaflet({
     data <- filtered_data()
    
     if (nrow(data) == 0) {
       leaflet() %>%
         addTiles() %>%
         addPopups(
           lng = 2.3522, lat = 48.8566,
           popup = "Aucun arrêt de ce type dans ce département"
         )
     } else {
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
  
################################################################################
# Affichage des informations après clic                                        #
################################################################################
   output$click_info <- renderText({
     click_info <- click_data()
     
     if (is.null(click_info)) return("Cliquez sur un élément de la carte.")
     
     paste(
       "Vous avez cliqué sur :", click_info$click_id,
       "\nHeure de pointe :", click_info$heure_pointe$TRNC_HORR_60,
       "\nCatégorie dominante :", click_info$categorie_dominante$CATEGORIE_TITRE,
       "\nNombre de validations moyen :", click_info$valid_mean$nb_valid
     )
   })
  
################################################################################
# Graphes à comparer                                                           #
################################################################################
  
  render_graph <- function(graph_id, data_function, plot_function) {
    observeEvent(input[[graph_id]], {
      output$graph_ref <- renderPlot({
        dataRef <- data_function(input$period_ref)
        dataCompare <- data_function(input$period_compare)
        plots_list <- plot_function(dataRef, dataCompare, input$period_ref, input$period_compare)
        
        wrap_plots(plots_list, ncol = 2, nrow = 1)
      })
    })
  }
  
  #  Définition des observers pour chaque graphe
  render_graph("graph_2", generate_nb_valid_day_period, create_plots_nb_valid_day)
  render_graph("graph_3", generate_categorie_titre_period, create_plots_categorie_titre)
  render_graph("graph_4", generate_nb_valid_season, create_plots_nb_valid_season)

}

#  Lancer l'application Shiny
shinyApp(ui = ui, server = server)