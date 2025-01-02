library(shiny)

library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)

#spatial_data = readRDS("Spatial_data.RDS")

annual_profil_fer_2022 = readRDS("Annual_Profil_Fer_2022.RDS")
annual_nb_fer_2022 = readRDS("Annual_Nb_Fer_2022.RDS")

all_annual_nb_fer = readRDS("all_Annual_NB_Fer.RDS")
SPATIAL_DATA <- st_transform(SPATIAL_DATA, crs = 4326)


generate_nb_valid_period <- function(period) {
  filtered_data <- all_annual_nb_fer %>%
    filter(as.Date(JOUR) >= as.Date(period[1]) & as.Date(JOUR) <= as.Date(period[2]))
  
  nbValidPeriodResult <- filtered_data %>% mutate(
    year = format(as.Date(JOUR), "%Y"),
    month = format(as.Date(JOUR), "%m")) %>%
    group_by(year_month = paste0(year, "-M", month)) %>%
    summarise(nb_valid = sum(NB_VALD), .groups = "drop")
  
  return(nbValidPeriodResult)
}

generate_nb_valid_day_period <- function(period) {
  dataPerDayPeriod <- all_annual_nb_fer %>% 
    filter(as.Date(JOUR) >= as.Date(period[1]) & as.Date(JOUR) <= as.Date(period[2])) %>%
    group_by(weekday) %>% summarise(nb_valid = sum(NB_VALD))
  dataPerDayPeriod <- dataPerDayPeriod[order(dataPerDayPeriod$weekday), ]
  
  return(dataPerDayPeriod)
}

generate_categorie_titre_period <- function(period) {
  utilisationTitrePeriod <- all_annual_nb_fer %>% 
    filter(as.Date(JOUR) >= as.Date(period[1]) & as.Date(JOUR) <= as.Date(period[2]))%>%
    group_by(CATEGORIE_TITRE, WEEK) %>% 
    summarise(nb_valid = sum(NB_VALD))
  
  return(utilisationTitrePeriod)
}

generate_nb_valid_season <- function(period) {
  dataPerSaison <- all_annual_nb_fer %>% 
    filter(as.Date(JOUR) >= as.Date(period[1]) & as.Date(JOUR) <= as.Date(period[2])) %>% 
    group_by(saison) %>% 
    summarise(nb_valid = sum(NB_VALD))
  
  return(dataPerSaison)
}

create_plot_nb_valid <- function(data, title) {
  ggplot(data, aes(x=year_month, y=nb_valid, group = 1)) +
    geom_line(linewidth = 1, color="blue") +
    scale_y_continuous(
      breaks = seq(10000000, max(data$nb_valid) + 50000000, by = 10000000),
      labels = scales::label_comma(scale = 1000)
    ) +
    scale_x_discrete(
      breaks = data$year_month[seq(1, length(data$year_month), by = 3)],
      guide = guide_axis(angle = -45)) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = -75)) +
    ggtitle(title)
}

create_plot_nb_valid_day <- function(data, title) {
  ggplot(data, aes(x=weekday, y=nb_valid))+
    geom_col(fill = "#0073C2FF") + 
    geom_text(aes(label = nb_valid), vjust = 1.6, color="white") + 
    scale_y_continuous(
      breaks = seq(1000000, max(data$nb_valid) + 50000000, by = 50000000),
      labels = scales::label_comma(scale = 1000)
    ) +
    theme_minimal() +
    ggtitle(title)
}

create_plot_categorie_titre <- function(data, title) {
  print(length(data$WEEK[1]))
  ggplot(data, aes(x = WEEK, y = nb_valid)) +
    geom_line(aes(color = CATEGORIE_TITRE), position = position_stack()) +
    scale_x_continuous(
      breaks = data$WEEK[seq(1, length(data$WEEK), by = 3)], # Affiche une semaine sur deux
      labels = data$WEEK[seq(1, length(data$WEEK), by = 3)]  
    ) +
    scale_y_continuous(
      breaks = seq(10000000, max(data$nb_valid) + 5000000000, by = 20000000),
      labels = scales::label_comma(scale = 1000)
    ) +
    theme_light() +
    ggtitle(title)
}

create_plot_nb_valid_season <- function(data, title) {
  ggplot(data, aes(x=saison, y=nb_valid))+
    geom_col(fill = "#0073C2FF") +
    geom_text(aes(label = nb_valid), vjust = 1.6, color="white") + 
    scale_y_continuous(
      breaks = seq(100000000, max(data$nb_valid) + 5000000000, by = 200000000),
      labels = scales::label_comma(scale = 1000)
    ) +
    theme_minimal() + 
    ggtitle(title)
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
          h1(textOutput("infos")),
          leafletOutput("map"),
          verbatimTextOutput("click_info")
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
                        actionButton("graph_1", "Nombre de validations sur tout le réseau", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px; width: 485px;"),
                        actionButton("graph_2", "Nombre de validations par jour", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px; width: 485px;"),
                        actionButton("graph_3", "Type d'utilisation de titre par semaine en moyenne", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px;"),
                        actionButton("graph_4", "Nombre de validations par saison", style = "font-size: 20px; padding: 10px 20px; margin-bottom: 10px; width: 485px;"),
                      )
                      ),
               column(8,
                      fluidRow(
                        column(6,
                          plotOutput("graph_ref", height = "500px", width = "500px")),
                        column(6,
                          plotOutput("graph_compare", height = "500px", width = "500px")))
               )
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
    
    filtered <- SPATIAL_DATA %>% filter(substr(SPATIAL_DATA$commune, 1, 2) == dep_select)
    final_data <- filtered %>% filter(filtered$type_arret == type_arret_select)

    return(final_data)
  })
  
  click_data <- reactive({
    
    click <- input$map_shape_click
    if (is.null(click)) return(NULL)
    spatial_data_filtered <- SPATIAL_DATA %>% filter(nom == click$id)
    
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
    
    return(list(heure_pointe = heure_pointe, categorie_dominante = categorie_dominante, click_id = click$id))
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
  
  output$click_info <- renderText({
    click_info <- click_data()
    
    if (is.null(click_info)) return("Cliquez sur un élément de la carte.")
    
    message <- paste("Vous avez cliqué sur : ", click_info$click_id,
                     "\n Heure de pointe : ", click_info$heure_pointe$TRNC_HORR_60,
                     "\n Catégorie dominante : ", click_info$categorie_dominante$CATEGORIE_TITRE)
    return(message)
  })
  
  observeEvent(input$graph_1, {
    print("EN cours 1")
    print(input$period_compare)
    output$graph_ref <- renderPlot({
      data <- generate_nb_valid_period(input$period_ref)
      create_plot_nb_valid(data,paste("Période référence du ", input$period_ref[1], "au", input$period_ref[2]))
    })
    
    output$graph_compare <- renderPlot({
      data <- generate_nb_valid_period(input$period_compare)
      create_plot_nb_valid(data,paste("Période à comparer du ", input$period_compare[1], "au", input$period_compare[2]))
    })
  })
  
  observeEvent(input$graph_2, {
    print("EN cours 2")
    output$graph_ref <- renderPlot({
      data <- generate_nb_valid_day_period(input$period_ref)
      
      create_plot_nb_valid_day(data,paste("Période référence du ", input$period_ref[1], "au", input$period_ref[2]))
    })
    
    output$graph_compare <- renderPlot({
      data <- generate_nb_valid_day_period(input$period_compare)
      
      create_plot_nb_valid_day(data,paste("Période à comparer du ", input$period_compare[1], "au", input$period_compare[2]))
    })
  })
  
  observeEvent(input$graph_3, {
    
    print("EN cours 3")
    output$graph_ref <-renderPlot({
      
      data <- generate_categorie_titre_period(input$period_ref)
      
      create_plot_categorie_titre(data, paste("Période référence du ", input$period_ref[1], "au", input$period_ref[2]))
    })
    
    output$graph_compare <-renderPlot({
      
      data <- generate_categorie_titre_period(input$period_compare)
      
      create_plot_categorie_titre(data, paste("Période à comparer du ", input$period_compare[1], "au", input$period_compare[2]))
    })
  })
  
  observeEvent(input$graph_4, {
    
    print("EN cours 4")
    output$graph_ref <-renderPlot({
      
      data <- generate_nb_valid_season(input$period_ref)
      
      create_plot_nb_valid_season(data, paste("Période référence du ", input$period_ref[1], "au", input$period_ref[2]))
    })
    
    output$graph_compare <-renderPlot({
      
      data <- generate_nb_valid_season(input$period_compare)
      
      create_plot_nb_valid_season(data, paste("Période à comparer du ", input$period_compare[1], "au", input$period_compare[2]))
    })
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)

