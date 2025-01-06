library(shiny)

library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(patchwork)
library(gridExtra) 
library(plotly)


allDataFrameNB <- readRDS("allDataFrameNB.RDS")
allDataFrameProfile <- readRDS("allDataFrameProfile.RDS")
SPATIAL_DATA <- readRDS("Spatial_Data.RDS")
SPATIAL_DATA <- st_transform(SPATIAL_DATA, crs = 4326)

################################################################################
# fonction                                                                     #
################################################################################

generate_nb_valid_period <- function(period) {
  filtered_data <- allDataFrameNB %>%
    filter(as.Date(JOUR) >= as.Date(period[1]) & as.Date(JOUR) <= as.Date(period[2]))
  
  nbValidPeriodResult <- filtered_data %>% mutate(
    year = format(as.Date(JOUR), "%Y"),
    month = format(as.Date(JOUR), "%m")) %>%
    group_by(year_month = paste0(year, "-M", month)) %>%
    summarise(nb_valid = sum(NB_VALD), .groups = "drop")
  
  return(nbValidPeriodResult)
}

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

create_plots_nb_valid <- function(dataRef, dataCompare, period_ref, period_compare) {
  plots_nb_valid <- list()
  max_y_nb_valid = max(dataRef$nb_valid, dataCompare$nb_valid)
  p <- ggplot(dataRef, aes(x=year_month, y=nb_valid, group = 1)) +
    geom_line(linewidth = 1, color="blue") +
    scale_y_continuous(
      limits = c(0, max_y_nb_valid),
      breaks = seq(0, max_y_nb_valid, by = max_y_nb_valid / 10) 
    ) +
    scale_x_discrete(
      breaks = dataRef$year_month[seq(1, length(dataRef$year_month), by = 3)],
      guide = guide_axis(angle = -45)) +
    labs(
      title = paste0("Période référence du ", period_ref[1], " au ", period_ref[2])
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = -75)
      )
  plots_nb_valid <- append(plots_nb_valid, list(p))
  
  p <- ggplot(dataCompare, aes(x=year_month, y=nb_valid, group = 1)) +
    geom_line(linewidth = 1, color="blue") +
    scale_y_continuous(
      limits = c(0, max_y_nb_valid),
      breaks = seq(0, max_y_nb_valid, by = max_y_nb_valid / 10) 
    ) +
    scale_x_discrete(
      breaks = dataCompare$year_month[seq(1, length(dataCompare$year_month), by = 3)],
      guide = guide_axis(angle = -45)) +
    labs(
      title = paste0("Période à comparer du ", period_compare[1], " au ", period_compare[2])
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = -75)
    )
  plots_nb_valid <- append(plots_nb_valid, list(p))
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
    scale_x_continuous(
      breaks = dataRef$WEEK[seq(1, length(dataRef$WEEK), by = 3)], # Affiche une semaine sur deux
      labels = dataRef$WEEK[seq(1, length(dataRef$WEEK), by = 3)]  
    ) +
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

generate_HV_VS <- function(data) {
  max_y_pourc_valid = max(allDataFrameProfile %>% 
                            group_by(CAT_JOUR, TRNC_HORR_60, YEAR) %>%
                            summarize(n = mean(pourc_validations)) %>%
                            mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60))) %>%
                            pull(n))
  plots_HV_VS <- list()
  for (year in 2018:2022) {
    scolarPeriod = data %>% 
      filter(YEAR == year) %>%
      group_by(CAT_JOUR, TRNC_HORR_60) %>% 
      filter(CAT_JOUR == 'JOVS' | CAT_JOUR == "SAVS") %>%
      summarize(pourc_validations_mean = mean(pourc_validations)) %>%
      mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60)))
    
    taffPeriod = data %>% 
      filter(YEAR == year) %>%
      group_by(CAT_JOUR, TRNC_HORR_60) %>% 
      filter(CAT_JOUR == 'JOHV' | CAT_JOUR == "SAHV") %>%
      summarize(pourc_validations_mean = mean(pourc_validations)) %>%
      mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60))) 
    
    p <- ggplot() +
      geom_line(data = scolarPeriod, aes(x = Heure_Val, y=pourc_validations_mean, 
                                         group=1, color = "Jour Ouvré en Période de Vacances Scolaires")) +
      geom_line(data = taffPeriod, aes(x = Heure_Val, y=pourc_validations_mean, 
                                       group=1, color = "Jour Ouvré Hors Vacances Scolaires")) +
      theme_bw()+
      xlab("Tranche horaire 1H") + ylab("validations / 100")+
      scale_x_continuous(
        breaks = scolarPeriod$Heure_Val, 
        labels = scolarPeriod$TRNC_HORR_60
      ) +
      scale_y_continuous(
        limits = c(0, max_y_pourc_valid),
        breaks = seq(0, max_y_pourc_valid, by = max_y_pourc_valid / 10) 
      ) +
      labs(
        title = paste("Année :", year)
      ) +
      scale_color_brewer("Saison",palette ="Spectral")+
      guides(colour = "legend", size = "legend")+
      scale_color_manual(
        name = "Périodes", # Nom de la légende
        values = c("Jour Ouvré en Période de Vacances Scolaires" = "blue", 
                   "Jour Ouvré Hors Vacances Scolaires" = "red")
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom", # Légende en bas
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotation des labels de l'axe X
      )
    plots_HV_VS <- append(plots_HV_VS, list(p))
  }
  return(plots_HV_VS)
}

generate_Jour <- function(data) {
  max_y_pourc_valid = max(allDataFrameProfile %>% 
                            group_by(CAT_JOUR, TRNC_HORR_60, YEAR) %>%
                            summarize(n = mean(pourc_validations)) %>%
                            mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60))) %>%
                            pull(n))
  plots_Jour <- list()
  for (cat in c("JOVS", "SAVS", "JOHV", "SAHV")) {
    data_filtered <- data %>% 
      group_by(CAT_JOUR, TRNC_HORR_60, YEAR) %>%
      filter(CAT_JOUR == cat) %>%
      summarize(pourc_validations_mean = mean(pourc_validations)) %>%
      mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60)))
    
    p <- ggplot(data_filtered, aes(x = Heure_Val, y = pourc_validations_mean,color = as.factor(YEAR), group = YEAR)) +
      geom_line(linewidth = 1) +
      labs(
        title = cat,
        x = "Tranche horaire",
        y = "% de validations",
        color = "Année"
      ) +
      scale_x_continuous(
        breaks = data$Heure_Val, 
        labels = data$TRNC_HORR_60
      ) +
      scale_y_continuous(
        limits = c(0, max_y_pourc_valid),
        breaks = seq(0, max_y_pourc_valid, by = max_y_pourc_valid / 10) 
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom", # Légende en bas
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = -90, hjust = 1)  # Rotation des labels de l'axe X
      )
    plots_Jour <- append(plots_Jour, list(p))
  }
  return(plots_Jour)
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
             ),
             fluidRow(
               column(12,
                      h3("Pourcentages de validations sur l'ensemble du réseau par année en fonction du type de jour"),
                      plotOutput("gridPlotHV_VS", width = "100%", height = "600px")
               )
             ),
             fluidRow(
               column(12,
                      h3("Pourcentages de validations par tranche horaire"),
                      plotOutput("gridPlotJour", width = "100%", height = "600px")
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
             ),
             fluidRow(
               column(12,
                      plotOutput("graph_ref", height = "500px", width = "100%")
               )
             )
    ),
    tabPanel(
      "Tendances annuelles",
      fluidPage(
        fluidRow(
          column(12,
                 h3("Nombre de validations par année"),
                 plotOutput("gridPlot", width = "100%", height = "300px"),
                 hr()
          )
        ),
        fluidRow(
          column(12,
                 h3("Moyenne de validations par semaine par année"),
                 plotOutput("gridPlotWeekMean", width = "100%", height = "300px"),
                 hr()
          )
        ),
        fluidRow(
          column(12,
                 h3("Moyenne de validations par saison par année"),
                 plotOutput("gridPlotSeasonMean", width = "100%", height = "300px"),
                 hr()
          )
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
    id_refa_click <- spatial_data_filtered %>% select(ID_REFA_LDA) %>% pull(ID_REFA_LDA)
    dataProfile <- allDataFrameProfile %>% filter(ID_REFA_LDA == id_refa_click)
    dataNB <- allDataFrameNB %>% filter(ID_REFA_LDA == id_refa_click)
    
    heure_pointe <- dataProfile %>%
      group_by(TRNC_HORR_60) %>%
      summarise(total = sum(pourc_validations)) %>%
      arrange(desc(total)) %>%
      head(1)
    
    categorie_dominante <- dataNB %>%
      group_by(CATEGORIE_TITRE) %>%
      summarise(total = sum(NB_VALD)) %>%
      arrange(desc(total)) %>%
      head(1)
    
    valid_mean <- dataNB %>% summarise(nb_valid = mean(NB_VALD))
    
    output$gridPlotHV_VS <- renderPlot({
      wrap_plots(generate_HV_VS(dataProfile), ncol = 3, nrow = 2)
    })
    
    output$gridPlotJour <- renderPlot({
      wrap_plots(generate_Jour(dataProfile), ncol = 2, nrow = 2)
    })
    
    return(list(heure_pointe = heure_pointe, categorie_dominante = categorie_dominante, click_id = click$id, valid_mean = valid_mean))
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
                     "\n Catégorie dominante : ", click_info$categorie_dominante$CATEGORIE_TITRE,
                     "\nNombre de validations moyenne : ", click_info$valid_mean)
    return(message)
  })
  
  observeEvent(input$graph_1, {
    print(input$period_compare)
    output$graph_ref <- renderPlot({
      dataRef <- generate_nb_valid_period(input$period_ref)
      dataCompare <- generate_nb_valid_period(input$period_compare)
      plots_list <- create_plots_nb_valid(dataRef, dataCompare, input$period_ref, input$period_compare)
      
      wrap_plots(plots_list, ncol = 2, nrow = 1)
    })
  })
  
  observeEvent(input$graph_2, {
    output$graph_ref <- renderPlot({
      dataRef <- generate_nb_valid_day_period(input$period_ref)
      dataCompare <- generate_nb_valid_day_period(input$period_compare)
      plots_list <- create_plots_nb_valid_day(dataRef, dataCompare, input$period_ref, input$period_compare)
      
      wrap_plots(plots_list, ncol = 2, nrow = 1)
    })
  })
  
  observeEvent(input$graph_3, {
    output$graph_ref <-renderPlot({
      
      dataRef <- generate_categorie_titre_period(input$period_ref)
      dataCompare <- generate_categorie_titre_period(input$period_compare)
      plots_list <- create_plots_categorie_titre(dataRef, dataCompare, input$period_ref, input$period_compare)
      
      wrap_plots(plots_list, ncol = 2, nrow = 1)
    })
  })
  
  observeEvent(input$graph_4, {
    output$graph_ref <-renderPlot({
      
      dataRef <- generate_nb_valid_season(input$period_ref)
      dataCompare <- generate_nb_valid_season(input$period_compare)
      plots_list <- create_plots_nb_valid_season(dataRef, dataCompare, input$period_ref, input$period_compare)
      
      wrap_plots(plots_list, ncol = 2, nrow = 1)
    })
  })
  
  ################################################################################
  # PLOS VALID WEEK                                                              #
  ################################################################################
  
  max_y_dataPerWeek = max(allDataFrameNB %>% 
                            group_by(WEEK, YEAR) %>% 
                            summarise(n = n()) %>% 
                            pull(n), na.rm = TRUE)
  
  plots_valid_week <- list()
  for (year in 2018:2023) {
    dataPerWeek <- allDataFrameNB %>% filter(YEAR == year) %>% 
      group_by(WEEK) %>% 
      summarise(n=n()) %>% 
      arrange(WEEK)
    
    minWeek <- min(dataPerWeek$n, na.rm = TRUE)
    maxWeek <- max(dataPerWeek$n, na.rm = TRUE)
    meanWeek <- mean(dataPerWeek$n, na.rm = TRUE)
    
    seuil <- data.frame(
      yintercept = c(minWeek, maxWeek, meanWeek),
      label = c("min", "max", "mean"),
      color = c("red", "green", "blue")
    )
    
    p <- ggplot(dataPerWeek, aes(x=WEEK,y=n, group = 1)) +
      geom_line() +
      geom_hline(data = seuil, aes(yintercept = yintercept, color = label), linetype = "dashed", linewidth = 0.8) +
      theme_minimal() +
      scale_y_continuous(
        limits = c(0, max_y_dataPerWeek),
        breaks = seq(0, max_y_dataPerWeek, by = max_y_dataPerWeek / 10) 
      ) + 
      labs(
        title = paste("Année :", year)
      ) +
      scale_color_manual(
        name = "Seuils",
        values = setNames(seuil$color, seuil$label)
      ) +
      theme(
        legend.position = "top",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      ) 
    plots_valid_week <- append(plots_valid_week, list(p))
  }
  
  output$gridPlot <- renderPlot({
    wrap_plots(plots_valid_week, ncol = 6, nrow = 1)
  })  
  
  ################################################################################
  # PLOTS WEEK.                                                                  #
  ################################################################################
  
  dataPerDay <- allDataFrameNB %>% group_by(weekday, YEAR) %>% summarise(nb_valid = mean(NB_VALD), .groups = "drop")
  dataPerDay <- dataPerDay[order(dataPerDay$weekday), ]
  
  max_y_dataPerDay <- max(dataPerDay$nb_valid, na.rm = TRUE)
  
  plots_week <- list()
  for (year in 2018:2023) {
    data_filtered <- dataPerDay %>% filter(YEAR == year)
    
    p <- ggplot(data_filtered, aes(x=weekday, y=nb_valid, group = 1)) +
      geom_col(fill = "#0073C2FF") + 
      scale_y_continuous( 
        limits = c(0, max_y_dataPerDay),
        breaks = seq(0, max_y_dataPerDay, by = max_y_dataPerDay / 10) 
      ) +
      labs(
        title = paste("Année :", year)
      ) +
      theme_minimal() +  
      theme(
        legend.position = "top",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = -75)
      )
    plots_week <- append(plots_week, list(p))
  }
  
  output$gridPlotWeekMean <- renderPlot({
    wrap_plots(plots_week, ncol = 6, nrow = 1)
  })
  
  ################################################################################
  # PLOTS SEASON MEAN                                                            #
  ################################################################################
  
  max_y_dataPerSaison <- max(allDataFrameNB %>% group_by(saison, YEAR) %>% 
                               summarise(n=mean(NB_VALD)) %>% 
                               pull(n), na.rm = TRUE)
  
  plots_season_mean <- list()
  for (year in 2018:2023) {
    dataPerSaison <- allDataFrameNB %>% filter(YEAR == year) %>% group_by(saison) %>% summarise(nb_valid = mean(NB_VALD))
    
    p <- ggplot(dataPerSaison, aes(x=saison, y=nb_valid)) +
      geom_col(fill = "#0073C2FF") +
      scale_y_continuous(
        limits = c(0, max_y_dataPerSaison),
        breaks = seq(0, max_y_dataPerSaison, by = max_y_dataPerSaison / 10) 
      ) + 
      labs(
        title = paste("Année :", year)
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = -75)
      ) 
    plots_season_mean <- append(plots_season_mean, list(p))
  }
  
  output$gridPlotSeasonMean <- renderPlot({
    wrap_plots(plots_season_mean, ncol = 6, nrow = 1)
  })
  
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
