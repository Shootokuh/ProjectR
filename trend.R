library(dplyr)
library(ggplot2)
library(gdata)


print("TREND BEGIN")
################################################################################
# A CORRIGER !!! FAIRE LA MOYENNE ET NON PAS LA SOMME POUR LES DATA SUR SEMAINES
# / DAY / SAISON                                                               x#
################################################################################

ANNUAL_NB_FER_2021$WEEK <- strftime(ANNUAL_NB_FER_2021$JOUR, format = "%V")
ANNUAL_NB_FER_2021$WEEK <- as.numeric(ANNUAL_NB_FER_2021$WEEK)
ANNUAL_NB_FER_2021$YEAR <- strftime(ANNUAL_NB_FER_2021$JOUR, format = "%Y")
ANNUAL_NB_FER_2021$YEAR <- as.numeric(ANNUAL_NB_FER_2021$YEAR)
ANNUAL_NB_FER_2021$year_week <- paste0(ANNUAL_NB_FER_2021$YEAR, "-W", ANNUAL_NB_FER_2021$WEEK)
ANNUAL_NB_FER_2021$weekday <- weekdays(ANNUAL_NB_FER_2021$JOUR)
ANNUAL_NB_FER_2018$weekday <- factor(
  ANNUAL_NB_FER_2018$weekday,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)
ANNUAL_NB_FER_2021$year_week <- factor(ANNUAL_NB_FER_2021$year_week, levels = unique(ANNUAL_NB_FER_2021$year_week))

season_order <- c("Printemps", "Été", "Automne", "Hiver")

allData$saison <- factor(allData$saison, levels = season_order)

get_season <- function(date) {
  month <- as.numeric(format(date, "%m"))
  day <- as.numeric(format(date, "%d"))
  
  if ((month == 12 && day >= 21) || month %in% c(1, 2) || (month == 3 && day < 21)) {
    return("Hiver")
  } else if ((month == 3 && day >= 21) || month %in% c(4, 5) || (month == 6 && day < 21)) {
    return("Printemps")
  } else if ((month == 6 && day >= 21) || month %in% c(7, 8) || (month == 9 && day < 21)) {
    return("Été")
  } else {
    return("Automne")
  }
}

# Appliquer la fonction à la colonne date
ANNUAL_NB_FER_2021$saison <- sapply(as.Date(ANNUAL_NB_FER_2021$JOUR), get_season)



allData = rbind(ANNUAL_NB_FER_2018, ANNUAL_NB_FER_2019, ANNUAL_NB_FER_2020, ANNUAL_NB_FER_2021)
allData <- allDataFrameNB
allData$weekday <- factor(
  allData$weekday,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)

selected_years <- c(2018, 2019, 2020, 2021, 2022)  # Années fixes
################################################################################
# Tendances sur les années                                                     #
################################################################################

trendAllData <- allData %>% mutate(
  year = format(as.Date(JOUR), "%Y"), # Extraire l'année
  month = format(as.Date(JOUR), "%m") # Extraire la semaine
) %>%
  # Créer un regroupement par combinaison unique année-semaine
  group_by(year_month = paste0(year, "-M", month), YEAR) %>%
  summarise(nb_valid = sum(NB_VALD), .groups = "drop")

max_y_trendAllData <- max(trendAllData$nb_valid, na.rm = TRUE)

ggplot(trendAllData, aes(x=year_month, y=nb_valid, group = 1)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(
    breaks = seq(10000000, max(trendAllData$nb_valid) + 50000000, by = 10000000),
    labels = scales::label_comma(scale = 1000)
  ) +
  theme_bw() +
  theme_linedraw() + 
  theme(axis.text.x = element_text(angle = -75)) +
  ggtitle("Nombre de validations sur tout le réseau du 01/01/2018 au 31/12/2021")

t <- allData %>% filter(YEAR == 2018) %>% mutate(
  year = format(as.Date(JOUR), "%Y"), # Extraire l'année
  month = format(as.Date(JOUR), "%m") # Extraire la semaine
) %>%
  # Créer un regroupement par combinaison unique année-semaine
  group_by(year_month = paste0(year, "-M", month)) %>%
  summarise(nb_valid = sum(NB_VALD), .groups = "drop")

ggplot(trendAllData, aes(x = year_month, y = nb_valid, group = 1)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(
    breaks = seq(10000000, max(trendAllData$nb_valid) + 50000000, by = 10000000),
    labels = scales::label_comma(scale = 1000)
  ) +
  theme_bw() +
  theme_linedraw() + 
  theme(axis.text.x = element_text(angle = -75)) +
  ggtitle(paste("Année :", year, "| Mesure :"))
################################################################################



ggplot(dataPerDay %>% filter(YEAR == 2021), aes(x=weekday, y=nb_valid, group = 1))+
  geom_col(fill = "#0073C2FF") + 
  geom_text(aes(label = nb_valid), vjust = 1.6, color="white") + 
  scale_y_continuous(
    breaks = seq(1000000, max(dataPerDay$nb_valid) + 50000000, by = 50000000),
    labels = scales::label_comma(scale = 1000)
  ) +
  theme_minimal() +
  ggtitle("Nombre de validations par jour")


################################################################################

utilisationTitrePerWeek <- allData %>% group_by(CATEGORIE_TITRE, weekday) %>% summarise(nb_valid = sum(NB_VALD))

ggplot(utilisationTitrePerWeek, aes(x = weekday, y = nb_valid)) +
  geom_col(aes(color = CATEGORIE_TITRE, fill = CATEGORIE_TITRE), position = position_stack()) +
  scale_y_continuous(
    breaks = seq(100000000, max(dataPerDay$nb_valid) + 5000000000, by = 200000000),
    labels = scales::label_comma(scale = 1000)
  ) +
  theme_minimal() + 
  ggtitle("Type d'utilisation de titre par semaine")

################################################################################

vacancesWeek = c(28,29,30,31,32,33)
utilisationTitreVacancesEte <- allData %>%
                              group_by(CATEGORIE_TITRE, WEEK) %>% 
                              summarise(nb_valid = sum(NB_VALD))

ggplot(utilisationTitreVacancesEte, aes(x = WEEK, y = nb_valid)) +
  geom_line(aes(color = CATEGORIE_TITRE), position = position_stack()) +
  scale_x_continuous(
    breaks = utilisationTitreVacancesEte$WEEK,
    labels = utilisationTitreVacancesEte$WEEK
  ) +
  scale_y_continuous(
    breaks = seq(10000000, max(utilisationTitreVacancesEte$nb_valid) + 5000000000, by = 20000000),
    labels = scales::label_comma(scale = 1000)
  ) +
  theme_light() +
  ggtitle("Type d'utilisation de titre par semaine en moyenne sur les années 2018-2021")

################################################################################
# VALID PER WEEK DONE                                                          #
################################################################################




################################################################################
# TENDANCE HORAIRES                                                            #
################################################################################

max_y_pourc_valid = max(allDataFrameProfile %>% 
                          group_by(CAT_JOUR, TRNC_HORR_60, YEAR) %>%
                          summarize(n = mean(pourc_validations)) %>%
                          mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60))) %>%
                          pull(n))

plots_HV_VS <- list()
for (year in selected_years) {
  scolarPeriod = allDataFrameProfile %>% 
    filter(YEAR == year) %>%
    group_by(CAT_JOUR, TRNC_HORR_60) %>% 
    filter(CAT_JOUR == 'JOVS' | CAT_JOUR == "SAVS") %>%
    summarize(pourc_validations_mean = mean(pourc_validations)) %>%
    mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60)))
  
  taffPeriod = allDataFrameProfile %>% 
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


allDataFrameProfileJOVS <- allDataFrameProfile %>% 
  group_by(CAT_JOUR, TRNC_HORR_60, YEAR) %>%
  filter(CAT_JOUR == 'JOVS') %>%
  summarize(pourc_validations_mean = mean(pourc_validations)) %>%
  mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60)))

allDataFrameProfileSAVS <- allDataFrameProfile %>% 
  group_by(CAT_JOUR, TRNC_HORR_60, YEAR) %>%
  filter(CAT_JOUR == 'SAVS') %>%
  summarize(pourc_validations_mean = mean(pourc_validations)) %>%
  mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60)))

allDataFrameProfileJOHV <- allDataFrameProfile %>% 
  group_by(CAT_JOUR, TRNC_HORR_60, YEAR) %>%
  filter(CAT_JOUR == 'JOHV') %>%
  summarize(pourc_validations_mean = mean(pourc_validations)) %>%
  mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60)))

allDataFrameProfileSAHV <- allDataFrameProfile %>% 
  group_by(CAT_JOUR, TRNC_HORR_60, YEAR) %>%
  filter(CAT_JOUR == 'SAHV') %>%
  summarize(pourc_validations_mean = mean(pourc_validations)) %>%
  mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60)))



ggplot(allDataFrameProfileSAHV, aes(x = Heure_Val, y = pourc_validations_mean,color = as.factor(YEAR), group = YEAR)) +
  geom_line(linewidth = 1) +  # Ligne pour chaque année
  labs(
    title = "Pourcentage de validations par tranche horaire",
    x = "Tranche horaire",
    y = "Pourcentage de validations",
    color = "Année"
  ) +
  scale_x_continuous(
    breaks = scolarPeriod$Heure_Val, 
    labels = scolarPeriod$TRNC_HORR_60
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", # Légende en bas
    axis.text.x = element_text(angle = -90, hjust = 1)  # Rotation des labels de l'axe X
  )


print("TREND #1")
################################################################################
# Les 15 stations + flux                                                       #
################################################################################

s <- spatial_data %>% filter(nom == "Haussmann Saint-Lazare")
#t <- left_join(s, allData)
#u <- unique(t$LIBELLE_ARRET)


dataStations <- allData %>%
                group_by(ID_REFA_LDA) %>% 
                summarise(n = mean(sum(NB_VALD))) %>%
                arrange(desc(n))

#a <- allData %>% filter(YEAR == 2020)
#      group_by(LIBELLE_ARRET, ID_REFA_LDA) %>% 
#      summarise(n = sum(NB_VALD)) %>% filter(n < 10000) %>% arrange(desc(n))

#a1 <- allData %>% filter(ID_REFA_LDA == 73688, YEAR == 2021) %>% 
#  group_by(ID_REFA_LDA) %>%
#  summarise(n = sum(NB_VALD, na.rm = TRUE)) %>% arrange(desc(n))

#libelle <- allData %>% group_by(ID_REFA_LDA) %>%
#  filter(n_distinct(LIBELLE_ARRET) > 1) %>%
#  distinct(ID_REFA_LDA, LIBELLE_ARRET) %>% arrange(desc(ID_REFA_LDA))

print("TREND END !")
#19_519_939
#23_712_307

saison <- allData %>% group_by(saison) %>% summarise(nb_valid = mean(NB_VALD))

ggplot(saison, aes(x=saison, y=nb_valid)) +
  geom_col(fill = "#0073C2FF") +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = -75)
  ) 