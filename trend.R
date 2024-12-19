library(dplyr)
library(ggplot2)

################################################################################
# A CORRIGER !!! FAIRE LA MOYENNE ET NON PAS LA SOMME POUR LES DATA SUR SEMAINES
# / DAY / SAISON                                                               #
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

################################################################################
# Tendances sur les années                                                     #
################################################################################

trendAllData <- allData %>% mutate(
  year = format(as.Date(JOUR), "%Y"), # Extraire l'année
  month = format(as.Date(JOUR), "%m") # Extraire la semaine
) %>%
  # Créer un regroupement par combinaison unique année-semaine
  group_by(year_month = paste0(year, "-M", month)) %>%
  summarise(nb_valid = sum(NB_VALD), .groups = "drop")

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

################################################################################

dataPerDay <- allData %>% group_by(weekday) %>% summarise(nb_valid = sum(NB_VALD))
dataPerDay <- dataPerDay[order(dataPerDay$weekday), ]

ggplot(dataPerDay, aes(x=weekday, y=nb_valid))+
  geom_col(fill = "#0073C2FF") + 
  geom_text(aes(label = nb_valid), vjust = 1.6, color="white") + 
  scale_y_continuous(
    breaks = seq(1000000, max(dataPerDay$nb_valid) + 50000000, by = 50000000),
    labels = scales::label_comma(scale = 1000)
  ) +
  theme_minimal() +
  ggtitle("Nombre de validations par jour")

################################################################################

dataPerSaison <- allData %>% filter(YEAR == 2019) %>% group_by(saison) %>% summarise(nb_valid = sum(NB_VALD))

ggplot(dataPerSaison, aes(x=saison, y=nb_valid))+
  geom_col(fill = "#0073C2FF") +
  geom_text(aes(label = nb_valid), vjust = 1.6, color="white") + 
  scale_y_continuous(
    breaks = seq(100000000, max(dataPerDay$nb_valid) + 5000000000, by = 200000000),
    labels = scales::label_comma(scale = 1000)
  ) +
  theme_minimal() + 
  ggtitle("Nombre de validations par saisons")

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
# PAS INTÉRESSANT                                                              #
################################################################################
utilisationTitre <- allData %>% 
                    group_by(CATEGORIE_TITRE, year_week) %>%
                    summarise(nb_valid = sum(NB_VALD))

ggplot(utilisationTitre, aes(x = year_week, y = nb_valid, group = 1)) +
  geom_line(aes(color = CATEGORIE_TITRE), position = position_stack()) +
  scale_y_continuous(
    breaks = seq(100000, max(utilisationTitreVacancesEte$nb_valid) + 5000000000, by = 5000000),
    labels = scales::label_comma(scale = 1000)
  ) +
  theme_light() +
  ggtitle("Type d'utilisation de titre par semaine en moyenne sur les années 2018-2021")

