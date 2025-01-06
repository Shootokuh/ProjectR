library(dplyr)
library(ggplot2)

# allDataFrameNB$CATEGORIE_TITRE <- ifelse(allDataFrameNB$CATEGORIE_TITRE == "?", "N/A", allDataFrameNB$CATEGORIE_TITRE)

ANNUAL_NB_FER_2018$WEEK <- strftime(ANNUAL_NB_FER_2018$JOUR, format = "%V")
ANNUAL_NB_FER_2018$WEEK <- as.numeric(ANNUAL_NB_FER_2018$WEEK)
ANNUAL_NB_FER_2018$YEAR <- strftime(ANNUAL_NB_FER_2018$JOUR, format = "%Y")
ANNUAL_NB_FER_2018$YEAR <- as.numeric(ANNUAL_NB_FER_2018$YEAR)
dataAboveMeanByWeekYear2018 = dataByWeekYear2018 %>% filter(n <= meanWeek)

################################################################################

dataByDayYear2018Summer = ANNUAL_NB_FER_2018 %>% 
  filter(JOUR >= as.Date("2018-07-01") & JOUR <= as.Date("2018-09-01")) %>%
  group_by(JOUR) %>% 
  summarize(n=n()) %>%
  arrange(JOUR)


ggplot(dataByDayYear2018Summer) +
  geom_line(aes(x=JOUR,y=n))+
  theme_bw()+
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d-%b"
  )+
  xlab("nombre de validations")+ylab("temps")+
  ggtitle("Nombres de validation par jour sur la période Juillet-Septembre 2018")

################################################################################

dataByDayPerYearLess = dataByDayYear2018 %>% filter(n < 4800)

ggplot(dataByDayPerYearLess) +
  geom_line(aes(x=JOUR,y=n))+
  theme_bw()+
  scale_x_date(
    date_breaks = "1 month",      # Afficher une graduation par semaine
    date_labels = "%d-%b"       # Formater les étiquettes en "jour-mois" (e.g., 01-Jan)
  )+
  xlab("nombre de validations")+ylab("temps")+
  ggtitle("Nombres de validation par jour sur 2018 Inférieur à 4800 validations")

################################################################################


dataByWeekYear2019 = ANNUAL_NB_FER_2019 %>% 
  group_by(WEEK) %>% 
  summarize(n=n()) %>%
  arrange(WEEK)

ggplot(dataByWeekYear2018, aes(x=WEEK,y=n, group = 1)) +
  geom_line() + # Ligne semaine min
  geom_hline(aes(yintercept = minWeek, color = "min"), linetype = "dashed", show.legend = TRUE) +
  geom_line() + # Ligne semaine max
  geom_hline(aes(yintercept = maxWeek, color = "max"), linetype = "dashed") +
  geom_line() + # Ligne semaine mean
  geom_hline(aes(yintercept = meanWeek, color = "mean"), linetype = "dashed") +
  theme_bw() +
  theme_minimal() +
  scale_x_continuous(
    breaks = dataByWeekYear2018$WEEK,
    labels = dataByWeekYear2018$WEEK
  ) + 
  scale_color_manual(
    name = "Seuils",  # Nom de la légende
    values = c("min" = "red", "max" = "green", "mean" = "blue")
  ) +
  xlab("Semaines")+ylab("temps")+
  ggtitle("Nombres de validations par semaine sur 2018")

minWeek <- min(dataByWeekYear2019$n)
minWeek3 <- dataByWeekYear2019 %>%
            arrange(desc(n)) %>%
            slice(1:3)
minWeek3List <- minWeek3 %>% pull(WEEK)
maxWeek <- max(dataByWeekYear2019$n)
meanWeek <- mean(dataByWeekYear2019$n)

################################################################################
# DIFFERENCE VACANCES SCOLAIRES / HORS VACANCES SCOLAIRES                      #
################################################################################
S1_2018_PROFIL_FER$pourc_validations <- gsub(",", ".", S1_2018_PROFIL_FER$pourc_validations)
S1_2018_PROFIL_FER$pourc_validations <- as.numeric(S1_2018_PROFIL_FER$pourc_validations)

scolarByWeekPeroid2018 
taffByWeekPeriod2018


################################################################################

utilisationTitre2018 = ANNUAL_NB_FER_2018 %>%
  group_by(CATEGORIE_TITRE, WEEK) %>%
  summarize(nb_valid = sum(NB_VALD))

semaine_moyenne = utilisationTitre2018 %>%
  group_by(WEEK) %>%
  summarise(nb_vald_moyenne = mean(nb_valid, na.rm = TRUE))


ggplot(utilisationTitre2018, aes(x = WEEK, y = nb_valid, color = CATEGORIE_TITRE, group = CATEGORIE_TITRE))+
  geom_line(linewidth=1)+
  scale_x_continuous(
    breaks = utilisationTitreVacances2018$WEEK,
    labels = utilisationTitreVacances2018$WEEK
  ) + 
  scale_y_continuous(
    breaks = seq(10000, max(utilisationTitreVacances2018$nb_valid), by = 5000000),
    labels = scales::label_comma(scale = 100)
  ) + 
  theme_light() +
  theme(
    legend.position = "right"  # Position de la légende
  ) + 
  ggtitle("Nombre de validations par semaine et par catégorie de titre en 2018")

################################################################################
# Le plus fréquenté                                                            #
################################################################################


##########################
# Arrêt les + fréquenté  #
##########################


###########################
# RER                     #
###########################

# Ligne la + fréquenté RER
ligneMostFrequent2018 = ANNUAL_NB_FER_2018 %>% 
  filter(startsWith(as.character(CODE_STIF_TRNS), "8")) %>%
  group_by(CODE_STIF_TRNS, CODE_STIF_RES) %>% 
  summarize(nb_valid=sum(NB_VALD)) %>%
  arrange(desc(nb_valid)) %>%
  slice(1:5)

ggplot(ligneMostFrequent2018, 
      aes(
        x = interaction(CODE_STIF_TRNS, CODE_STIF_RES), 
        y = nb_valid
      )) +
  geom_col(fill = "#0073C2FF")+
  labs(x = "CODE_STIF_TNRS, CODE_STIF_RES", y = "nb_valid", title = "Diagramme en barres des sommes de NG") +
  geom_text(aes(label = nb_valid), vjust = 1.6, color="white") +
  scale_y_continuous(
    labels = scales::label_comma(scale = 100),  # Formater les labels avec des virgules pour les grandes valeurs
    breaks = seq(10000000, max(ligneMostFrequent2018$nb_valid), by = 30000000)  # Définir l'intervalle des graduations
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Station les + fréquenté sur la semaine la + fréquenté
stationMostFrequentMostWeek2018 = ANNUAL_NB_FER_2018 %>% 
  filter(WEEK == maxWeek$WEEK & startsWith(as.character(CODE_STIF_TRNS), "8")) %>%
  group_by(CODE_STIF_TRNS, CODE_STIF_RES) %>% 
  summarize(nb_valid=sum(NB_VALD)) %>%
  arrange(desc(nb_valid)) %>%
  slice(1:5)

ggplot(stationMostFrequentMostWeek2018,
      aes(
        x = interaction(CODE_STIF_TRNS, CODE_STIF_RES), 
        y = nb_valid
      )) +
  geom_col(fill = "#0073C2FF") +
  labs(x = "CODE_STIF_TNRS, CODE_STIF_RES", y = "nb_valid", title = "Diagramme en barres des sommes de NG") +
  geom_text(aes(label = nb_valid), vjust = 1.6, color = "white") +
  scale_y_continuous(
    labels = scales::label_comma(scale = 100),  # Formater les labels avec des virgules pour les grandes valeurs
    breaks = seq(100000, max(stationMostFrequentMostWeek2018$nb_valid), by = 300000)  # Définir l'intervalle des graduations
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Station les + fréquenté sur 2018
stationMostFrequent2018 = ANNUAL_NB_FER_2018 %>%
  filter(startsWith(as.character(CODE_STIF_TRNS), "8")) %>%
  group_by(CODE_STIF_TRNS, CODE_STIF_RES) %>% 
  summarize(nb_valid=sum(NB_VALD)) %>%
  arrange(desc(nb_valid)) %>%
  slice(1:5)

ggplot(stationMostFrequent2018, 
      aes(
        x = interaction(CODE_STIF_TRNS, CODE_STIF_RES), 
        y = nb_valid,
      )) +
  geom_col(fill = "#0073C2FF") +
  labs(x = "CODE_STIF_TNRS, CODE_STIF_RES", y = "nb_valid", title = "Nombre de validations par stations") +
  geom_text(aes(label = nb_valid), vjust = 1.6, color = "white") +
  scale_y_continuous(
    labels = scales::label_comma(scale = 100),  # Formater les labels avec des virgules pour les grandes valeurs
    breaks = seq(10000000, max(stationMostFrequent2018$nb_valid), by = 30000000)  # Définir l'intervalle des graduations
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#######
# RER #
#######

test = ANNUAL_NB_FER_2018 %>% filter(CODE_STIF_TRNS == 800) %>% group_by(CODE_STIF_RES, CODE_STIF_TRNS) %>% summarize(n=n())

# RER A
# 810 801
# RER B
# 810 802
# RER E
# 800 805 964
# 800 805 967

# Metro 2
# 100 110 

#########
# METRO #
#########
