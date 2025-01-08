# Chargement des bibliothèques nécessaires
library(readr)
library(sf)
library(dplyr)

# Suppression au préalable des données
rm(list = ls())

# Liste des départements présents en Ile de France
ile_de_france_departments <- c("75", "77", "78", "91", "92", "93", "94", "95")

# Fonction déterminant la saison selon la date donnée
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


S1_2023_NB_FER <- read.csv("validations-reseau-ferre-nombre-validations-par-jour-1er-semestre.csv", sep=";")

years <- 2018:2023
semesters <- c("S1", "S2")

# Parcours des années et semestres
for (year in years) {
  for (semester in semesters) {
    delimiter <- ifelse(year == "2022" & semester == "S2", ";", "\t")
    
    # Récupération des données dans les fichiers au format .txt
    nb_fer_file <- paste0("data-rf-", year, "/", year, "_", semester, "_NB_FER.txt")
    profil_fer_file <- paste0("data-rf-", year, "/", year, "_", semester, "_PROFIL_FER.txt")
    
    # Stockage des données dans des dataframe
    if (file.exists(nb_fer_file)) {
      assign(paste0(semester, "_", year, "_NB_FER"), read_delim(nb_fer_file, delim = delimiter))
    }
    if (file.exists(profil_fer_file)) {
      assign(paste0(semester, "_", year, "_PROFIL_FER"), read_delim(profil_fer_file, delim = delimiter))
    }
  }
  
  # Renommage des colonnes "lda" en "ID_REFA_LDA" pour les années 2022 et 2023
  # afin d'être en cohérence avec les autres années
  if(exists("S2_2022_PROFIL_FER")) {
    colnames(S2_2022_PROFIL_FER)[colnames(S2_2022_PROFIL_FER) == "lda"] <- "ID_REFA_LDA"
  }
  if(exists("S2_2022_NB_FER")) {
    colnames(S2_2022_NB_FER)[colnames(S2_2022_NB_FER) == "lda"] <- "ID_REFA_LDA"
  }
  if(exists("S1_2023_NB_FER")) {
    colnames(S1_2023_NB_FER)[colnames(S1_2023_NB_FER) == "lda"] <- "ID_REFA_LDA"
  }
  if (year != 2023) {
    # Récupération des semestres S1 et S2 pour les data NB_FER
    S1 <- paste0("S1_", year, "_NB_FER")
    S2 <- paste0("S2_", year, "_NB_FER")
    # Récupération des semestres S1 et S2 pour les data PROFIL_FER
    S1_PROFIL_FER <- paste0("S1_", year, "_PROFIL_FER")
    S2_PROFIL_FER <- paste0("S2_", year, "_PROFIL_FER")
    
    # Conversion de la colonne JOUR en un format de Date pour les data NB_FER
    vectorS1 <- get(S1)
    vectorS1$JOUR <- as.Date(vectorS1$JOUR, format = "%d/%m/%Y")
    vectorS2 <- get(S2)
    vectorS2$JOUR <- as.Date(vectorS2$JOUR, format = "%d/%m/%Y")
    
    
    vectorS1_PROFIL_FER <- get(S1_PROFIL_FER)
    vectorS2_PROFIL_FER <- get(S2_PROFIL_FER)
    # Création des tables annuelles pour les data PROFIL_FER et NB_FER 
    # en regroupant les semestres S1 et S2
    assign(paste0("ANNUAL_NB_FER_", year), rbind(vectorS1, vectorS2))
    assign(paste0("ANNUAL_PROFIL_FER_", year), rbind(vectorS1_PROFIL_FER, vectorS2_PROFIL_FER))
    
    # Suppression des data non utilisées
    rm(vectorS1, vectorS2, vectorS1_PROFIL_FER, vectorS2_PROFIL_FER)
  }
}

# Stockage des données spatiales dans la data SPATIAL_DATA 
# Initialisation des coordonnées dans SPATIAL_DATA en Lambert-93, 
# un système projeté couramment utilisé pour la région Île-de-France.
SPATIAL_DATA = st_read("REF_ZdA/PL_ZDL_R_17_12_2024.shp", crs=2154)

# Renommage des colonnes "idrefa_lda" en "ID_REFA_LDA" pour la data SPATIAL_DATA
colnames(SPATIAL_DATA)[colnames(SPATIAL_DATA) == "idrefa_lda"] <- "ID_REFA_LDA"

# Filtrage de la colonne commune afin de garder seulement les communes
# en Île-de-France
SPATIAL_DATA <- SPATIAL_DATA %>%
  filter(substr(commune, 1, 2) %in% ile_de_france_departments)

for (year in 2018:2022) {
  
  # Début du traitement pour les data de la forme ANNUAL_NB_FER_ANNEE
  annual_nb_fer <- get(paste0("ANNUAL_NB_FER_", year))
  
  # Suppression des espaces blancs dans la colonne LIBELLE_ARRET
  annual_nb_fer <- annual_nb_fer %>%
    mutate(LIBELLE_ARRET = trimws(LIBELLE_ARRET))
  
  # Changement des données incohérentes dans CATEGORIE_TITRE par 'autre'
  # afin d'obtenir un ensemble de données cohérent
  annual_nb_fer <- annual_nb_fer %>%
    mutate(CATEGORIE_TITRE = recode(CATEGORIE_TITRE,
                                    "?" = "autre",
                                    "NON DEFINI" = "autre",
                                    "AUTRE TITRE" = "autre"))
  
  # Ajout d'une colonne WEEK de type numérique 
  # afin d'obtenir le numéro de la semaine de l'année pour chaque ligne
  annual_nb_fer$WEEK <- strftime(annual_nb_fer$JOUR, format = "%V")
  annual_nb_fer$WEEK <- as.numeric(annual_nb_fer$WEEK)
  
  # Ajout d'une colonne YEAR dans chaque data ANNUAL_NB_FER_ANNEE
  annual_nb_fer$YEAR <- strftime(annual_nb_fer$JOUR, format = "%Y")
  annual_nb_fer$YEAR <- as.numeric(annual_nb_fer$YEAR)
  
  # Ajout d'une colonne year_week qui combine la colonne YEAR et WEEK
  annual_nb_fer$year_week <- paste0(annual_nb_fer$YEAR, "-W", annual_nb_fer$WEEK)
  
  # Ajout d'une colonne weekday qui contient le nom du jour de la semaine
  # pour chaque date dans la colonne JOUR
  annual_nb_fer$weekday <- weekdays(annual_nb_fer$JOUR)
  annual_nb_fer$weekday <- factor(
    annual_nb_fer$weekday,
    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  )
  # Transformation de la colonne year_week en un facteur où les niveaux sont définis
  # par les valeurs uniques présentes dans la colonne 
  annual_nb_fer$year_week <- factor(annual_nb_fer$year_week, levels = unique(annual_nb_fer$year_week))
  
  # Ajout d'une colonne saison qui applique la fonction get_season à chaque date 
  # afin de déterminer la saison
  season_order <- c("Printemps", "Été", "Automne", "Hiver")
  annual_nb_fer$saison <- sapply(as.Date(annual_nb_fer$JOUR), get_season)
  annual_nb_fer$saison <- factor(annual_nb_fer$saison, levels = season_order)
  assign(paste0("ANNUAL_NB_FER_", year), annual_nb_fer)
  
  
  # Début du traitement pour les data de la forme ANNUAL_PROFIL_FER_ANNEE
  annual_profil_fer <- get(paste0("ANNUAL_PROFIL_FER_", year))
  
  annual_profil_fer <- annual_profil_fer %>%
    mutate(LIBELLE_ARRET = trimws(LIBELLE_ARRET))
  
  # Remplacement des virgules et conversion en numérique des valeurs pourc_validations
  annual_profil_fer$pourc_validations <- gsub(",", ".", annual_profil_fer$pourc_validations)
  annual_profil_fer$pourc_validations <- as.numeric(annual_profil_fer$pourc_validations)
  
  # Suppression des valeurs 'ND' 
  annual_profil_fer <- annual_profil_fer %>% filter(TRNC_HORR_60 != "ND")
  
  # Ajout d'une colonne YEAR dans chaque data ANNUAL_PROFIL_FER_ANNEE
  annual_profil_fer$YEAR <- year
  
  
  assign(paste0("ANNUAL_PROFIL_FER_", year), annual_profil_fer)
  
  # Suppression des data inutilisées
  rm(annual_nb_fer, annual_profil_fer)
}

# Le traitement de la colonne weekday pour l'année 2018 doit se faire 
# à la suite de la boucle afin d'éviter les incohérences et les valeurs manquantes
ANNUAL_NB_FER_2018$weekday <- factor(
  ANNUAL_NB_FER_2018$weekday,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)

ANNUAL_NB_FER_2018$weekday = weekdays(ANNUAL_NB_FER_2018$JOUR)


# Enregistrement de toutes les data NB_fer annuelles dans une liste
years_data_nb <- list(ANNUAL_NB_FER_2018, ANNUAL_NB_FER_2019, ANNUAL_NB_FER_2020, 
                      ANNUAL_NB_FER_2021, ANNUAL_NB_FER_2022)

# Enregistrement de toutes les data NB_profil annuelles dans une liste
years_data_profil <- list(ANNUAL_PROFIL_FER_2018, ANNUAL_PROFIL_FER_2019, ANNUAL_PROFIL_FER_2020, 
                          ANNUAL_PROFIL_FER_2021, ANNUAL_PROFIL_FER_2022)

# Étant donné que le fichier contenant les données NB_FER pour le semestre S1 2023 
# possède un format différent des autres fichiers, le traitement se fait hors de la boucle
# de manière spécifique pour 2023
S1_2023_NB_FER <- S1_2023_NB_FER  %>%
  mutate(CATEGORIE_TITRE = recode(CATEGORIE_TITRE,
                                  "?" = "autre",
                                  "NON DEFINI" = "autre",
                                  "AUTRE TITRE" = "autre"))

S1_2023_NB_FER <- S1_2023_NB_FER %>%
  mutate(LIBELLE_ARRET = trimws(LIBELLE_ARRET))

S1_2023_NB_FER$JOUR <- as.Date(S1_2023_NB_FER$JOUR, format = "%Y-%m-%d")

S1_2023_NB_FER$WEEK <- strftime(S1_2023_NB_FER$JOUR, format = "%V")
S1_2023_NB_FER$WEEK <- as.numeric(S1_2023_NB_FER$WEEK)

S1_2023_NB_FER$YEAR <- strftime(S1_2023_NB_FER$JOUR, format = "%Y")
S1_2023_NB_FER$YEAR <- as.numeric(S1_2023_NB_FER$YEAR)

S1_2023_NB_FER$year_week <- paste0(S1_2023_NB_FER$YEAR, "-W", S1_2023_NB_FER$WEEK)

S1_2023_NB_FER$weekday <- weekdays(S1_2023_NB_FER$JOUR)

S1_2023_NB_FER$year_week <- factor(S1_2023_NB_FER$year_week, levels = unique(S1_2023_NB_FER$year_week))

S1_2023_NB_FER$saison <- sapply(as.Date(S1_2023_NB_FER$JOUR), get_season)

# Suppression des data S1 et S2 NB_FER et PROFIL_FER pour chaque année 
# car elles ne sont pas utilisées
rm(list = ls(pattern = "^(S1|S2)_(2018|2019|2020|2021|2022)_NB_FER$"))
rm(list = ls(pattern = "^(S1|S2)_(2018|2019|2020|2021|2022)_PROFIL_FER$"))

allDataFrameNB = rbind(ANNUAL_NB_FER_2018, ANNUAL_NB_FER_2019, ANNUAL_NB_FER_2020, ANNUAL_NB_FER_2021, ANNUAL_NB_FER_2022, S1_2023_NB_FER)

allDataFrameProfile = rbind(ANNUAL_PROFIL_FER_2018, ANNUAL_PROFIL_FER_2019, ANNUAL_PROFIL_FER_2020, ANNUAL_PROFIL_FER_2021, ANNUAL_PROFIL_FER_2022)

saveRDS(allDataFrameNB, "ProjectR_dashboard/allDataFrameNB.RDS", compress = TRUE)
saveRDS(allDataFrameProfile, "ProjectR_dashboard/allDataFrameProfile.RDS", compress = TRUE)
saveRDS(SPATIAL_DATA, "ProjectR_dashboard/Spatial_Data.RDS", compress = TRUE)
