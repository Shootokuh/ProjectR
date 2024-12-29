library(readr)
library(sf)
library(dplyr)

# Supprime toute les données 
rm(list = ls())

ile_de_france_departments <- c("75", "77", "78", "91", "92", "93", "94", "95")

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

years <- 2018:2023
semesters <- c("S1", "S2")


for (year in years) {
  for (semester in semesters) {
    delimiter <- ifelse(year == "2022" & semester == "S2", ";", "\t")
    nb_fer_file <- paste0("data-rf-", year, "/", year, "_", semester, "_NB_FER.txt")
    profil_fer_file <- paste0("data-rf-", year, "/", year, "_", semester, "_PROFIL_FER.txt")
    if (file.exists(nb_fer_file)) {
      assign(paste0(semester, "_", year, "_NB_FER"), read_delim(nb_fer_file, delim = delimiter))
    }
    if (file.exists(profil_fer_file)) {
      assign(paste0(semester, "_", year, "_PROFIL_FER"), read_delim(profil_fer_file, delim = delimiter))
    }
  }
  if(exists("S2_2022_PROFIL_FER")) {
    colnames(S2_2022_PROFIL_FER)[colnames(S2_2022_PROFIL_FER) == "lda"] <- "ID_REFA_LDA"
  }
  if(exists("S2_2022_NB_FER")) {
    colnames(S2_2022_NB_FER)[colnames(S2_2022_NB_FER) == "lda"] <- "ID_REFA_LDA"
  }
  if (year != 2023) {
    # Ajouter les données spatiales à chaque variables
    # Annual NB_FER
    S1 <- paste0("S1_", year, "_NB_FER")
    S2 <- paste0("S2_", year, "_NB_FER")
    # Annual PROFIL_FER
    S1_PROFIL_FER <- paste0("S1_", year, "_PROFIL_FER")
    S2_PROFIL_FER <- paste0("S2_", year, "_PROFIL_FER")
    vectorS1 <- get(S1)
    vectorS1$JOUR <- as.Date(vectorS1$JOUR, format = "%d/%m/%Y")
    vectorS2 <- get(S2)
    vectorS2$JOUR <- as.Date(vectorS2$JOUR, format = "%d/%m/%Y")
    vectorS1_PROFIL_FER <- get(S1_PROFIL_FER)
    vectorS2_PROFIL_FER <- get(S2_PROFIL_FER)
    assign(paste0("ANNUAL_NB_FER_", year), rbind(vectorS1, vectorS2))
    assign(paste0("ANNUAL_PROFIL_FER_", year), rbind(vectorS1_PROFIL_FER, vectorS2_PROFIL_FER))
    
  }
}

# Rangement des semestres PROFIL_FER dans une liste
sn_profil_fer_year <- list(S1_2018_PROFIL_FER, S2_2018_PROFIL_FER,
                           S1_2019_PROFIL_FER, S2_2019_PROFIL_FER,
                           S1_2020_PROFIL_FER, S2_2020_PROFIL_FER,
                           S1_2021_PROFIL_FER, S2_2021_PROFIL_FER,
                           S1_2022_PROFIL_FER, S2_2022_PROFIL_FER)

# Supression des Valeurs 'ND' dans chaque dataframe PROFIL_FER 
for (i in seq_along(sn_profil_fer_year)) {
  sn_profil_fer_year[[i]] = sn_profil_fer_year[[i]] %>% filter(TRNC_HORR_60 != "ND")
  sn_profil_fer_year[[i]]$pourc_validations <- gsub(",", ".", sn_profil_fer_year[[i]]$pourc_validations)
  sn_profil_fer_year[[i]]$pourc_validations <- as.numeric(sn_profil_fer_year[[i]]$pourc_validations)
}

S1_2018_PROFIL_FER <- sn_profil_fer_year[[1]]
S2_2018_PROFIL_FER <- sn_profil_fer_year[[2]]
S1_2019_PROFIL_FER <- sn_profil_fer_year[[3]]
S2_2019_PROFIL_FER <- sn_profil_fer_year[[4]]
S1_2020_PROFIL_FER <- sn_profil_fer_year[[5]]
S2_2020_PROFIL_FER <- sn_profil_fer_year[[6]]
S1_2021_PROFIL_FER <- sn_profil_fer_year[[7]]
S2_2021_PROFIL_FER <- sn_profil_fer_year[[8]]
S1_2022_PROFIL_FER <- sn_profil_fer_year[[9]]
S2_2022_PROFIL_FER <- sn_profil_fer_year[[10]]


# Spatial data
SPATIAL_DATA = st_read("REF_ZdA/PL_ZDL_R_17_12_2024.shp", crs=4326)

colnames(SPATIAL_DATA)[colnames(SPATIAL_DATA) == "idrefa_lda"] <- "ID_REFA_LDA"

#Adaptation des données spatiales à la région Île-de-France
st_crs(SPATIAL_DATA) <- 2154

SPATIAL_DATA <- st_transform(SPATIAL_DATA, crs = 4326)

SPATIAL_DATA <- SPATIAL_DATA %>%
  filter(substr(commune, 1, 2) %in% ile_de_france_departments)

for (year in 2018:2022) {
  annual_nb_fer <- get(paste0("ANNUAL_NB_FER_", year))
  
  # Jointure des spatial data avec les ANNUAL_NB_FER
  #annual_nb_fer <- left_join(annual_nb_fer, SPATIAL_DATA, by = c("ID_REFA_LDA" = "ID_REFA_LDA"))
  
  # Changement des données incohérentes dans CATEGORIE_TITRE par 'autre'
  annual_nb_fer <- annual_nb_fer %>%
    mutate(CATEGORIE_TITRE = recode(CATEGORIE_TITRE,
                                    "?" = "autre",
                                    "NON DEFINI" = "autre",
                                    "AUTRE TITRE" = "autre"))
  
  # Ajout d'une colonne WEEK de type numérique 
  annual_nb_fer$WEEK <- strftime(annual_nb_fer$JOUR, format = "%V")
  annual_nb_fer$WEEK <- as.numeric(annual_nb_fer$WEEK)
  
  annual_nb_fer$YEAR <- strftime(annual_nb_fer$JOUR, format = "%Y")
  annual_nb_fer$YEAR <- as.numeric(annual_nb_fer$YEAR)
  
  annual_nb_fer$year_week <- paste0(annual_nb_fer$YEAR, "-W", annual_nb_fer$WEEK)

  annual_nb_fer$weekday <- weekdays(annual_nb_fer$JOUR)
  
  annual_nb_fer$year_week <- factor(annual_nb_fer$year_week, levels = unique(annual_nb_fer$year_week))
  
  annual_nb_fer$saison <- sapply(as.Date(annual_nb_fer$JOUR), get_season)
  
  assign(paste0("ANNUAL_NB_FER_", year), annual_nb_fer)
  
  annual_profil_fer <- get(paste0("ANNUAL_PROFIL_FER_", year))
  
  
  # Remplacement des virgules et conversion en numérique des valeurs pourc_validations
  annual_profil_fer$pourc_validations <- gsub(",", ".", annual_profil_fer$pourc_validations)
  annual_profil_fer$pourc_validations <- as.numeric(annual_profil_fer$pourc_validations)
  
  # Suppression des valeurs 'ND' 
  annual_profil_fer <- annual_profil_fer %>% filter(TRNC_HORR_60 != "ND")
  
  assign(paste0("ANNUAL_PROFIL_FER_", year), annual_profil_fer)
}

ANNUAL_NB_FER_2018$weekday <- factor(
  ANNUAL_NB_FER_2018$weekday,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)

ANNUAL_NB_FER_2018$weekday = weekdays(ANNUAL_NB_FER_2018$JOUR)

# Enregistrement de tout les NB fer dans une liste
years_data_nb <- list(ANNUAL_NB_FER_2018, ANNUAL_NB_FER_2019, ANNUAL_NB_FER_2020, 
                      ANNUAL_NB_FER_2021, ANNUAL_NB_FER_2022)

# Enregistrement de tout les NB profil dans une liste
years_data_profil <- list(ANNUAL_PROFIL_FER_2018, ANNUAL_PROFIL_FER_2019, ANNUAL_PROFIL_FER_2020, 
                          ANNUAL_PROFIL_FER_2021, ANNUAL_PROFIL_FER_2022)


#for (i in seq_along(years_data_nb)) {
#  years_data_nb[[i]]$weekday <- weekdays(years_data_nb[[i]]$JOUR)
#}

#ANNUAL_NB_FER_2018 = years_data_nb[[1]]
#ANNUAL_NB_FER_2019 = years_data_nb[[2]]
#ANNUAL_NB_FER_2020 = years_data_nb[[3]]
#ANNUAL_NB_FER_2021 = years_data_nb[[4]]
#ANNUAL_NB_FER_2022 = years_data_nb[[5]]
#allDataFrameNB <- do.call(rbind, years_data_nb)

#allDataFrameProfile = do.call(rbind, years_data_profil)

t = left_join(SPATIAL_DATA, ANNUAL_NB_FER_2018, by = c("ID_REFA_LDA" = "ID_REFA_LDA"))

#categorie_max <- t %>%
#  group_by(t$CATEGORIE_TITRE) %>%
#  summarise(total_validation = sum(t$NB_VALD)) %>% head(1)

#heure de pointe + pourcentage validations
horaire_pointe_lognes <- t %>%
  filter(nom == "Torcy") %>%  # Filtrer les données pour l'arrêt "Lognes"
  group_by(TRNC_HORR_60) %>%    # Grouper par horaire (ou la colonne horaire souhaitée)
  summarise(total = sum(pourc_validations)) %>%
  arrange(desc(total))# Calculer la somme des validations
#Categorie + utilisé
categorie <- t %>%
  filter(nom == "Torcy") %>%  # Filtrer les données pour l'arrêt "Lognes"
  group_by(CATEGORIE_TITRE) %>%    # Grouper par horaire (ou la colonne horaire souhaitée)
  summarise(total = sum(NB_VALD)) %>%
  arrange(desc(total))# Calculer la somme des validations

