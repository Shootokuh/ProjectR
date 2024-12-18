library(readr)
library(sf)
library(dplyr)

# Supprime toute les données 
rm(list = ls())

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


# Spatial data
SPATIAL_DATA = st_read("REF_ZdA/PL_ZDL_R_17_12_2024.shp", crs=4326)

colnames(SPATIAL_DATA)[colnames(SPATIAL_DATA) == "idrefa_lda"] <- "ID_REFA_LDA"

# On left_join la spatial data sur tout les ANNUAL_NB_FER
for (year in 2018:2022) {
  annual_nb_fer <- get(paste0("ANNUAL_NB_FER_", year))
  annual_nb_fer <- left_join(annual_nb_fer, SPATIAL_DATA, by = c("ID_REFA_LDA" = "ID_REFA_LDA"))
  assign(paste0("ANNUAL_NB_FER_", year), annual_nb_fer)
}

#On enregistre tout les NB fer dans une liste
years_data_nb <- list(ANNUAL_NB_FER_2018, ANNUAL_NB_FER_2019, ANNUAL_NB_FER_2020, 
                      ANNUAL_NB_FER_2021, ANNUAL_NB_FER_2022)

#On enregistre tout les NB profil dans une liste
years_data_profil <- list(ANNUAL_PROFIL_FER_2018, ANNUAL_PROFIL_FER_2019, ANNUAL_PROFIL_FER_2020, 
                          ANNUAL_PROFIL_FER_2021, ANNUAL_PROFIL_FER_2022)

# On change pour chaque année les données dans CATEGORIE_TITRE par autre
for (i in seq_along(years_data_nb)) {
  years_data_nb[[i]] <- years_data_nb[[i]] %>%
    mutate(CATEGORIE_TITRE = recode(CATEGORIE_TITRE,
                                    "?" = "autre",
                                    "NON DEFINI" = "autre",
                                    "AUTRE TITRE" = "autre"))
}

# Réassigner les changements a la data d'origine
ANNUAL_NB_FER_2018 <- years_data_nb[[1]]
ANNUAL_NB_FER_2019 <- years_data_nb[[2]]
ANNUAL_NB_FER_2020 <- years_data_nb[[3]]
ANNUAL_NB_FER_2021 <- years_data_nb[[4]]
ANNUAL_NB_FER_2022 <- years_data_nb[[5]]

#allDataFrameNB <- do.call(rbind, years_data_nb)

#allDataFrameProfile = do.call(rbind, years_data_profil)
