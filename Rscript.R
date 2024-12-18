library(readr)
library(sf)
library(dplyr)
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
  if (year != 2023) {
    # Ajouter les données spatiales à chaque variables
    # Annual NB_FER
    S1 <- paste0("S1_", year, "_NB_FER")
    S2 <- paste0("S2_", year, "_NB_FER")
    vectorS1 <- get(S1)
    vectorS1$JOUR <- as.Date(vectorS1$JOUR, format = "%d/%m/%Y")
    vectorS2 <- get(S2)
    vectorS2$JOUR <- as.Date(vectorS2$JOUR, format = "%d/%m/%Y")
    assign(paste0("ANNUAL_NB_FER_", year), rbind(vectorS1, vectorS2))
  }
}

colnames(S2_2022_PROFIL_FER)[colnames(S2_2022_PROFIL_FER) == "lda"] <- "ID_REFA_LDA"
colnames(S2_2022_NB_FER)[colnames(S2_2022_NB_FER) == "lda"] <- "ID_REFA_LDA"

# Spatial data
SPATIAL_DATA = st_read("REF_ZdA/PL_ZDL_R_17_12_2024.shp", crs=4326)

colnames(SPATIAL_DATA)[colnames(SPATIAL_DATA) == "idrefa_lda"] <- "ID_REFA_LDA"

#Aggregate spatial data with data
ANNUAL_NB_FER_2018 = left_join(ANNUAL_NB_FER_2018, SPATIAL_DATA, by=c("ID_REFA_LDA" = "ID_REFA_LDA"))
