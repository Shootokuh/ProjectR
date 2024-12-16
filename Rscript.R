	library(readr)

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
}

colnames(S2_2022_PROFIL_FER)[colnames(S2_2022_PROFIL_FER) == "lda"] <- "ID_REFA_LDA"
colnames(S2_2022_NB_FER)[colnames(S2_2022_NB_FER) == "lda"] <- "ID_REFA_LDA"
