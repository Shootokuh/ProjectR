library(readr)

years <- 2018:2023
semesters <- c("S1", "S2")

for (year in years) {
  for (semester in semesters) {
    nb_fer_file <- paste0("data-rf-", year, "/", year, "_", semester, "_NB_FER.txt")
    profil_fer_file <- paste0("data-rf-", year, "/", year, "_", semester, "_PROFIL_FER.txt")
    
    if (file.exists(nb_fer_file)) {
      assign(paste0(semester, "_", year, "_NB_FER"), read_delim(nb_fer_file, delim = "\t"))
    }
    if (file.exists(profil_fer_file)) {
      assign(paste0(semester, "_", year, "_PROFIL_FER"), read_delim(profil_fer_file, delim = "\t"))
    }
  }
}

S1_2023_VALIDATIONS_RESEAU = read_csv2("validations-reseau-ferre-nombre-validations-par-jour-1er-semestre.csv")

