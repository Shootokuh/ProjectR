library(dplyr)

# Annual NB_FER
ANNUAL_NB_FER_2018 = rbind(S1_2018_NB_FER,S2_2018_NB_FER)
ANNUAL_NB_FER_2019 = rbind(S1_2019_NB_FER,S2_2019_NB_FER)
ANNUAL_NB_FER_2020 = rbind(S1_2020_NB_FER,S2_2020_NB_FER)
ANNUAL_NB_FER_2021 = rbind(S1_2021_NB_FER,S2_2021_NB_FER)
ANNUAL_NB_FER_2022 = rbind(S1_2022_NB_FER,S2_2022_NB_FER)

# Annual PROFIL_FER
ANNUAL_PROFIL_FER_2018 = rbind(S1_2018_PROFIL_FER, S2_2018_PROFIL_FER)
ANNUAL_PROFIL_FER_2019 = rbind(S1_2019_PROFIL_FER, S2_2019_PROFIL_FER)
ANNUAL_PROFIL_FER_2020 = rbind(S1_2020_PROFIL_FER, S2_2020_PROFIL_FER)
ANNUAL_PROFIL_FER_2021 = rbind(S1_2021_PROFIL_FER, S2_2021_PROFIL_FER)
ANNUAL_PROFIL_FER_2022 = rbind(S1_2022_PROFIL_FER, S2_2022_PROFIL_FER)

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

allDataFrameNB <- do.call(rbind, years_data_nb)

allDataFrameProfile = do.call(rbind, years_data_profil)