library(dplyr)
library(ggplot2)

allDataFrameProfile = rbind(S1_2018_PROFIL_FER,
                            S2_2018_PROFIL_FER,
                            S1_2019_PROFIL_FER,
                            S2_2019_PROFIL_FER,
                            S1_2020_PROFIL_FER,
                            S2_2020_PROFIL_FER,
                            S1_2021_PROFIL_FER,
                            S2_2021_PROFIL_FER,
                            S1_2022_PROFIL_FER,
                            S2_2022_PROFIL_FER)

allDataFrameNB = rbind(S1_2018_NB_FER,
                       S2_2018_NB_FER,
                       S1_2019_NB_FER,
                       S2_2019_NB_FER,
                       S1_2020_NB_FER,
                       S2_2020_NB_FER,
                       S1_2021_NB_FER,
                       S2_2021_NB_FER,
                       S1_2022_NB_FER,
                       S2_2022_NB_FER)