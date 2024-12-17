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

# Spatial data
SPATIAL_DATA = st_read("REF_ZdA/PL_ZDL_R_17_12_2024.shp", crs=4326)


