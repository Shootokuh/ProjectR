library(dplyr)
library(ggplot2)

# allDataFrameNB$CATEGORIE_TITRE <- ifelse(allDataFrameNB$CATEGORIE_TITRE == "?", "N/A", allDataFrameNB$CATEGORIE_TITRE)

dataByDayYear2018 = ANNUAL_NB_FER_2018 %>% 
                  group_by(JOUR) %>% 
                  summarize(n=n()) %>%
                  arrange(JOUR)

ggplot(dataByDayYear2018) +
  geom_line(aes(x=JOUR,y=n))+
  theme_bw()+
  scale_x_date(
    date_breaks = "2 month",      # Afficher une graduation par semaine
    date_labels = "%d-%b"       # Formater les étiquettes en "jour-mois" (e.g., 01-Jan)
  )+
  xlab("nombre de validations")+ylab("temps")+
  ggtitle("Nombres de validation par jour sur l'année 2018")


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

ANNUAL_NB_FER_2018$WEEK <- strftime(ANNUAL_NB_FER_2018$JOUR, format = "%V")

dataByWeekYear2018 = ANNUAL_NB_FER_2018 %>% 
  group_by(WEEK) %>% 
  summarize(n=n()) %>%
  arrange(WEEK)

ggplot(dataByWeekYear2018, aes(x=WEEK,y=n, group = 1)) +
  geom_line() + # Ligne semaine data
  geom_line() + # Ligne semaine min
  geom_hline(yintercept = minWeek, color = "red", linetype = "dashed") +
  geom_line() + # Ligne semaine max
  geom_hline(yintercept = maxWeek, color = "green", linetype = "dashed") +
  geom_line() + # Ligne semaine mean
  geom_hline(yintercept = meanWeek, color = "blue", linetype = "dashed") +
  theme_bw()+
  xlab("nombre de validations")+ylab("temps")+
  ggtitle("Nombres de validation par jour sur 2018 Inférieur à 4800 validations")

minWeek <- min(dataByWeekYear2018$n)
maxWeek <- max(dataByWeekYear2018$n)
meanWeek <- mean(dataByWeekYear2018$n)

dataAboveMeanByWeekYear2018 = dataByWeekYear2018 %>% filter(n <= meanWeek)

################################################################################

S1_2018_PROFIL_FER$pourc_validations <- gsub(",", ".", S1_2018_PROFIL_FER$pourc_validations)
S1_2018_PROFIL_FER$pourc_validations <- as.numeric(S1_2018_PROFIL_FER$pourc_validations)

scolarPeriod2018 = S1_2018_PROFIL_FER %>% group_by(CAT_JOUR, TRNC_HORR_60) %>% 
                    filter(CAT_JOUR == 'JOVS') %>%
                    summarize(pourc_validations_mean = mean(pourc_validations)) %>%
                    mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60)))

taffPeriod2018 = S1_2018_PROFIL_FER %>% group_by(CAT_JOUR, TRNC_HORR_60) %>% 
                  filter(CAT_JOUR == 'JOHV') %>%
                  summarize(pourc_validations_mean = mean(pourc_validations)) %>%
                  mutate(Heure_Val = as.numeric(gsub("H-.*", "", TRNC_HORR_60)))
                  # À retirer les valeurs dont la TRNC_HORR_60 == "ND"
                  test = S1_2018_PROFIL_FER %>% filter(TRNC_HORR_60 == "ND")

ggplot(scolarPeriod2018, aes(x = Heure_Val, y=pourc_validations_mean, group=1)) +
  geom_line() +
  theme_bw()+
  xlab("Tranche horaire 1H") + ylab("validations / 100")+
  scale_x_continuous(
    breaks = scolarPeriod2018$Heure_Val, 
    labels = scolarPeriod2018$TRNC_HORR_60
  )
  ggtitle("?")





