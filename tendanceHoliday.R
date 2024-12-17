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


