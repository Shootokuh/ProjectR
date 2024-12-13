# Analyse et Visualisation des Modèles de Fréquentation du Réseau Ferré d'Île-de-France

**AUTEUR**  
Etienne Côme  

**PUBLIÉ**  
12 novembre 2024  

## Présentation du Projet
Dans ce projet d’analyse de données, les étudiants exploreront les données de fréquentation des gares ferroviaires d’Île-de-France sur la période 2018-2023. L’objectif principal est d’analyser et de visualiser les modèles de fréquentation, en créant un tableau de bord permettant aux parties prenantes de surveiller et de comparer la fréquentation par rapport à la normale. L’analyse se concentrera spécifiquement sur la mise en évidence des variations par rapport à une semaine type, en distinguant entre les semaines normales et les périodes de vacances.

## Tâches du Projet

### 1. Collecte et Nettoyage des Données
- Les données utilisées pour cette étude sont disponibles via le portail Open Data STIF :
  - Validation par jour et par point d’arrêt pour le premier semestre 2023
  - Validation par jour et par point d’arrêt de 2015 à 2022
  - Localisation des zones d’arrêt, pour ce dernier fichier les données spatiales peuvent être téléchargées directement [ici](https://eu.ftp.opendatasoft.com/stif/Reflex/REF_ZdA.zip)
  - Autres ressources disponibles sur le site Open Data du STIF
- Rassembler et compiler les données de fréquentation des gares ferroviaires d’Île-de-France pour la période spécifiée, à partir du 01/01/2018.
- Nettoyer les données pour gérer les valeurs manquantes, les aberrations, et les incohérences.
- Agréger les données au niveau de la “Zone d’arrêt” définie par la caractéristique ID_REFA_LDA et collecter les données géographiques de leur localisation.
- Fournir un script R reproductible nommé `cleaning.R` regroupant toutes les étapes de nettoyage.

### 2. Analyse Exploratoire des Données (EDA)
- Effectuer une analyse exploratoire pour identifier les tendances générales et les modèles de fréquentation.
- Explorer la saisonnalité, les tendances mensuelles, et les aberrations potentielles affectant les données.

### 3. Comparaison avec la Norme
- Définir une semaine type “normale” et étudier les écarts pendant les périodes de vacances et hors vacances.
- Évaluer l’impact des vacances et des périodes scolaires sur les modèles de fréquentation.

### 4. Développement du Tableau de Bord avec Shiny
- Construire un tableau de bord interactif à l’aide du framework Shiny en R.
- Inclure des visualisations clés montrant les tendances générales de fréquentation, les variations hebdomadaires, et les comparaisons avec la norme.
- Exemples de fonctionnalités :
  - Permettre à l’utilisateur de sélectionner une période de référence et une période à comparer, et fournir des statistiques significatives pour mettre en évidence les différences entre ces deux périodes, en distinguant les jours de la semaine.
  - Permettre à l’utilisateur de sélectionner rapidement une gare d’intérêt avec une carte interactive et fournir des statistiques clés sur les tendances actuelles pour cette gare.

### 5. Méthodes Statistiques
- Appliquer des méthodes statistiques simples pour valider les résultats et tirer des conclusions significatives des données.
- Utiliser des tests statistiques pour évaluer la significativité des variations observées, lorsque c’est possible.

### 6. Rapport et Documentation
- Rédiger un rapport détaillé décrivant l’ensemble du processus d’analyse, les méthodologies utilisées, et les principaux résultats.
- Inclure des informations sur l’évolution de la fréquentation du réseau ferroviaire sur la période étudiée.

### 7. Déploiement de l’Application Shiny
- Déployer le tableau de bord Shiny sur shinyapp.io pour une accessibilité et une interaction utilisateur accrues.
- S’assurer que l’application est conviviale et capable de fournir des informations dynamiques sur les modèles de fréquentation.

## Livrables
- Un script R commenté regroupant toutes les étapes de collecte et de prétraitement développées.
- Un rapport bien documenté au format quarto incluant le code, l’analyse et les interprétations.
- Un tableau de bord Shiny fonctionnel accessible via shinyapp.io.

## Date Limite
8 janvier 2024 par e-mail

## Taille du Groupe
3 à 4 étudiants par groupe.

**Note :** Le projet encourage le travail collaboratif, la pensée critique, et l’application de techniques d’analyse de données à des données de transport réelles.
