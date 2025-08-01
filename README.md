# cartopackage

**cartopackage** est un package R conçu pour visualiser la **consommation de produits de la mer** en Afrique, avec un focus particulier sur les pays de la **CEDEAO**, de l’**UEMOA** et sur les **Regions du Sénégal**.

Il propose des **cartes interactives** basées sur **Leaflet** qui affichent la consommation moyenne annuelle par tête, à la fois au niveau national et régional.

---

## **Fonctionnalités principales**

- **`carte_interactive_afrique()`**  
  Crée une carte interactive couvrant **toute l'Afrique** avec les données de consommation FAO par pays et les Regions du Sénégal.

- **`carte_interactive_cedeao()`**  
  Affiche la **consommation moyenne des pays de la CEDEAO** et les régions du Sénégal.

- **`carte_interactive_uemoa()`**  
  Affiche la **consommation moyenne des pays de l’UEMOA** et les régions du Sénégal.

- **`preparer_base_senegal()`**  
  Prépare automatiquement la base de données régionale du Sénégal (à partir d’un fichier `.dta`) pour l’intégrer dans les cartes.

---

## **Installation**

Télécharger et installer Rtools 4.5 via le lien suivant : https://cran.r-project.org/bin/windows/Rtools/

Ensuite, installer les packages necessaires : 
install.packages(c("rnaturalearth", "leaflet", "leaflet.extras", "dplyr", "sf", "haven", "tibble", "rnaturalearthdata"))

Puis, faire 
library(rnaturalearth)      # Accès aux données géospatiales mondiales (pays, régions)
library(leaflet)            # Création de cartes interactives
library(leaflet.extras)     # Fonctionnalités supplémentaires pour Leaflet
library(dplyr)              # Manipulation de données (sélection, filtrage, agrégation)
library(sf)                 # Manipulation de données géospatiales (Simple Features)
library(haven)              # Lecture/écriture de fichiers .dta (Stata), .sav (SPSS), etc.
library(tibble)             # Amélioration des data.frames (affichage, manipulation)
library(rnaturalearthdata)  # Données supplémentaires pour rnaturalearth

### ** Depuis GitHub**

# Installer remotes si nécessaire
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Installer le package depuis GitHub
remotes::install_github("DavidLSanam/cartopackage")



## Dépendances
Le package utilise les librairies suivantes :

leaflet – Cartes interactives.

leaflet.extras – Fonctions avancées (recherche sur la carte).

sf – Gestion des données géospatiales.

rnaturalearth – Données géopolitiques.

dplyr et tibble – Manipulation des données.


## Exemples d’utilisation
### 1. Carte CEDEAO et UEMOA

library(cartopackage)

# Préparer la base Sénégal à partir du package
base_senegal <- preparer_base_senegal(
  system.file("BASE_X1_Appuree.dta", package = "cartopackage")
)

# Carte interactive CEDEAO
carte_cedeao <- carte_interactive_cedeao(
  base_senegal = base_senegal,
  shapefile_ce_deao = system.file("shapefiles/wca_admbnda_adm0_ocha_29062021.shp", package = "cartopackage"),
  shapefile_senegal = system.file("shapefiles/Limite_Region.shp", package = "cartopackage")
)

carte_cedeao

htmlwidgets::saveWidget(carte_cedeao, "carte_interactive_cedeao.html") # Enregistrement de la carte

# Carte interactive UEMOA
carte_uemoa <- carte_interactive_uemoa(
  base_senegal = base_senegal,
  shapefile_senegal = system.file("shapefiles/Limite_Region.shp", package = "cartopackage")
)

carte_uemoa

htmlwidgets::saveWidget(carte_uemoa, "carte_interactive_uemoa.html") # Enregistrement de la carte

### 3. Carte Afrique

library(cartopackage)

base_senegal <- preparer_base_senegal(
  system.file("BASE_X1_Appuree.dta", package = "cartopackage")
)

carte <- carte_interactive_afrique(
  base_senegal_path = system.file("BASE_X1_Appuree.dta", package = "cartopackage"),
  shapefile_senegal = system.file("shapefiles/Limite_Region.shp", package = "cartopackage")
)

carte

htmlwidgets::saveWidget(carte, "carte_interactive_afrique.html") # Enregistrement de la carte


## Auteurs

David Landry SANAM
Leslye Patricia NKWA
Heman Parfait YAMAHA
Michel TEVOEDJRE

## Licence
Ce projet est sous licence MIT – Vous êtes libre de l’utiliser et de le modifier avec attribution.
