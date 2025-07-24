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

### **1. Depuis GitHub**

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
### 1. Carte CEDEAO

library(cartopackage)

base_senegal <- preparer_base_senegal("BASE_X1_Appuree.dta") # Ou une autre base du même type e votre choix

carte <- carte_interactive_cedeao(
  base_senegal,
  shapefile_ce_deao = "shapefiles/wca_admbnda_adm0_ocha_29062021.shp",
  shapefile_senegal = "shapefiles/Limite_Region.shp"
)

carte  # Affiche la carte interactive


### 2. Carte UEMOA

base_senegal <- preparer_base_senegal("BASE_X1_Appuree.dta")  # Ou une autre base du même type e votre choix

carte <- carte_interactive_uemoa(
  base_senegal = base_senegal,
  shapefile_senegal = "shapefiles/Limite_Region.shp"
)

carte


### 3. Carte Afrique

carte <- carte_interactive_afrique(
  base_senegal_path = "BASE_X1_Appuree.dta",
  shapefile_senegal = "shapefiles/Limite_Region.shp"
)

carte


## Auteurs

David Landry SANAM
Leslye Patricia NKWA
Heman Parfait YAMAHA
Michel TEVOEDJRE

## Licence
Ce projet est sous licence MIT – Vous êtes libre de l’utiliser et de le modifier avec attribution.
