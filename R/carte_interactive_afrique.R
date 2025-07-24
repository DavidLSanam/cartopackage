#' Carte interactive de la consommation de produits de la mer en Afrique
#'
#' Cette fonction crée une carte interactive pour l'Afrique avec des données FAO
#' sur la consommation de produits de la mer par pays. Pour le Sénégal, les régions
#' sont affichées avec des données détaillées.
#'
#' @param base_senegal_path Chemin vers la base `.dta` contenant les données
#'   de consommation pour le Sénégal (sera préparée automatiquement).
#' @param shapefile_senegal Chemin vers le shapefile des régions du Sénégal.
#'
#' @return Un objet `leaflet` interactif.
#' @export
#'
#' @import leaflet dplyr sf rnaturalearth
#' @examples
#' \dontrun{
#' carte <- carte_interactive_afrique(
#'   base_senegal_path = "Base_X1_Appuree.dta",
#'   shapefile_senegal = "shapefiles/Limite_Region.shp"
#' )
#' }
carte_interactive_afrique <- function(base_senegal_path,
                                      shapefile_senegal) {
  # Fonction pour installer un package si nécessaire
  installer_si_absent <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }

  # Installation conditionnelle des packages
  installer_si_absent("leaflet")
  installer_si_absent("leaflet.extras")
  installer_si_absent("sf")
  installer_si_absent("rnaturalearth")
  installer_si_absent("dplyr")
  installer_si_absent("tibble")

  # Chargement des bibliothèques
  library(leaflet)
  library(leaflet.extras)
  library(dplyr)
  library(sf)
  library(rnaturalearth)
  library(tibble)

  # -------------------------------
  # Préparer la base Sénégal
  # -------------------------------
  base_senegal <- preparer_base_senegal(base_senegal_path)

  # -------------------------------
  # Charger le shapefile du Sénégal
  # -------------------------------
  shp_senegal <- sf::st_read(shapefile_senegal, quiet = TRUE) %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::left_join(base_senegal, by = "NOMREG")

  if (all(is.na(shp_senegal$QuantConsKGMoyIndAnnee))) {
    warning("Aucune donnée valide de consommation trouvée pour les régions du Sénégal.")
  }

  # -------------------------------
  # Charger la carte des pays africains
  # -------------------------------
  shp_afrique <- rnaturalearth::ne_countries(continent = "africa",
                                             returnclass = "sf") %>%
    sf::st_transform(crs = 4326)

  # -------------------------------
  # Données de consommation FAO (pays)
  # -------------------------------
  conso_data <- tibble::tribble(
    ~admin, ~consommation_mer_kg,
    "Senegal", 17.8, "Benin", 16.6, "Cote d'Ivoire", 22.6,
    "Guinea Bissau", 11.5, "Sierra Leone", 25.3,
    "Togo", 9.4, "Mali", 9.0,
    "Niger", 5.0, "Burkina Faso", 4.5, "Algeria", 7.4,
    "Angola", 10.7, "Botswana", 2.4, "Burundi", 4.0,
    "Cabo Verde", 9.7, "Cameroon", 14.2, "Central African Republic", 3.2,
    "Chad", 3.2, "Comoros", 11.8, "Republic of the Congo", 8.1,
    "Democratic Republic of the Congo", 9.3, "Djibouti", 4.8,
    "Egypt", 17.3, "Equatorial Guinea", 12.0, "Eritrea", 4.5,
    "Eswatini", 2.9, "Ethiopia", 4.5, "Gabon", 15.0,
    "Gambia", 24.3, "Ghana", 24.6, "Guinea", 11.0,
    "Kenya", 4.8, "Lesotho", 2.9, "Liberia", 8.5,
    "Libya", 2.4, "Madagascar", 14.8, "Malawi", 10.1,
    "Mauritania", 15.5, "Mauritius", 13.3, "Morocco", 12.5,
    "Mozambique", 13.1, "Namibia", 11.7, "Nigeria", 6.3,
    "Rwanda", 4.0, "Sao Tome and Principe", 18.0,
    "Seychelles", 33.0, "Somalia", 20.0, "South Africa", 11.2,
    "South Sudan", 3.5, "Sudan", 3.2, "Tanzania", 9.6,
    "Tunisia", 7.4, "Uganda", 4.8, "Zambia", 9.3,
    "Zimbabwe", 9.1
  )

  # -------------------------------
  # Harmoniser les noms des pays
  # -------------------------------
  shp_afrique <- shp_afrique %>%
    mutate(admin = case_when(
      admin == "Ivory Coast" ~ "Cote d'Ivoire",
      admin == "Guinea-Bissau" ~ "Guinea Bissau",
      admin == "Eswatini (Swaziland)" ~ "Eswatini",
      admin == "United Republic of Tanzania" ~ "Tanzania",
      TRUE ~ admin
    )) %>%
    left_join(conso_data, by = "admin")

  # -------------------------------
  # Palettes de couleurs
  # -------------------------------
  pal_afrique <- colorNumeric(
    palette = "YlOrRd",
    domain = shp_afrique$consommation_mer_kg,
    na.color = "transparent"
  )

  pal_senegal <- colorNumeric(
    palette = "Blues",
    domain = shp_senegal$QuantConsKGMoyIndAnnee,
    na.color = "transparent"
  )

  conso_nationale_sn <- conso_data %>%
    filter(admin == "Senegal") %>%
    pull(consommation_mer_kg)

  # -------------------------------
  # Construction de la carte
  # -------------------------------
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      data = shp_afrique,
      fillColor = ~pal_afrique(consommation_mer_kg),
      color = "black", weight = 1, opacity = 1, fillOpacity = 0.7,
      group = "Pays",
      label = ~admin,
      popup = ~paste0("<b>", admin, "</b><br>",
                      "Consommation : ", consommation_mer_kg, " kg/an")
    ) %>%
    addPolygons(
      data = shp_senegal,
      fillColor = ~pal_senegal(QuantConsKGMoyIndAnnee),
      color = "black", weight = 1, opacity = 1, fillOpacity = 0.7,
      group = "Régions Sénégal",
      label = ~NOMREG,
      popup = ~paste0("<b>", NOMREG, " (Sénégal)</b><br>",
                      "Consommation régionale : ", round(QuantConsKGMoyIndAnnee, 1), " kg/an<br>",
                      "Consommation nationale : ", conso_nationale_sn, " kg/an")
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal_afrique,
      values = shp_afrique$consommation_mer_kg,
      title = "Consommation moyenne - pays (kg/an)"
    ) %>%
    addLegend(
      position = "bottomleft",
      pal = pal_senegal,
      values = shp_senegal$QuantConsKGMoyIndAnnee,
      title = "Consommation régionale - Sénégal (kg/an)"
    ) %>%
    addLayersControl(
      overlayGroups = c("Pays", "Régions Sénégal"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addSearchFeatures(
      targetGroups = c("Pays", "Régions Sénégal"),
      options = searchFeaturesOptions(zoom = 5, openPopup = TRUE, firstTipSubmit = TRUE)
    )
}
