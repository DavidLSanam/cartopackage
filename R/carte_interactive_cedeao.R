#' Carte interactive de la consommation de produits de la mer dans la CEDEAO
#'
#' Cette fonction crée une carte interactive affichant la consommation moyenne
#' de produits de la mer dans les pays de la CEDEAO et les régions du Sénégal.
#'
#' @param base_senegal Dataframe contenant les colonnes NOMREG (nom de la région) et QuantConsKGMoyIndAnnee.
#' @param shapefile_ce_deao Chemin vers le shapefile des pays CEDEAO.
#' @param shapefile_senegal Chemin vers le shapefile des régions du Sénégal.
#'
#' @return Un objet `leaflet` interactif.
#' @export
#'
#' @import leaflet dplyr sf
#' @examples
#' \dontrun{
#' base_sen <- data.frame(NOMREG = c("DAKAR", "THIES"), QuantConsKGMoyIndAnnee = c(20, 18))
#' carte_interactive_cedeao(
#'   base_senegal = base_sen,
#'   shapefile_ce_deao = "data/shapefiles/wca_admbnda_adm0_ocha_29062021.shp",
#'   shapefile_senegal = "data/shapefiles/Limite_Region.shp"
#' )
#' }
carte_interactive_cedeao <- function(base_senegal,
                                     shapefile_ce_deao,
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
  # Vérification des colonnes
  # -------------------------------
  if (!all(c("NOMREG", "QuantConsKGMoyIndAnnee") %in% names(base_senegal))) {
    stop("Le dataframe base_senegal doit contenir les colonnes : NOMREG et QuantConsKGMoyIndAnnee.")
  }

  # -------------------------------
  # Charger et reprojeter les shapefiles
  # -------------------------------
  shp_ce_deao <- sf::st_read(shapefile_ce_deao, quiet = TRUE) %>%
    sf::st_transform(crs = 4326)

  shp_senegal <- sf::st_read(shapefile_senegal, quiet = TRUE) %>%
    sf::st_transform(crs = 4326)

  # -------------------------------
  # Liste des pays CEDEAO
  # -------------------------------
  pays_cedeao <- c(
    "Benin", "Burkina Faso", "Cabo Verde", "Côte d'Ivoire",
    "Gambia", "Ghana", "Guinea", "Guinea Bissau", "Liberia",
    "Nigeria", "Sierra Leone", "Togo", "Senegal"
  )

  # -------------------------------
  # Ajouter données FAO pour la CEDEAO
  # -------------------------------
  shp_ce_deao <- shp_ce_deao %>%
    dplyr::filter(admin0Name %in% pays_cedeao) %>%
    dplyr::mutate(consommation_mer_kg = dplyr::case_when(
      admin0Name == "Sierra Leone"     ~ 25.3,
      admin0Name == "Ghana"            ~ 24.6,
      admin0Name == "Gambia"           ~ 24.3,
      admin0Name == "Côte d'Ivoire"    ~ 22.6,
      admin0Name == "Senegal"          ~ 17.8,
      admin0Name == "Benin"            ~ 16.6,
      admin0Name == "Guinea Bissau"    ~ 11.5,
      admin0Name == "Cabo Verde"       ~ 9.7,
      admin0Name == "Togo"             ~ 9.4,
      admin0Name == "Liberia"          ~ 8.5,
      admin0Name == "Nigeria"          ~ 6.3,
      TRUE                             ~ NA_real_
    ))

  # -------------------------------
  # Associer les données du Sénégal
  # -------------------------------
  shp_senegal <- shp_senegal %>%
    dplyr::left_join(base_senegal, by = "NOMREG")

  if (all(is.na(shp_senegal$QuantConsKGMoyIndAnnee))) {
    warning("Aucune donnée valide de consommation trouvée pour les régions du Sénégal.")
  }

  # -------------------------------
  # Consommation nationale Sénégal
  # -------------------------------
  conso_nationale_sn <- shp_ce_deao %>%
    dplyr::filter(admin0Name == "Senegal") %>%
    dplyr::pull(consommation_mer_kg)

  # -------------------------------
  # Palettes de couleurs
  # -------------------------------
  pal_pays <- leaflet::colorNumeric(
    palette = "YlOrRd",
    domain = shp_ce_deao$consommation_mer_kg,
    na.color = "transparent"
  )

  pal_senegal <- leaflet::colorNumeric(
    palette = "Blues",
    domain = shp_senegal$QuantConsKGMoyIndAnnee,
    na.color = "transparent"
  )

  # -------------------------------
  # Construction de la carte
  # -------------------------------
  leaflet::leaflet() %>%
    leaflet::addProviderTiles("CartoDB.Positron") %>%
    leaflet::addPolygons(
      data = shp_ce_deao,
      fillColor = ~pal_pays(consommation_mer_kg),
      color = "black",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.7,
      group = "Pays",
      label = ~admin0Name,
      popup = ~paste0("<b>", admin0Name, "</b><br>Consommation moyenne : ",
                      consommation_mer_kg, " kg/an")
    ) %>%
    leaflet::addPolygons(
      data = shp_senegal,
      fillColor = ~pal_senegal(QuantConsKGMoyIndAnnee),
      color = "black",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.7,
      group = "Régions Sénégal",
      label = ~NOMREG,
      popup = ~paste0("<b>", NOMREG, " (Sénégal)</b><br>",
                      "Consommation régionale : ", round(QuantConsKGMoyIndAnnee, 1), " kg/an<br>",
                      "Consommation nationale : ", conso_nationale_sn, " kg/an")
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      pal = pal_pays,
      values = shp_ce_deao$consommation_mer_kg,
      title = "Consommation pays CEDEAO (kg/an)"
    ) %>%
    leaflet::addLegend(
      position = "bottomleft",
      pal = pal_senegal,
      values = shp_senegal$QuantConsKGMoyIndAnnee,
      title = "Consommation régions Sénégal (kg/an)"
    ) %>%
    leaflet::addLayersControl(
      overlayGroups = c("Pays", "Régions Sénégal"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet.extras::addSearchFeatures(
      targetGroups = c("Pays", "Régions Sénégal"),
      options = leaflet.extras::searchFeaturesOptions(
        zoom = 5, openPopup = TRUE, firstTipSubmit = TRUE
      )
    )
}
