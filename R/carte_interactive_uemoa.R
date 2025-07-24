#' Carte interactive de la consommation de produits de la mer dans l'UEMOA
#'
#' Cette fonction crée une carte interactive affichant la consommation moyenne
#' de produits de la mer dans les pays de l'UEMOA et les régions du Sénégal.
#'
#' @param base_senegal Dataframe contenant les colonnes NOMREG (nom de la région)
#'   et QuantConsKGMoyIndAnnee.
#' @param shapefile_senegal Chemin vers le shapefile des régions du Sénégal.
#'
#' @return Un objet `leaflet` interactif.
#' @export
#'
#' @import leaflet dplyr sf rnaturalearth
#'
#' @examples
#' \dontrun{
#' base_sen <- data.frame(
#'   NOMREG = c("DAKAR", "THIES"),
#'   QuantConsKGMoyIndAnnee = c(20, 18)
#' )
#' carte_interactive_uemoa(
#'   base_senegal = base_sen,
#'   shapefile_senegal = "data/shapefiles/Limite_Région.shp"
#' )
#' }
carte_interactive_uemoa <- function(base_senegal,
                                    shapefile_senegal) {

  # -------------------------------
  # Vérification des colonnes
  # -------------------------------
  if (!all(c("NOMREG", "QuantConsKGMoyIndAnnee") %in% names(base_senegal))) {
    stop("Le dataframe base_senegal doit contenir les colonnes : NOMREG et QuantConsKGMoyIndAnnee.")
  }

  # -------------------------------
  # Charger et harmoniser la carte des pays via rnaturalearth
  # -------------------------------
  shp_uemoa <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    sf::st_transform(crs = 4326)

  # Corriger le nom de la Côte d'Ivoire
  shp_uemoa$admin[shp_uemoa$admin == "Ivory Coast"] <- "Côte d'Ivoire"

  # -------------------------------
  # Liste des pays UEMOA
  # -------------------------------
  pays_uemoa <- c(
    "Senegal", "Benin", "Côte d'Ivoire", "Guinea-Bissau",
    "Togo", "Mali", "Niger", "Burkina Faso"
  )

  # -------------------------------
  # Ajouter données FAO pour l'UEMOA
  # -------------------------------
  shp_uemoa <- shp_uemoa %>%
    dplyr::filter(admin %in% pays_uemoa) %>%
    dplyr::mutate(consommation_mer_kg = dplyr::case_when(
      admin == "Senegal"       ~ 17.8,
      admin == "Benin"         ~ 16.6,
      admin == "Côte d'Ivoire" ~ 22.6,
      admin == "Guinea-Bissau" ~ 11.5,
      admin == "Togo"          ~ 9.4,
      admin == "Mali"          ~ 9.0,
      admin == "Niger"         ~ 5.0,
      admin == "Burkina Faso"  ~ 4.5,
      TRUE                     ~ NA_real_
    ))

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
  # Palettes de couleurs
  # -------------------------------
  pal_pays <- leaflet::colorNumeric(
    palette = "YlOrRd",
    domain = shp_uemoa$consommation_mer_kg,
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
      data = shp_uemoa,
      fillColor = ~pal_pays(consommation_mer_kg),
      color = "black",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.7,
      popup = ~paste0("<b>", admin, "</b><br>Consommation moyenne annuelle par tête : ", consommation_mer_kg, " kg/an")
    ) %>%
    leaflet::addPolygons(
      data = shp_senegal,
      fillColor = ~pal_senegal(QuantConsKGMoyIndAnnee),
      color = "black",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.7,
      popup = ~paste0(
        "<b>", NOMREG, "</b><br>Consommation moyenne annuelle par tête : ",
        round(QuantConsKGMoyIndAnnee, 1), " kg/an"
      )
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      pal = pal_pays,
      values = shp_uemoa$consommation_mer_kg,
      title = "Conso UEMOA (kg/an)"
    ) %>%
    leaflet::addLegend(
      position = "bottomleft",
      pal = pal_senegal,
      values = shp_senegal$QuantConsKGMoyIndAnnee,
      title = "Conso Sénégal (kg/an)"
    )
}
