% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/carte_interactive_uemoa.R
\name{carte_interactive_uemoa}
\alias{carte_interactive_uemoa}
\title{Carte interactive de la consommation de produits de la mer dans l'UEMOA}
\usage{
carte_interactive_uemoa(base_senegal, shapefile_senegal)
}
\arguments{
\item{base_senegal}{Dataframe contenant les colonnes NOMREG (nom de la région)
et QuantConsKGMoyIndAnnee.}

\item{shapefile_senegal}{Chemin vers le shapefile des régions du Sénégal.}
}
\value{
Un objet \code{leaflet} interactif.
}
\description{
Cette fonction crée une carte interactive affichant la consommation moyenne
de produits de la mer dans les pays de l'UEMOA et les régions du Sénégal.
}
\examples{
\dontrun{
base_sen <- data.frame(
  NOMREG = c("DAKAR", "THIES"),
  QuantConsKGMoyIndAnnee = c(20, 18)
)
carte_interactive_uemoa(
  base_senegal = base_sen,
  shapefile_senegal = "data/shapefiles/Limite_Region.shp"
)
}
}
