% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/carte_interactive_afrique.R
\name{carte_interactive_afrique}
\alias{carte_interactive_afrique}
\title{Carte interactive de la consommation de produits de la mer en Afrique}
\usage{
carte_interactive_afrique(base_senegal_path, shapefile_senegal)
}
\arguments{
\item{base_senegal_path}{Chemin vers la base \code{.dta} contenant les données
de consommation pour le Sénégal (sera préparée automatiquement).}

\item{shapefile_senegal}{Chemin vers le shapefile des régions du Sénégal.}
}
\value{
Un objet \code{leaflet} interactif.
}
\description{
Cette fonction crée une carte interactive pour l'Afrique avec des données FAO
sur la consommation de produits de la mer par pays. Pour le Sénégal, les régions
sont affichées avec des données détaillées.
}
\examples{
\dontrun{
carte <- carte_interactive_afrique(
  base_senegal_path = "Base_X1_Appuree.dta",
  shapefile_senegal = "shapefiles/Limite_Region.shp"
)
}
}
