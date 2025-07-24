#' Préparer la base du Sénégal pour la cartographie
#'
#' Cette fonction lit un fichier `.dta`, agrège les données par ménage et calcule
#' la consommation moyenne annuelle par individu et par région.
#'
#' @param chemin_dta Chemin du fichier Stata (.dta).
#'
#' @return Un dataframe avec deux colonnes : `NOMREG` (noms des régions) et
#'         `QuantConsKGMoyIndAnnee` (quantité moyenne annuelle en kg).
#' @export
#'
#' @import haven dplyr
#'
#' @examples
#' \dontrun{
#' base_senegal <- preparer_base_senegal("BASE_X1_Appuree.dta")
#' }
preparer_base_senegal <- function(chemin_dta) {
  library(haven)
  library(dplyr)

  # --- 1. Lecture de la base ---
  base <- read_dta(chemin_dta)

  colonnes_req <- c("IDs", "QuantiteConsommee", "ValeurConsommee",
                    "QuantiteConsommeeKG", "Calories", "Source",
                    "Unite", "Taille", "VUy", "Taille_Menage",
                    "CaloriesParTete", "HorsPlageCalories",
                    "region", "departement", "milieu")
  if (!all(colonnes_req %in% names(base))) {
    stop("Colonnes manquantes dans la base : ",
         paste(setdiff(colonnes_req, names(base)), collapse = ", "))
  }

  # --- 2. Agrégation par ménage ---
  base <- base %>%
    group_by(IDs) %>%
    summarise(
      QuantiteConsommee   = sum(QuantiteConsommee, na.rm = TRUE),
      ValeurConsommee     = sum(ValeurConsommee, na.rm = TRUE),
      QuantiteConsommeeKG = sum(QuantiteConsommeeKG, na.rm = TRUE),
      Calories            = sum(Calories, na.rm = TRUE),
      Source              = paste0(unique(Source[QuantiteConsommee > 0]), collapse = ", "),
      Unite               = first(Unite),
      Taille              = first(Taille),
      VUy                 = first(VUy),
      Taille_Menage       = first(Taille_Menage),
      CaloriesParTete     = first(CaloriesParTete),
      HorsPlageCalories   = first(HorsPlageCalories),
      region              = first(region),
      departement         = first(departement),
      milieu              = first(milieu),
      .groups = "drop"
    )

  # --- 3. Quantité par membre ---
  base <- base %>%
    mutate(QuantiteParMembre = QuantiteConsommeeKG / Taille_Menage) %>%
    select(IDs, QuantiteParMembre, region)

  # --- 4. Moyenne annuelle par région ---
  base <- base %>%
    group_by(region) %>%
    summarise(QuantConsoKGMoyInd = mean(QuantiteParMembre, na.rm = TRUE), .groups = "drop") %>%
    mutate(QuantConsKGMoyIndAnnee = QuantConsoKGMoyInd * 52) %>%
    select(-QuantConsoKGMoyInd) %>%
    filter(!is.na(region))

  # --- 5. Mapping des codes de région ---
  code_to_region <- c(
    "1" = "DAKAR", "2" = "ZIGUINCHOR", "3" = "DIOURBEL",
    "4" = "SAINT LOUIS", "5" = "TAMBACOUNDA",
    "6" = "KAOLACK", "7" = "THIES", "8" = "LOUGA",
    "9" = "FATICK", "10" = "KOLDA", "11" = "MATAM",
    "12" = "KAFFRINE", "13" = "KEDOUGOU", "14" = "SEDHIOU"
  )

  base <- base %>%
    mutate(region = as.character(region),
           NOMREG = code_to_region[region]) %>%
    select(NOMREG, QuantConsKGMoyIndAnnee)

  # Vérifier les codes non mappés
  if (any(is.na(base$NOMREG))) {
    warning("Certaines régions n'ont pas été mappées correctement : ",
            paste(unique(base$region[is.na(base$NOMREG)]), collapse = ", "))
  }

  return(base)
}
