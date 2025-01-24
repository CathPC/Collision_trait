#' TraitCompilation: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Nicolas Casajus \email{rdev.nc@gmail.com}
#' 
#' @date 2024/05/29


# Project setup ----

## Install Dependencies (listed in DESCRIPTION) ----

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here())


# Run project ----

source(here::here("analyses", "1-clean_bioshifts_species_names.R"))
source(here::here("analyses", "2-subset_trait_databases.R"))
source(here::here("analyses", "3-extract_trait_values.R"))
