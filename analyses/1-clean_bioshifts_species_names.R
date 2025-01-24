#' Clean Bioshifts species names
#' 
#' @description 
#' This script contains the first step of the workflow:
#' 
#' - Import `data/bioshifts_v3_harmonized.csv`
#' - Extract original names
#' - Clean original names
#' - Retrieve accepted names
#' - Export table in `data/derived-data/bioshifts-species_list.qs`
#' 
#' @author Nicolas Casajus \email{rdev.nc@gmail.com}
#' 
#' @date 2024/05/30

## Clean Bioshifts species names ----

bioshifts_species <- get_bioshifts_species() |> 
  clean_species_names() |> 
  retrieve_accepted_names()


## Save species list ----

dir.create(here::here("data", "derived-data"), showWarnings = FALSE)

qs::qsave(x    = bioshifts_species, 
          file = here::here("data", "derived-data", 
                            "bioshifts-species_list.qs"))


## Clean workspace ----

rm(bioshifts_species, envir = .GlobalEnv)
gc(verbose = FALSE)
