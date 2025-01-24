#' Clean species names
#' 
#' @description 
#' This script contains the first step of the workflow:
#' 
#' - Import `data/species_list.csv`
#' - Extract original names
#' - Clean original names
#' - Retrieve accepted names
#' - Export table in `data/derived-data/bioshifts-species_list.qs`
#' 
#' @author Nicolas Casajus \email{rdev.nc@gmail.com}
#' 
#' @date 2024/05/30

## Clean Bioshifts species names ----

species_list <- get_species_names() |> 
  clean_species_names() |> 
  retrieve_accepted_names(species_only = FALSE)


## Save species list ----

dir.create(here::here("data", "derived-data"), showWarnings = FALSE)

qs::qsave(x    = species_list, 
          file = here::here("data", "derived-data", 
                            "species_list.qs"))


## Clean workspace ----

rm(species_list, envir = .GlobalEnv)
gc(verbose = FALSE)
