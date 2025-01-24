#' Extract trait values for Bioshifts species
#' 
#' @description 
#' This script contains the third step of the workflow:
#' 
#' - Import trait databases `data/derived-data/traitdata-database.qs`
#' - Read the trait metadata from Google Sheet
#' - Extract trait values
#' - Convert trait values
#' - Create long format table
#' - Export table in `outputs/bioshifts_trait_name.qs`
#' 
#' @author Nicolas Casajus \email{rdev.nc@gmail.com}
#' 
#' @date 2024/05/30


trait_names <- c("body_size")


## Extract and convert trait values ----

trait_data <- lapply(trait_names, function(trait) {

  trait |> 
    extract_trait_values() |> 
    convert_trait_units()
})

trait_data <- do.call(rbind.data.frame, trait_data)


## Add classification of Bioshifts species names ----

species_names <- qs::qread(here::here("data", "derived-data", "species_list.qs"))

trait_data <- merge(trait_data, 
                    species_names[ , c("accepted_name", "kingdom", "phylum",
                                       "class", "order", "family")],
                    by = "accepted_name", all = FALSE)


## Save Bioshifts trait database ----

qs::qsave(trait_data, here::here("outputs", "collision_trait_database.qs"))
