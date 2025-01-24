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


trait_names <- c("test")


## Extract and convert trait values ----

trait_data <- lapply(trait_names, function(trait) {

  trait |> 
    extract_trait_values() |> 
    convert_trait_units()
})

trait_data <- do.call(rbind.data.frame, trait_data)


## Add classification of Bioshifts species names ----

bioshifts_species <- qs::qread(here::here("data", "derived-data", 
                                          "bioshifts-species_list.qs"))

trait_data <- merge(trait_data, 
                    bioshifts_species[ , c("accepted_name", "kingdom", "phylum",
                                           "class", "order", "family")],
                    by = "accepted_name", all = FALSE)


## Fix bug for Fishes (need to be improved) ----

pos <- which(is.na(trait_data$"class"))

if (length(pos) > 0) {
  trait_data[pos, "class"] <- "Actinopteri"
}


## Save Bioshifts trait database ----

qs::qsave(trait_data, here::here("outputs", "bioshifts_trait_database.qs"))


## Explore data ----

# ggplot2::ggplot(trait_data, ggplot2::aes(x = family, 
#                                          y = bioshifts_trait_value)) +
#   ggplot2::geom_boxplot() +
#   ggplot2::facet_wrap(.~ class, scales = "free") +
#   ggplot2::theme_light() + 
#   ggplot2::labs(y = "Body size (in mm)") +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, 
#                                                      size = 5),
#                  axis.title.x = ggplot2::element_blank())
