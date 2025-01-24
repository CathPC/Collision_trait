#' Subset trait databases for Bioshifts species
#' 
#' @description 
#' This script contains the second step of the workflow:
#' 
#' - Import trait databases
#' - Extract original names
#' - Clean original names
#' - Retrieve accepted names
#' - Select Bioshifts species
#' - Export table in `data/derived-data/species_list-database.qs`
#' - Subset trait database for selected Bioshifts species
#' - Export trait database in `data/derived-data/traitdata-database.qs`
#' 
#' @author Nicolas Casajus \email{rdev.nc@gmail.com}
#' 
#' @date 2024/05/30


## Do not process trait databases again and again ----

options("update_compilation" = FALSE)


## Import Bioshifts species names ----

bioshifts_species <- qs::qread(here::here("data", "derived-data", 
                                          "bioshifts-species_list.qs"))


## Get list of trait databases ----

trait_databases <- list_databases()


## For one specific database ----

trait_databases <- trait_databases |>
  subset(trait_databases$"database" == "PalmTraits")


## Subset 'traitdata' and export tables ----

pipeline <- lapply(trait_databases$"database", function(database) {

  filename <- here::here("data", "derived-data", 
                         paste0("species_list-", database, ".qs"))
  
  if ((file.exists(filename) && getOption("update_compilation")) || 
      !file.exists(filename)) {
    
    ## Clean 'traitdata' species names ----
    
    traitdata_species <- database |> 
      extract_species_names() |> 
      clean_species_names() |> 
      retrieve_accepted_names() |> 
      filter_species_names(reference = bioshifts_species$"accepted_name")
    
    
    ## Save species list ----
    
    qs::qsave(traitdata_species, file = filename)
    
  } else {
    
    ## Import cleaned species list ----
    
    traitdata_species <- qs::qread(file = filename)
  }
  
  
  if (nrow(traitdata_species) > 0) {
    
    filename <- here::here("data", "derived-data", 
                           paste0("traitdata-", database, ".qs"))
    
    
    if ((file.exists(filename) && getOption("update_compilation")) || 
        !file.exists(filename)) {
      
      ## Subset 'traitdata' data for selected species ----
      
      traitdata_df <- traitdata_species |> 
        filter_traitdata()
      
      
      ## Save trait data ----
      
      qs::qsave(traitdata_df, file = filename)
    }
  }
  
  invisible(NULL)
  
})
