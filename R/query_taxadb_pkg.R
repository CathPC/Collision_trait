#' Search taxonomic information for a list of taxa using the `rgbif` package
#'
#' @param sp_names a `character` vector with taxon names
#' 
#' @param providers a `character` vector w/ taxonomic databases to query. See
#'   `taxadb::filter_name()` for further information.
#'   
#' @param species_only a `logical`. If `TRUE` (default), returns only matches at
#'   species rank.
#' 
#' @param found_only a `logical`. If `TRUE` (default), returns only matches.
#'   
#' @param quiet a `logical`. If `FALSE` (default), prints information.
#' 
#' @return A `data.frame` with the following columns:
#'   - `requested_name`: the species name provided by the user
#'   - `accepted_name`: the accepted species name
#'   - `kingdom`: the kingdom of the species
#'   - `phylum`: the phylum of the species
#'   - `class`: the class of the species
#'   - `order`: the order of the species
#'   - `family`: the family of the species
#'   - `species_id`: the GBIF key of the accepted name
#'   - `source`: the name of the taxonomic database
#' 
#' @export
#'
#' @examples
#' species <- c("Albericus brunhildae", "Aglyptodactylus securifer", 
#'              "Duttaphrynus pariet")
#' 
#' query_taxadb_pkg(sp_names = species, found_only = FALSE)

query_taxadb_pkg <- function(sp_names, providers = c("gbif", "itis", "ncbi"), 
                             species_only = TRUE, found_only = TRUE, 
                             quiet = FALSE) {
  
  
  ## Check args ----
  
  if (missing(sp_names)) {
    stop("Argument 'sp_names' is required")
  }
  
  if (!is.character(sp_names)) {
    stop("Argument 'sp_names' must be character")
  }
  
  if (any(is.na(sp_names))) {
    stop("Argument 'sp_names' cannot contain missing value (NA)")
  }
  
  if (!is.logical(species_only)) {
    stop("Argument 'species_only' must be a boolean ('TRUE' or 'FALSE')")
  }
  
  if (length(species_only) != 1) {
    stop("Argument 'species_only' must be a boolean of length 1")
  }
  
  if (!is.logical(found_only)) {
    stop("Argument 'found_only' must be a boolean ('TRUE' or 'FALSE')")
  }
  
  if (length(found_only) != 1) {
    stop("Argument 'found_only' must be a boolean of length 1")
  }
  
  
  ## Loop on providers ----
  
  new_search <- list()
  
  if (!quiet) {
    
    cat("==== Searching w/ taxadb package ============\n")
  }
  
  for (i in 1:length(providers)) {
    
    # cat(" * Searching in", toupper(providers[i]), "\n")
    
    results <- taxadb::filter_name(sp_names, provider = providers[i])
    
    results$"requested_name" <- results$"scientificName"
    results$"source"         <- paste0("taxadb-", tolower(providers[i]))
    results$"taxon_id"       <- results$"acceptedNameUsageID"
    
    if (providers[i] %in% c("col", "itis", "gbif")) {
      
      results$"scientificName" <- paste(results$"genus", 
                                        results$"specificEpithet")
      
    } else {
      
      results$"scientificName" <- results$"specificEpithet"
    }
    
    results <- results[with(results, order(requested_name, taxonomicStatus)), ]
    
    new_search[[i]] <- results
  }
  
  
  ## Append results ----
  
  new_search <- data.table::rbindlist(new_search, fill = TRUE)
  new_search <- as.data.frame(new_search)
  
  
  ## Remove duplicates ----
  
  new_search <- new_search[!duplicated(new_search$"requested_name"), ]
  
  
  ## Select only species rank ----
  
  if (species_only) {
    
    pos <- which(toupper(new_search$"taxonRank") != "SPECIES" | 
                   is.na(new_search$"taxonRank"))
    
    new_search[pos, -1] <- NA
  }
  
  
  ## Remove unmatched taxa ----
  
  if (found_only) {
    
    new_search <- new_search[!is.na(new_search$"scientificName"), ]
  
  } else {
    
    species    <- data.frame("requested_name" = sp_names)
    new_search <- merge(species, new_search, by = "requested_name", all = TRUE)
  }
  
  
  ## Select columns ----
  
  new_search <- new_search[ , c("requested_name", "scientificName", "kingdom", 
                                "phylum", "class", "order", "family", 
                                "taxon_id", "source")]
  
  
  ## Rename columns ----
  
  colnames(new_search) <- c("requested_name", "accepted_name", "kingdom", 
                            "phylum", "class", "order", "family", 
                            "species_id", "source")
  
  
  new_search <- new_search[order(new_search$"requested_name"), ]
  
  rownames(new_search) <- NULL
  
  
  ## Print summary ----
  
  if (!quiet) {
  
    total_number_of_taxa     <- length(sp_names)
    number_of_taxa_found     <- sum(!is.na(new_search$"accepted_name"))
    number_of_taxa_not_found <- total_number_of_taxa - number_of_taxa_found
    
    values <- format(c(total_number_of_taxa, number_of_taxa_found, 
                       number_of_taxa_not_found))
    
    cat("\n---- Results --------------------------------\n")
    
    cat("   * Number of taxa:          ", values[1], "\n")
    cat("   * Number of taxa found:    ", values[2], "\n")
    cat("   * Number of taxa not found:", values[3], "\n")
  
    cat("\n---- Done -----------------------------------\n\n")
  }
  
  new_search
}
