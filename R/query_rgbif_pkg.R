#' Search taxonomic information for a list of taxa using the `rgbif` package
#'
#' @param sp_names a `character` vector with taxon names
#' 
#' @param kingdom a `character` of length 1. (Optional) Can be used to refine 
#'   queries. If provided, it will be applied to all taxon names.
#'   
#' @param species_only a `logical`. If `TRUE` (default), returns only matches at
#'   species rank.
#' 
#' @param found_only a `logical`. If `TRUE` (default), returns only matches.
#' 
#' @param n_cores an `integer` of length 1. The number of threads used to 
#'   parallelize code.
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
#' query_rgbif_pkg(sp_names = species, kingdom = "Animalia", found_only = FALSE)

query_rgbif_pkg <- function(sp_names, kingdom = NULL, species_only = TRUE, 
                            found_only = TRUE, quiet = FALSE,
                            n_cores = parallel::detectCores() - 2) {
  
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
  
  if (!is.null(kingdom)) {
    
    if (!is.character(kingdom)) {
      stop("Argument 'kingdom' must be character")
    }
    
    if (length(kingdom) != 1) {
      stop("Argument 'kingdom' must be a character of length 1")
    }
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
  
  if (!is.numeric(n_cores)) {
    stop("Argument 'n_cores' must be a numeric")
  }
  
  if (length(n_cores) != 1) {
    stop("Argument 'n_cores' must be a numeric of length 1")
  }
  
  if (!quiet) {
    
    cat("==== Searching w/ rgbif package =============\n")
  }
  
  
  ## Setup parallelization ----
  
  cl <- parallel::makeCluster(n_cores)
  
  parallel::clusterExport(cl, c("sp_names", "kingdom"), envir = environment())
  
  
  results <- parallel::parLapply(sp_names, function(x) {
      
    results <- rgbif::name_backbone(name    = x, 
                                    kingdom = kingdom)
    
    if (any(results$"matchType" == "HIGHERRANK")) {
      
      results <- rgbif::name_backbone(name    = x, 
                                      kingdom = kingdom,
                                      verbose = TRUE)
    }
    
    results <- results[results$"matchType" %in% c("FUZZY", "EXACT"), ]
    results <- results[results$"confidence" > 94, ]
    results <- results[order(results$"confidence", decreasing = TRUE), ]
    results <- results[1, ]
    
    results$"requested_name" <- x
    
    results
    
  }, cl = cl)
  
  parallel::stopCluster(cl)
  
  
  ## Append list to data.frame ----
  
  results <- data.table::rbindlist(results, fill = TRUE)
  results <- as.data.frame(results)
  
  
  ## Add GBIF code ----
  
  results$"species_id" <- results$"usageKey"
  
  if ("acceptedUsageKey" %in% colnames(results)) {
    
    pos <- which(!is.na(results$"acceptedUsageKey"))
    
    if (length(pos) > 0) {
      results[pos, "species_id"] <- results[pos, "acceptedUsageKey"]
    }
  }
  
  results$"species_id" <- paste0("GBIF:", results$"species_id")
  
  
  ## Rename columns ----
  
  results$"accepted_name" <- results$"species"
  results$"source"        <- "rgbif"
  
  
  ## Select only species rank ----
  
  if (species_only) {
    
    if ("rank" %in% tolower(colnames(results))) {
      pos <- which(toupper(results$"rank") != "SPECIES" | is.na(results$"rank"))
      
      results[pos, c("kingdom", "phylum", "class", "order", "family", 
                     "accepted_name", "species_id", "source")] <- NA
      
    } else {
      
      results <- data.frame(matrix(ncol = 9, nrow = 0))
      colnames(results) <- c("requested_name", "accepted_name", "kingdom", 
                             "phylum", "class", "order", "family", "species_id",
                             "source")
    }
  }
  
  
  ## Remove unmatched taxa ----
  
  if (found_only) {
    
    results <- results[!is.na(results$"accepted_name"), ]
  }
  
  
  ## Clean output ----
  
  results <- results[order(results$"requested_name"), ]
  
  results <- results[ , c("requested_name", "accepted_name", "kingdom", 
                          "phylum", "class", "order", "family", "species_id",
                          "source")]

  rownames(results) <- NULL
  
  
  ## Print summary ----
  
  if (!quiet) {
  
    total_number_of_taxa     <- length(sp_names)
    number_of_taxa_found     <- sum(!is.na(results$"accepted_name"))
    number_of_taxa_not_found <- total_number_of_taxa - number_of_taxa_found
    
    values <- format(c(total_number_of_taxa, number_of_taxa_found, 
                       number_of_taxa_not_found))
    
    cat("\n---- Results --------------------------------\n")
    
    cat("   * Number of taxa:          ", values[1], "\n")
    cat("   * Number of taxa found:    ", values[2], "\n")
    cat("   * Number of taxa not found:", values[3], "\n")
    
    cat("\n---- Done -----------------------------------\n\n")
  }
  
  results
}
