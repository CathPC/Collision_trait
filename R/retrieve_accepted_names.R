#' Retrieve species accepted names
#' 
#' @description
#' Queries GBIF, ITIS and NCBI databases to retrieve species accepted names by
#' using `rgbif`, `bdc`, `taxadb` packages.
#' 
#' This function gives priority to GBIF. If accepted names are found in ITIS
#' or NCBI databases, this function tries to find these accepted names in the 
#' GBIF system.
#'
#' @param data a `data.frame`. The output of `clean_species_names()` with at 
#'   least the column `cleaned_name`.
#' 
#' @param kingdom a `character` of length 1. (Optional) Can be used to refine 
#'   queries. If provided, it will be applied to all species.
#'   
#' @param providers a `character` vector w/ the names of taxonomic databases to
#'   query. See `taxadb::filter_name()` for further information.
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
#' @return A `data.frame` with the columns of `data` plus the following columns:
#'   - `accepted_name`: the species accepted name
#'   - `kingdom`: the kingdom of the species
#'   - `phylum`: the phylum of the species
#'   - `class`: the class of the species
#'   - `order`: the order of the species
#'   - `family`: the family of the species
#'   - `species_id`: the GBIF/ITIS/NCBI identifier of the accepted name
#'   - `source`: the name of the taxonomic database
#' 
#' @examples
#' ## See `make.R`

retrieve_accepted_names <- function(data, kingdom = NULL, 
                                    providers = c("gbif","itis","ncbi"), 
                                    species_only = TRUE, found_only = TRUE, 
                                    quiet = FALSE,
                                    n_cores = parallel::detectCores() - 2) {
  
  
  ## Check args ----
  
  if (missing(data)) {
    stop("Argument 'data' is required", call. = FALSE)
  }
  
  if (is.null(data)) {
    stop("Argument 'data' cannot be NULL", call. = FALSE)
  }
  
  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data.frame", call. = FALSE)
  }
  
  if (nrow(data) == 0) {
    stop("Argument 'data' must have at least one row (species)", call. = FALSE) 
  }
  
  if (!("cleaned_name" %in% colnames(data))) {
    stop("Argument 'data' must contain a column named 'cleaned_name'", 
         call. = FALSE)
  }
  
  if (any(is.na(data$"cleaned_name"))) {
    stop("The column 'cleaned_name' of 'data' cannot contain missing values", 
         call. = FALSE)
  }
  
  if (!is.null(kingdom)) {
    
    if (!is.character(kingdom)) {
      stop("Argument 'kingdom' must be a character", call. = FALSE)
    }
    
    if (length(kingdom) != 1) {
      stop("Argument 'kingdom' must be a character of length 1", call. = FALSE)
    }
  }
  
  if (!is.character(providers)) {
    stop("Argument 'providers' must be a character", call. = FALSE)
  }
  
  available_providers <- c("itis", "ncbi", "col", "gbif", "ott", "iucn")
  
  if (any(!(providers %in% available_providers))) {
    stop("Some providers listed in 'providers' are not available.", 
         "\n-> Possible values are: 'itis', 'ncbi', 'col', 'gbif', 'ott', and ",
         "'iucn'", call. = FALSE)
  }
  
  if (!is.logical(species_only)) {
    stop("Argument 'species_only' must be a boolean ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  if (length(species_only) != 1) {
    stop("Argument 'species_only' must be a boolean of length 1", call. = FALSE)
  }
  
  if (!is.logical(found_only)) {
    stop("Argument 'found_only' must be a boolean ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  if (length(found_only) != 1) {
    stop("Argument 'found_only' must be a boolean of length 1", call. = FALSE)
  }
  
  if (!is.logical(quiet)) {
    stop("Argument 'quiet' must be a boolean ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  if (length(quiet) != 1) {
    stop("Argument 'quiet' must be a boolean of length 1", call. = FALSE)
  }
  
  if (!is.numeric(n_cores)) {
    stop("Argument 'n_cores' must be a numeric", call. = FALSE)
  }
  
  if (length(n_cores) != 1) {
    stop("Argument 'n_cores' must be a numeric of length 1", call. = FALSE)
  }
  
  
  ## Verbose ----
  
  if (!quiet) {
    cat("==== Retrieving accepted names =================\n")
  }
  
  
  species <- data.frame("requested_name" = unique(data$"cleaned_name"))
  
  
  ## Searching rgbif package ----
  
  rgbif_names <- query_rgbif_pkg(sp_names     = species$"requested_name", 
                                 kingdom      = kingdom,
                                 species_only = species_only,
                                 found_only   = TRUE,
                                 n_cores      = n_cores,
                                 quiet        = TRUE)

  backbone <- merge(species, rgbif_names, by = "requested_name", all = FALSE)
  
  
  ## Searching bdc package ----
  
  pos <- which(!(data$"cleaned_name" %in% backbone$"requested_name"))
  
  if (length(pos) > 0) {
  
    tofind <- data[pos, "cleaned_name"]
    tofind <- unique(tofind)
    
    bdc_gbif_names <- query_bdc_pkg(sp_names     = tofind, 
                                    kingdom      = kingdom,
                                    species_only = species_only,
                                    found_only   = TRUE,
                                    n_cores      = n_cores, 
                                    quiet        = TRUE)
    
    backbone <- rbind(backbone, bdc_gbif_names)
  }
  
  
  ## Searching taxadb package ----
  
  pos <- which(!(data$"cleaned_name" %in% backbone$"requested_name"))
  
  if (length(pos) > 0) {
    
    tofind <- data[pos, "cleaned_name"]
    tofind <- unique(tofind)
    
    taxadb_names <- query_taxadb_pkg(sp_names     = tofind, 
                                     providers    = providers,
                                     species_only = species_only,
                                     found_only   = TRUE, 
                                     quiet        = TRUE)
    
    backbone <- rbind(backbone, taxadb_names)
  }

    
  ## Find non GBIF accepted species in GBIF (rgbif) ----
  
  tofind <- backbone[-grep("gbif", backbone$"source"), ]
  
  if (nrow(tofind) > 0) {
    
    gbif_names <- query_rgbif_pkg(sp_names     = tofind$"accepted_name", 
                                  kingdom      = kingdom,
                                  species_only = species_only,
                                  found_only   = TRUE,
                                  n_cores      = n_cores,
                                  quiet        = TRUE)
    
    if (nrow(gbif_names) > 0) {
      
      for (i in 1:nrow(gbif_names)) {
        
        pos <- which(backbone$"requested_name" == tofind[i, "requested_name"])
        
        backbone[pos, ] <- tofind[i, ]
      }
    }
  }
  
  
  ## Find non GBIF accepted species in GBIF (bdc) ----
  
  tofind <- backbone[-grep("gbif", backbone$"source"), ]
  
  if (nrow(tofind) > 0) {
    
    gbif_names <- query_bdc_pkg(sp_names     = tofind$"accepted_name", 
                                kingdom      = kingdom,
                                species_only = species_only,
                                found_only   = TRUE,
                                n_cores      = n_cores,
                                quiet        = TRUE)
    
    if (nrow(gbif_names) > 0) {
      
      for (i in 1:nrow(gbif_names)) {
        
        pos <- which(backbone$"requested_name" == tofind[i, "requested_name"])
        
        backbone[pos, ] <- tofind[i, ]
      }
    }
  }
  
  
  ## Find non GBIF accepted species in GBIF (taxadb) ----
  
  tofind <- backbone[-grep("gbif", backbone$"source"), ]
  
  if (nrow(tofind) > 0) {
    
    gbif_names <- query_taxadb_pkg(sp_names     = tofind$"accepted_name", 
                                   providers    = "gbif",
                                   species_only = species_only,
                                   found_only   = TRUE,
                                   quiet        = TRUE)
    
    if (nrow(gbif_names) > 0) {
      
      for (i in 1:nrow(gbif_names)) {
        
        pos <- which(backbone$"requested_name" == tofind[i, "requested_name"])
        
        backbone[pos, ] <- tofind[i, ]
      }
    }
  }
  
  
  if (!found_only) {
    
    backbone <- merge(species, backbone, by = "requested_name", all = TRUE)    
  }
  
  
  ## Clean outputs ----
  
  backbone <- merge(data, backbone, 
                    by.x = "cleaned_name", 
                    by.y = "requested_name", 
                    all = FALSE)
  
  backbone <- backbone[ , c(colnames(data), colnames(rgbif_names)[-1])]
  
  backbone <- backbone[order(backbone$"cleaned_name"), ]
  rownames(backbone) <- NULL
  
  
  ## Hack to fix ----
  
  if (!species_only) {
    backbone$"accepted_name" <- gsub("\\sNA$", "", backbone$"accepted_name")
  }
  
  
  ## Verbose ----
  
  if (!quiet) {
    
    total_number_of_taxa     <- nrow(data)
    number_of_taxa_found     <- sum(!is.na(backbone$"accepted_name"))
    number_of_taxa_not_found <- total_number_of_taxa - number_of_taxa_found
    
    values <- format(c(total_number_of_taxa, number_of_taxa_found, 
                       number_of_taxa_not_found))
    
    cat("\n---- Results -----------------------------------\n")
    cat("   * Number of species:          ", values[1], "\n")
    cat("   * Number of species found:    ", values[2], "\n")
    cat("   * Number of species not found:", values[3], "\n")
    cat("Done.\n\n")
  }
  
  backbone
}
