#' Extract unique species names from a single database of the traitdata package
#'
#' @param database a `character` of length 1. The name of the database to 
#'   extract species names. Must be available in the package `traitdata`. Use 
#'   `data(package = "traitdata")` to list available databases.
#' 
#' @param quiet a `logical` of length 1. If `FALSE` (default), prints 
#'   information.
#'
#' @return A two-column `data.frame` with:
#'   - `database`: the name of the 'traitdata' database;
#'   - `original_name`: the binomial name of species.
#'   
#' @export
#' 
#' @details
#' To install the package `traitdata`, use the following line: 
#' `remotes::install_github("RS-eco/traitdata")`
#' 
#' @examples
#' ## See `make.R`

extract_species_names <- function(database, quiet = FALSE) {
  
  ## Check arg ----
  
  if (missing(database)) {
    stop("Argument 'database' is required", call. = FALSE)
  }
  
  if (is.null(database)) {
    stop("Argument 'database' cannot be NULL", call. = FALSE)
  }
  
  if (!is.character(database)) {
    stop("Argument 'database' must be a character", call. = FALSE)
  }
  
  if (length(database) != 1) {
    stop("Argument 'database' must be a character of length 1", call. = FALSE)
  }
  
  if (is.na(database)) {
    stop("Argument 'database' cannot contain missing values", call. = FALSE)
  }
  
  if (!is.logical(quiet)) {
    stop("Argument 'quiet' must be a boolean ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  if (length(quiet) != 1) {
    stop("Argument 'quiet' must be a boolean of length 1", call. = FALSE)
  }
  
  
  ## Retrieve database metadata ----
  
  metadata <- list_databases()
  source   <- metadata[metadata$"database" == database, "source", drop = TRUE]
  
  
  if (source == "traitdata") {
  
    trait_data <- read_traitdata(database)
  }
  
  if (source == "wget") {
    
    trait_data <- read_wget_data(database)
  }
  
  
  ## Verbose ----
  
  if (!quiet) {
    cat("==== Extracting species names ==================\n")
  }
  
  
  ## Remove duplicated names ----
  
  species <- sort(unique(trait_data$"original_name"))
  species <- species[!is.na(species)]
  species <- species[species != ""]
  
  
  ## Verbose ----
  
  if (!quiet) {
    cat("\n---- Results -----------------------------------\n")
    cat("   * Database name:              ", database, "\n")
    cat("   * Number of species:          ", length(species), "\n")
    cat("Done.\n\n")
  }
  
  
  ## Output ----
  
  data.frame("database"      = database,
             "original_name" = species)
}
