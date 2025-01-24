#' Read taxonomy file and extract original species names
#' 
#' @description
#' Imports the file `data/SPATMAN_collision_list_Of_species1.csv` and extract 
#' the column `original_name`.
#' 
#' @param quiet a `logical` of length 1. If `FALSE` (default), prints 
#'   information.
#'   
#' @return A `data.frame` with the column `original_name`.
#'   
#' @export
#' 
#' @examples
#' ## See `make.R`

get_species_names <- function(quiet = FALSE) {
  
  ## Check arg ----
  
  if (!is.logical(quiet)) {
    stop("Argument 'quiet' must be a boolean ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  if (length(quiet) != 1) {
    stop("Argument 'quiet' must be a boolean of length 1", call. = FALSE)
  }
  
  
  ## Check file ----
  
  filename <- here::here("data", "SPATMAN_collision_list_Of_species1.csv")
  
  if (!file.exists(filename)) {
    stop("Unable to find the file '", filename, "'", call. = FALSE)
  }
  
  
  ## Verbose ----
  
  if (!quiet) {
    cat("==== Extracting species names ==================\n")
  }
  
  ## Read data ----
  
  data <- read.csv(filename)
  data <- data[!is.na(data$"genus"), ]
  
  
  ## Verbose ----
  
  if (!quiet) {
    cat("\n---- Results -----------------------------------\n")
    cat("   * Database name:              ", "Collision", "\n")
    cat("   * Number of species:          ", nrow(data), "\n")
    cat("Done.\n\n")
  }
  
  ## Clean data ----
  
  data.frame("original_name" = sort(data$"original_name"))
}
