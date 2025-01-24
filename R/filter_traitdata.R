#' Subset traitdata for Bioshifts species
#'
#' @param data a `data.frame`. The output of `filter_species_names()` with at 
#'   least three columns: `original_name`, `accepted_name`, and `database`.
#'   
#' @param quiet a `logical`. If `FALSE` (default), prints information.
#' 
#' @return A `data.frame` with trait data filtered for Bioshifts species.
#' 
#' @examples
#' ## See `make.R`

filter_traitdata <- function(data, quiet = FALSE) {
  
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
  
  if (!("original_name" %in% colnames(data))) {
    stop("Argument 'data' must contain a column named 'original_name'", 
         call. = FALSE)
  }
  
  if (any(is.na(data$"original_name"))) {
    stop("The column 'original_name' of 'data' cannot contain missing values", 
         call. = FALSE)
  }
  
  if (!("accepted_name" %in% colnames(data))) {
    stop("Argument 'data' must contain a column named 'accepted_name'", 
         call. = FALSE)
  }
  
  if (any(is.na(data$"accepted_name"))) {
    stop("The column 'accepted_name' of 'data' cannot contain missing values", 
         call. = FALSE)
  }
  
  if (!("database" %in% colnames(data))) {
    stop("Argument 'data' must contain a column named 'database'", 
         call. = FALSE)
  }
  
  if (any(is.na(data$"database"))) {
    stop("The column 'database' of 'data' cannot contain missing values", 
         call. = FALSE)
  }
  
  if (length(unique(data$"database")) != 1) {
    stop("The column 'database' of 'data' must contain one unique value", 
         call. = FALSE)
  }
  
  if (!is.logical(quiet)) {
    stop("Argument 'quiet' must be a boolean ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  if (length(quiet) != 1) {
    stop("Argument 'quiet' must be a boolean of length 1", call. = FALSE)
  }  
  
  
  ## Retrieve database metadata ----
  
  database <- unique(data$"database")
  
  trait_data <- read_traitdata(database)

  
  ## Verbose ----
  
  if (!quiet) {
    cat("==== Filtering trait data ======================\n")
  }
  
  
  trait_datasub <- trait_data[trait_data$"original_name" %in% 
                                data$"original_name", ]
  
  data <- data[ , c("original_name", "accepted_name")]
  
  data <- merge(data, trait_datasub, by = "original_name", all = TRUE)
  
  
  ## Verbose ----
  
  if (!quiet) {
    
    values <- format(c(nrow(trait_data), nrow(data)))
    
    cat("\n---- Results -----------------------------------\n")
    cat("   * Database name:              ", database, "\n")
    cat("   * Number of initial rows:     ", values[1], "\n")
    cat("   * Number of filtered rows:    ", values[2], "\n")
    cat("Done.\n\n")
  }
  
  data
}
