#' Clean species binomial names
#'
#' @description
#' Removes multiple white spaces, numbers, punctuation, author names, etc. and
#' converts to the appropriate case (e.g. `Genus species`).
#'
#' @param data a `data.frame`. The output of `extract_species_names()` with at 
#'   least the column `original_name`.
#' 
#' @param species_only a `logical` of length 1. If `TRUE` (default), remove 
#'   subspecies, variety, etc. and keep only the parent (species).
#'
#' @param n_cores an `integer` of length 1. The number of threads used to 
#'   parallelize code.
#'   
#' @param quiet a `logical` of length 1. If `FALSE` (default), prints 
#'   information.
#'   
#' @return A `data.frame` same as `data` but with an additional column named
#'   `cleaned_name`.
#' 
#' @export
#'
#' @examples
#' ## See `make.R`

clean_species_names <- function(data, species_only = TRUE, quiet = FALSE, 
                                genus = TRUE,
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
  
  if (!("original_name" %in% colnames(data))) {
    stop("Argument 'data' must contain a column named 'original_name'", 
         call. = FALSE)
  }
  
  if (any(is.na(data$"original_name"))) {
    stop("The column 'original_name' of 'data' cannot contain missing values", 
         call. = FALSE)
  }
  
  if (!is.logical(species_only)) {
    stop("Argument 'species_only' must be a boolean ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  if (length(species_only) != 1) {
    stop("Argument 'species_only' must be a boolean of length 1", call. = FALSE)
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
    cat("==== Cleaning species names ====================\n")
  }
  
  
  ## Setup parallelization ----
  
  cl <- parallel::makeCluster(n_cores)
  
  parallel::clusterExport(cl, c("data"), envir = environment())
  
  
  ## Clean species names ----
  
  cleaned_names <- parallel::parLapply(data$"original_name", function(x) {
    
    x <- iconv(x, "latin1", "ASCII", "")
    
    x <- gsub(" sp\\..*", " sp.", x)
    
    x <- gsub("\\(.*\\)", "", x)
    x <- gsub("\\[.*\\]", "", x)
    x <- gsub("\\{.*\\}", "", x)
    
    x <- gsub("[[:digit:]]", "", x)
    
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    
    x <- gsub("[^[:alpha:][:space:]-]", "", x)
    
    x <- gsub(" sp(\\s|$)| spp(\\s|$)", " sp.", x)
    x <- gsub(" ssp(\\s|$)| subsp(\\s|$)", " subsp.", x)
    x <- gsub(" cf(\\s|$)", " cf.", x)
    x <- gsub(" var(\\s|$)", " var.", x)
    x <- gsub(" aff(\\s|$)", " aff.", x)
    
    x <- gsub("\\.", ". ", x)
    
    x <- gsub(" x ", " ", x, ignore.case = TRUE)
    x <- gsub("^x ", " ", x, ignore.case = TRUE)
    
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    
    x <- tolower(x)
    x <- paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
    
    
    if (!genus) {
      if (length(strsplit(x, " ")[[1]]) == 1) {
        x <- paste0(x, " sp.")
      }  
    }
    
    
    
    if (species_only) {
      
      x <- strsplit(x, " ")[[1]]
      
      if (length(x) > 0) {
        
        if (length(x) > 1) {
          x <- paste0(x[1:2], collapse = " ") 
        }
        
      } else {
        
        x <- NA
      }
      
    } else {
      
      if (nchar(x) == 0) {
        
        x <- NA
      }
    }
  
    x
    
  }, cl = cl)
  
  
  parallel::stopCluster(cl)
  
  cleaned_names <- unlist(cleaned_names)
  
  
  ## Verbose ----
  
  if (!quiet) {
    
    values <- format(c(length(cleaned_names), sum(is.na(cleaned_names)),
                       sum(cleaned_names != data$"original_name")))
    
    cat("\n---- Results -----------------------------------\n")
    cat("   * Number of species:          ", values[1], "\n")
    cat("   * Number of names fixed:      ", values[3], "\n")
    cat("   * Number of NA produced:      ", values[2], "\n")
    cat("Done.\n\n")
  }
  
  
  ## Clean output ----
  
  data$"cleaned_name" <- cleaned_names
  
  data
}
