#' Intersect two species names vectors
#'
#' @param data a `data.frame`. The output of `retrieve_accepted_names()` with at
#'   least the column `accepted_name`.
#' 
#' @param reference a `character` vector with species names used to filter 
#'   `data`.
#' 
#' @param quiet a `logical` of length 1. If `FALSE` (default), prints 
#'   information.

#' @return A subset of `data` w/ species found in `reference`.
#'   
#' @export
#' 
#' @examples
#' ## See `make.R`

filter_species_names <- function(data, reference, quiet = FALSE) {
  
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
  
  if (!("accepted_name" %in% colnames(data))) {
    stop("Argument 'data' must contain a column named 'accepted_name'", 
         call. = FALSE)
  }
  
  if (any(is.na(data$"accepted_name"))) {
    stop("The column 'accepted_name' of 'data' cannot contain missing values", 
         call. = FALSE)
  }
  
  if (missing(reference)) {
    stop("Argument 'reference' is required", call. = FALSE)
  }
  
  if (is.null(reference)) {
    stop("Argument 'reference' cannot be NULL", call. = FALSE)
  }
  
  if (!is.character(reference)) {
    stop("Argument 'reference' must be character", call. = FALSE)
  }
  
  if (any(is.na(reference))) {
    stop("Argument 'reference' cannot contain missing values", call. = FALSE)
  }
  
  if (!is.logical(quiet)) {
    stop("Argument 'quiet' must be a boolean ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  if (length(quiet) != 1) {
    stop("Argument 'quiet' must be a boolean of length 1", call. = FALSE)
  }
  
  
  ## Verbose ----
  
  if (!quiet) {
    cat("==== Filtering species names ===================\n")
  }
  
  
  ## Intersect species names ----
  
  reference <- reference[!duplicated(reference)]
  
  burst <- strsplit(reference, " ") |> 
    lapply(function(x) length(x)) |> 
    unlist()
  
  
  ## Match genus ----
  
  pos <- which(burst == 1)
  
  genus_sp <- data.frame()
  
  if (length(pos) > 0) {
    
    for (i in pos) {
      
      tmp <- data[grep(paste0("^", reference[i], "\\s.*"), 
                       data$"accepted_name"), ]
      
      tmp$"merge_string" <- rep(reference[i], nrow(tmp))
      
      genus_sp <- rbind(genus_sp, tmp)
    }
  }
  
  
  ## Match binomial ----
  
  binomial_sp <- data.frame()
  
  pos <- which(burst == 2)
  
  if (length(pos) > 0) {
    
    binomial_sp <- data[data$"accepted_name" %in% reference[pos], ]
    
    binomial_sp$"merge_string" <- binomial_sp$"accepted_name"
  }
  
  datasub <- rbind(genus_sp, binomial_sp)
  
  datasub <- datasub[order(datasub$"accepted_name"), ]
  rownames(datasub) <- NULL
  
  
  if (!quiet) {
    
    total_number_of_taxa     <- nrow(data)
    number_of_taxa_found     <- nrow(datasub)
    
    values <- format(c(total_number_of_taxa, number_of_taxa_found))
    
    cat("\n---- Results -----------------------------------\n")
    cat("   * Number of species:          ", values[1], "\n")
    cat("   * Number of Bioshifts species:", values[2], "\n")
    cat("Done.\n\n")
  }
  
  datasub
}
