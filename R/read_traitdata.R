read_traitdata <- function(database) {
  
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
  
  
  ## Check if database is available ----
  
  available_databases <- data(package = "traitdata")$"results"[ , 3]
  
  if (!(database %in% available_databases)) {
    stop("The database '", database, "' is not available (maybe mispelled).", 
         "\n-> Please use `data(package = \"traitdata\")` to list databases ", 
         "available in the 'traitdata' package.", call. = FALSE)
  }
  
  
  ## Import trait data ----
  
  data(list = database, package = "traitdata")
  
  trait_data <- eval(parse(text = database))
  
  
  ## Clean memory ----
  
  rm(list = database, envir = .GlobalEnv)
  gc(verbose = FALSE)
  
  
  ## Create species name ----
  
  trait_data$"original_name" <- paste(trait_data$"Genus", trait_data$"Species")
  
  trait_data
}
