convert_trait_units <- function(data) {
  
  
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
  
  if (!("original_trait_value" %in% colnames(data))) {
    stop("Argument 'data' must contain a column named 'original_trait_value'", 
         call. = FALSE)
  }
  
  if (any(is.na(data$"original_trait_value"))) {
    stop("The column 'original_trait_value' of 'data' cannot contain missing ",
         "values", call. = FALSE)
  }
  if (!("final_trait_name" %in% colnames(data))) {
    stop("Argument 'data' must contain a column named 'final_trait_name'", 
         call. = FALSE)
  }
  
  if (any(is.na(data$"final_trait_name"))) {
    stop("The column 'final_trait_name' of 'data' cannot contain missing ",
         "values", call. = FALSE)
  }
  
  if (!("original_trait_units" %in% colnames(data))) {
    stop("Argument 'data' must contain a column named 'original_trait_units'", 
         call. = FALSE)
  }
  
  metadata <- read.csv(here::here("data", "trait_metadata.csv"))
  
  trait_data <- lapply(seq_len(nrow(data)), function(i) {
    
    if (data[i, "final_trait_name"] %in% metadata$"final_trait_name") {
      
      if (!is.na(data[i, "original_trait_units"])) {
        
        x <- as.numeric(data[i, "original_trait_value"])
        y <- as.character(data[i, "original_trait_units"])
        
        units(x) <- units::as_units(y)
        
        pos <- which((metadata$"final_trait_name" == 
                       data[i, "final_trait_name"]) & 
                      metadata$"database" == data[i, "database"])
        
        z <- as.character(metadata[pos, "final_trait_units"])
        
        units(x) <- units::as_units(z)
        
        data[i, "final_trait_value"] <- as.numeric(x)
        data[i, "final_trait_units"] <- z
      }
    }
    
    data[i, ]
  })
  
  do.call(rbind.data.frame, trait_data)
}
