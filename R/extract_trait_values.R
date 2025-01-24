extract_trait_values <- function(trait = "body_size") {
  
  ## Check trait name ----
  
  metadata <- read.csv(here::here("data", "trait_metadata.csv"))
  
  pos <- which(metadata$"trait_category" == trait)
  
  if (length(pos) == 0) {
    stop("No metadata available for the trait '", trait, 
         "' in the Google Sheet", 
         "\n-> Please run `read_gsheet_data()` to list available traits", 
         call. = FALSE)
  }
  
  
  ## Import metadata for the trait ----
  
  metadata <- metadata[metadata$"trait_category" == trait, ]
  metadata <- metadata[which(!is.na(metadata$"original_trait_name") & 
                               metadata$"original_trait_name" != ""), ]
  
  databases <- unique(metadata$"database")
  
  
  ## Parse databases ----
  
  trait_values <- lapply(databases, function(database) {
    
    filename <- here::here("data", "derived-data", 
                           paste0("traitdata-", database, ".qs"))
    
    # if (!file.exists(filename)) {
    #   stop("The file '", filename, "' does not exist", call. = FALSE)
    # }
    
    datasub <- data.frame()
    
    if (file.exists(filename)) {
      
      data <- qs::qread(filename)
      
      info <- metadata[metadata$"database" == database, ]
      
      for (i in 1:nrow(info)) {
        
        if (!(info[i, "original_trait_name"] %in% colnames(data))) {
          stop("The column '", info[i, "original_trait_name"], "' does not exist", 
               " in the database '", database, "'")
        }
        
        trait_data <- data.frame(
          "database"             = database,
          "accepted_name"        = data$"accepted_name",
          "original_trait_name"  = info[i, "original_trait_name"],
          "final_trait_name"     = info[i, "final_trait_name"],
          "original_trait_value" = data[ , info[i, "original_trait_name"]],
          "original_trait_units" = info[i, "original_units"])
        
        
        ## Convert NA ----
        
        if (!is.na(info[i, "na_values"]) && info[i, "na_values"] != "") {
          trait_data$"original_trait_value" <- gsub(info[i, "na_values"], NA, 
                                                    trait_data$"original_trait_value")
        }
        
        datasub <- rbind(datasub, trait_data)
      }
    }
    
    datasub
  })
  
  trait_values <- do.call(rbind.data.frame, trait_values)
  trait_values <- trait_values[!is.na(trait_values$"original_trait_value"), ]
  
  rownames(trait_values) <- NULL
  
  trait_values
}
