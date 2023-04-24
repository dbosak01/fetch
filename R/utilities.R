

#' @description Get the extension of a file
#' @noRd
getExtension <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
} 

#' @description Get the name of a file without extension
#' @noRd
getFileName <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[1])
}



#' @description Apply import specs to any data frame
#' @noRd
exec_spec <- function(x, spcs, nm) {
  
  ret <- x
  
  if (!is.null(spcs)) {  
    
    colspcs <- list()
    naval <- spcs$na
    tws <- spcs$trim_ws
    
    if (!is.null(spcs$specs[[nm]])) {
      tspec <- spcs$specs[[nm]]
      if (!is.null(tspec$na))
        naval <- tspec$na
      if (!is.null(tspec$trim_ws))
        tws <- tspec$trim_ws
      if (!is.null(tspec$col_types))
        colspcs <- tspec$col_types
      
    }
    
    for (nm in names(ret)) {
      
      # Apply trimws if requested on character columns
      if ("character" %in% class(ret[[nm]]))
        ret[[nm]] <- trimws(ret[[nm]])
      
      # Apply na conversion on requested values
      if (length(colspcs) == 0 | is.null(colspcs[[nm]]))
        ret[[nm]] <- ifelse(ret[[nm]] %in% naval, NA, ret[[nm]])
      
      if (length(colspcs) > 0) {
        if (!is.null(colspcs[[nm]])) {
          
          if (colspcs[[nm]] != "guess") {
            if (colspcs[[nm]] == "integer") {
              ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.integer(NA), ret[[nm]])
              ret[[nm]] <- as.integer(ret[[nm]])
            } else if (colspcs[[nm]] == "numeric") {
              ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.numeric(NA), ret[[nm]])
              ret[[nm]] <- as.numeric(ret[[nm]])
            } else if (colspcs[[nm]] == "character") {
              ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.character(NA), ret[[nm]])
              ret[[nm]] <- as.character(ret[[nm]])
            } else if (colspcs[[nm]] == "logical") {
              ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.logical(NA), ret[[nm]])
              ret[[nm]] <- as.logical(ret[[nm]])
            } else {
              
              spl <- trimws(unlist(strsplit(colspcs[[nm]], "=", fixed = TRUE)))
              
              if (length(spl) > 1) {
                if (spl[1] == "date") {
                  ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.Date(NA), ret[[nm]])
                  ret[[nm]] <- as.Date(ret[[nm]], spl[2])
                } else if (spl[1] == "datetime") {
                  ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.POSIXct(NA), ret[[nm]])
                  ret[[nm]] <- as.POSIXct(ret[[nm]], format = spl[2])
                } else if (spl[1] == "time") { 
                  ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.POSIXct(NA), ret[[nm]])
                  ret[[nm]] <- as.POSIXct(ret[[nm]], format = spl[2])
                }
              }
            }
          }
        }
      }
      
      for (atnm in names(attributes(x[[nm]]))) {
        if (atnm != "class")
          attr(ret[[nm]], atnm) <- attr(x[[nm]], atnm)
        
      }
    }
  }
  
  return(ret)
}




#' @noRd
get_dictionary <- function(x, dsnm) {
  
  ret <- NULL
  rw <- NULL
  usr_wdth <- c()
  str_wdth <- c()
  cntr <- 0
  
  for (nm in names(x)) {
    
    cntr <- cntr + 1
    
    lbl <- attr(x[[nm]], "label")
    fmt <- paste(as.character(attr(x[[nm]], "format")), collapse = "\n")

    
    if (fmt == "")
      fmt <- NA
    
    if (length(x[[nm]]) > 0) {
      if (typeof(x[[nm]]) == "character")
        str_wdth[cntr] <-  suppressWarnings(max(nchar(x[[nm]]), na.rm = TRUE))
      else 
        str_wdth[cntr] <-  suppressWarnings(max(nchar(as.character(x[[nm]])), 
                                                na.rm = TRUE))
      
      if (is.na(str_wdth[cntr]) | str_wdth[cntr] == -Inf)
        str_wdth[cntr] <- 0
      
    } else {
      str_wdth[cntr] <- NA
    }
    
    rw <- data.frame(Name = dsnm,
                     Column = nm,
                     Class = paste0(class(x[[nm]]), collapse = " "),
                     Label = ifelse(!is.null(lbl), lbl, as.character(NA)),
                     Format = ifelse(!is.null(fmt), fmt, NA),
                     NAs = sum(is.na(x[[nm]])),
                     MaxChar = str_wdth[cntr])
    
    if (is.null(ret))
      ret <- rw
    else 
      ret <- rbind(ret, rw)
    
  }
  
  
  return(ret)
  
}

