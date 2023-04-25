

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
exec_spec <- function(x, spcs, tnm) {
  
  ret <- x
  
  if (!is.null(spcs)) {  
    
    colspcs <- list()
    naval <- spcs$na
    tws <- spcs$trim_ws
    tspec <- NULL
    
    if ("specs" %in% class(spcs)) {
      
      if (!is.null(spcs$specs[[tnm]])) {
        
        tspec <- spcs$specs[[tnm]]
        
      } 
      
    } else if ("import_spec" %in% class(spcs)) {
      
        tspec <- spcs
      
    }
      
    if (!is.null(tspec)) {
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

                  if ("character" %in% class(ret[[nm]]))
                    ret[[nm]] <- as.Date(ret[[nm]], spl[2])
                  else if (!"Date" %in% class(ret[[nm]]))
                    ret[[nm]] <- as.Date(ret[[nm]], origin = as.Date("1970-01-01"))
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



#' @description Mapping of column types
#' @noRd
spec_trans <- c("guess" = "guess",
                "logical" = "logical",
                "character" = "text",
                "numeric" = "numeric", 
                "integer" = "numeric")

#' @description This function maps the colspec interface to the colspecs
#' needed for the excel import function.              
#' @noRd
get_colspec_xlsx <- function(type_string, num_cols, col_names) {
  
  ret <- NULL  
  
  
  if (length(col_names) != num_cols)
    stop("Column names and length are not equal")
  
  ret <- rep("guess", num_cols)
  names(ret) <- col_names
  
  for (nm in col_names) {
    if (nm %in% names(type_string)) {
      
      ct <- type_string[[nm]]
      
      if (ct %in% names(spec_trans))
        ret[[nm]] <- spec_trans[[ct]]
      else {
        ret[[nm]] <- "date"
      }
    }
  }
  
  
  return(ret)
  
}

#' @description This function maps the colspec interface to the colspecs
#' needed for the csv import function. 
#' @import readr                
#' @noRd
get_colspec_csv <- function(type_string) {
  
  
  ret <- cols()
  for (nm in names(type_string)) {
    if (type_string[[nm]] == "logical")
      ret$cols[[nm]] <- col_logical()
    else if (type_string[[nm]] == "character")
      ret$cols[[nm]] <- col_character()
    else if (type_string[[nm]] == "integer")
      ret$cols[[nm]] <- col_integer()
    else if (type_string[[nm]] == "numeric")
      ret$cols[[nm]] <- col_double()
    else if (type_string[[nm]] == "guess")
      ret$cols[[nm]] <- col_guess()
    else {
      fp <- trimws(unlist(strsplit(type_string[[nm]], "=", fixed = TRUE)))
      if (fp[[1]] == "date")
        ret$cols[[nm]] <- col_date(fp[[2]])
      else if (fp[[1]] == "time")
        ret$cols[[nm]] <- col_time(fp[[2]])
      else if (fp[[1]] == "datetime")
        ret$cols[[nm]] <- col_datetime(fp[[2]])
      else 
        stop(paste0("Column type not valid: ", fp[[1]]))
      
    }
    
  }
  
  return(ret)
  
}

