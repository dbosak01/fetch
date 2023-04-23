

# Enumerations ------------------------------------------------------------

#' @title A list of engine types
#' @encoding UTF-8
#' @description The engines enumeration contains all possible options
#' for the "engine" parameter of the \code{\link{catalog}} function.  Use
#' this enumeration to specify what kind of data you would like to load.
#' Options are: csv, dbf, rda, rds, rdata, sas7bdat, xls, xlsx, and xpt.
#' @export
engines <- list()

# Assign class
class(engines) <- "etype"

# Assign engine types
engines$sas7bdat <- "sas7bdat"
engines$csv <- "csv"
engines$rds <- "rds"
engines$rdata <- "rdata"
engines$xpt <- "xpt"
engines$xls <- "xls"
engines$xlsx <- "xlsx"
engines$dbf <- "dbf"
engines$rda <- "rda"



# Get Data Info -----------------------------------------------------------

#' @import utils
get_dinfo_attributes <- function(dat, fp, nm, filter = NULL, top = NULL, 
                                 import_specs = NULL) {
  
  ret <- get_dictionary(dat, nm)
  
  attr(ret, "name") <- nm
  attr(ret, "path") <- fp
  if (!is.null(filter))
    attr(ret, "filter") <- as.character(filter)
  attr(ret, "top") <- top
  attr(ret, "import_specs") <- import_specs
  attr(ret, "nrow") <- nrow(dat)
  attr(ret, "ncol") <- ncol(dat)
  ret <- as.dinfo(ret)
  
  info <- file.info(fp)
  attr(ret, "modified") <- info[1, "mtime"]
  attr(ret, "size") <- format(utils::object.size(dat), units = "auto") 
  
  
  return(ret)
  
}


get_dinfo_rds <- function(fp, nm, filter = NULL, top = NULL, 
                             import_specs = NULL) {
  
  dat <- get_data_rds(fp, nm, filter, top, import_specs)

  
  ret <- get_dinfo_attributes(dat, fp, nm, filter, top, import_specs)
  
  attr(ret, "engine") <- engines$rds
  
  return(ret)
}


get_dinfo_rda <- function(fp, nm, filter = NULL, top = NULL, 
                          import_specs = NULL) {
  
  dat <- get_data_rda(fp, nm, filter, top, import_specs)
  
  ret <- get_dinfo_attributes(dat, fp, nm, filter, top, import_specs)
  
  attr(ret, "engine") <- engines$rda
  
  return(ret)
  
}


get_dinfo_rdata <- function(fp, nm, filter = NULL, top = NULL, 
                          import_specs = NULL) {
  
  dat <- get_data_rda(fp, nm, filter, top, import_specs)
  
  ret <- get_dinfo_attributes(dat, fp, nm, filter, top, import_specs)
  
  attr(ret, "engine") <- engines$rdata
  
  return(ret)
  
}

get_dinfo_csv <- function(fp, nm, filter = NULL, top = NULL, 
                          import_specs = NULL) {
  
  dat <- get_data_csv(fp, nm, filter, top, import_specs)
  
  ret <- get_dinfo_attributes(dat, fp, nm, filter, top, import_specs)
  
  attr(ret, "engine") <- engines$csv
  
  
  return(ret)
}


get_dinfo_sas7bdat <- function(fp, nm, filter = NULL, top = NULL, 
                               import_specs = NULL) {
  
  dat <- get_data_sas7bdat(fp, nm, filter, top, import_specs)
  
  ret <- get_dinfo_attributes(dat, fp, nm, filter, top, import_specs)
  
  attr(ret, "engine") <- engines$sas7bdat
  
  return(ret)

}

get_dinfo_dbf <- function(fp, nm, filter = NULL, top = NULL, 
                          import_specs = NULL) {
  
  dat <- get_data_dbf(fp, nm, filter, top, import_specs)
  
  ret <- get_dinfo_attributes(dat, fp, nm, filter, top, import_specs)
  
  attr(ret, "engine") <- engines$dbf
  
  return(ret)
  
}

get_dinfo_xpt <- function(fp, nm, filter = NULL, top = NULL, 
                          import_specs = NULL) {
  
  dat <- get_data_xpt(fp, nm, filter, top, import_specs)
  
  ret <- get_dinfo_attributes(dat, fp, nm, filter, top, import_specs)
  
  attr(ret, "engine") <- engines$xpt
  
  return(ret)
}


get_dinfo_xlsx <- function(fp, nm, filter = NULL, top = NULL, 
                           import_specs = NULL) {
  
  dat <- get_data_xlsx(fp, nm, filter, top, import_specs)
  
  ret <- get_dinfo_attributes(dat, fp, nm, filter, top, import_specs)
  
  attr(ret, "engine") <- engines$xlsx
  
  return(ret)
  
}

get_dinfo_xls <- function(fp, nm, filter = NULL, top = NULL, 
                          import_specs = NULL) {
  
  dat <- get_data_xls(fp, nm, filter, top, import_specs)
  
  ret <- get_dinfo_attributes(dat, fp, nm, filter, top, import_specs)
  
  attr(ret, "engine") <- engines$xls
  
  return(ret)
}



# Get data --------------------------------------------------------------


get_data_rds <- function(fp, nm, filter = NULL, top = NULL, import_specs = NULL) {
  
  dat <- read_rds(fp)
  
  if (!is.null(import_specs))
    dat <- exec_spec(dat, import_specs, nm) 
  
  if (!is.null(filter)) {

    dat <- tryCatch({subset(dat, eval(filter))},
                     error = function(cond){dat}) 
    
  }
  
  if (!is.null(top)) {
    if (nrow(dat) > top)
      dat <- dat[seq(1, top), ]
    
  }

  
  return(dat)
}


get_data_rda <- function(fp, nm, filter = NULL, top = NULL, import_specs = NULL) {
  
  # Create new environment
  erdata <- new.env()
  
  # Load file into new environment
  vrdata <- load(fp, envir = erdata)
  
  # Get file from environment into normal variable
  dat <- erdata[[vrdata[1]]]
  
  if (!is.null(import_specs))
    dat <- exec_spec(dat, import_specs, nm) 
  
  if (!is.null(filter)) {
    
    dat <- tryCatch({subset(dat, eval(filter))},
                    error = function(cond){dat}) 
    
  }
  
  if (!is.null(top)) {
    if (nrow(dat) > top)
      dat <- dat[seq(1, top), ]
    
  }

  return(dat)
}

#' @import readr
get_data_csv <- function(fp, nm, filter = NULL, top = NULL, 
                          import_specs = NULL) {
  
  
  
  if (is.null(import_specs)) {
    
    if (!is.null(top) & is.null(filter)) {
      
      dat <- read_csv(fp, col_types = cols(), n_max = top)
    
    } else {
      
      dat <- read_csv(fp, col_types = cols())
    }
    
  } else {
    

    # spcs <- get_colspec_csv(import_specs$specs[[nm]]$col_types)
    # # print(spcs)
    # na <- import_specs$specs[[nm]]$na
    # if (is.null(na))
    #   na = import_specs$na
    # tws <- import_specs$specs[[nm]]$trim_ws
    # if (is.null(tws))
    #   tws <- import_specs$trim_ws
    # 
    # dat <- suppressWarnings(read_csv(fp, 
    #                                  col_types = spcs,
    #                                  na = na,
    #                                  trim_ws = tws))
    # 
    # pb <- problems(dat)
    # 
    # if (nrow(pb) > 0) {
    #   pbmsg <- paste0("There were problems encountered reading in the '", 
    #                   nm, "' data file. Run 'problems(", name_c, "$", 
    #                   nm, ") to get ",
    #                   "a table of these problems.")
    #   
    #   warning(pbmsg) 
    #   
    # }
      
    
  }
  
  
  if (!is.null(filter)) {
    
    dat <- tryCatch({subset(dat, eval(filter))},
                    error = function(cond){dat}) 
    
  }
  
  if (!is.null(top) & !is.null(filter)) {
    if (nrow(dat) > top)
      dat <- dat[seq(1, top), ]
    
  }
  
  
  return(dat)
}


#' @import haven
get_data_sas7bdat <- function(fp, nm, filter = NULL, top = NULL, import_specs = NULL) {
  
  if (!is.null(top) & is.null(filter)) {
    
    dat <- read_sas(fp, n_max = top)
    
  } else {
    
    dat <- read_sas(fp)
  
  }
  
  if (!is.null(import_specs))
    dat <- exec_spec(dat, import_specs, nm)
  else {
    spcs <- specs(na = c("", "."))
    dat <- exec_spec(dat, spcs, nm)
    
  } 
  
  if (!is.null(filter)) {
    
    dat <- tryCatch({subset(dat, eval(filter))},
                    error = function(cond){dat}) 
    
  }
  
  if (!is.null(top) & !is.null(filter)) {
    if (nrow(dat) > top)
      dat <- dat[seq(1, top), ]
    
  }
  
  return(dat)
}

#' @import foreign
get_data_dbf <- function(fp, nm, filter = NULL, top = NULL, import_specs = NULL) {
  
  
  dat <- read.dbf(fp)
  if (!is_tibble(dat))
    dat <- as_tibble(dat)
  
  if (!is.null(import_specs))
    dat <- exec_spec(dat, import_specs, nm) 
  
  if (!is.null(filter)) {
    
    dat <- tryCatch({subset(dat, eval(filter))},
                    error = function(cond){dat}) 
    
  }
  
  if (!is.null(top)) {
    if (nrow(dat) > top)
      dat <- dat[seq(1, top), ]
    
  }
  
  
  return(dat)
  
}

#' @import haven
get_data_xpt <- function(fp, nm, filter = NULL, top = NULL, import_specs = NULL) {
  
  if (!is.null(top) & is.null(filter)) {
    
    dat <- read_xpt(fp, n_max = top)

  } else {
    
    dat <- read_xpt(fp)
  }
  
  if (!is.null(import_specs))
    dat <- exec_spec(dat, import_specs, nm)
  else {
    spcs <- specs(na = c("", "."))
    dat <- exec_spec(dat, import_specs, nm)
    
  } 
  
  if (!is.null(filter)) {
    
    dat <- tryCatch({subset(dat, eval(filter))},
                    error = function(cond){dat}) 
    
  }
  
  if (!is.null(top) & !is.null(filter)) {
    if (nrow(dat) > top)
      dat <- dat[seq(1, top), ]
    
  }
  
  return(dat)
}

#' @import readxl
get_data_xlsx <- function(fp, nm, filter = NULL, top = NULL, import_specs = NULL) {
  
  
  if (is.null(import_specs)) {
    if (!is.null(top) & is.null(filter))
      dat <- read_xlsx(fp, n_max = top)
    else
      dat <- read_xlsx(fp)
    
  } else {
    
    # if (is.null(import_specs$specs[[nm]]))
    #   dat <- read_xlsx(fp)
    # else {
    #   typs <- import_specs$specs[[nm]]$col_types
    #   tmp <- read_xlsx(fp,
    #                    col_types = c("text"))
    #   nms <- names(tmp)
    #   spcs <- get_colspec_xlsx(typs, length(nms), nms)
    #   na <- import_specs$specs[[nm]]$na
    #   if (is.null(na))
    #     na = import_specs$na
    #   tws <- import_specs$specs[[nm]]$trim_ws
    #   if (is.null(tws))
    #     tws <- import_specs$trim_ws
    #   
    #   dat <- suppressWarnings(read_xlsx(fp, 
    #                                     col_types = spcs, 
    #                                     na = na, 
    #                                     trim_ws = tws))
    #   
    #   pb <- problems(dat)
    #   
    #   if (nrow(pb) > 0) {
    #     pbmsg <- paste0("There were problems encountered reading in the '", 
    #                     nm, "' data file. Run 'problems(", name_c, "$", 
    #                     nm, ") to get ",
    #                     "a table of these problems.")
    #     
    #     warning(pbmsg) 
    #     
    #   }
    # } 
    
  }
  
  if (!is.null(filter)) {
    
    dat <- tryCatch({subset(dat, eval(filter))},
                    error = function(cond){dat}) 
    
  }
  
  if (!is.null(top) & !is.null(filter)) {
    if (nrow(dat) > top)
      dat <- dat[seq(1, top), ]
    
  }
  
  return(dat)
  
}

#' @import readxl
get_data_xls <- function(fp, nm, filter = NULL, top = NULL, import_specs = NULL) {
  
  
  if (is.null(import_specs)) {
    
    if (!is.null(top) & is.null(filter))
      dat <- read_xls(fp, n_max = top)
    else
      dat <- read_xls(fp)
    
  } else {
    
    # if (is.null(import_specs$specs[[nm]]))
    #   dat <- read_xls(fp)
    # else {
    #   
    #   typs <- import_specs$specs[[nm]]$col_types
    #   tmp <- read_xls(fp,
    #                   col_types = c("text"))
    #   nms <- names(tmp)
    #   spcs <- get_colspec_xlsx(typs, length(nms), nms)
    #   na <- import_specs$specs[[nm]]$na
    #   if (is.null(na))
    #     na = import_specs$na
    #   tws <- import_specs$specs[[nm]]$trim_ws
    #   if (is.null(tws))
    #     tws <- import_specs$trim_ws
    #   
    #   dat <- suppressWarnings(read_xls(fp, 
    #                                    col_types = spcs, 
    #                                    na = na, 
    #                                    trim_ws = tws))
    #   
    #   pb <- problems(dat)
    #   
    #   if (nrow(pb) > 0) {
    #     pbmsg <- paste0("There were problems encountered reading in the '", 
    #                     nm, "' data file. Run 'problems(", name_c, "$", 
    #                     nm, ") to get ",
    #                     "a table of these problems.")
    #     
    #     warning(pbmsg) 
    #     
    #   }
    # }
    
  } 
  
  if (!is.null(filter)) {
    
    dat <- tryCatch({subset(dat, eval(filter))},
                    error = function(cond){dat}) 
    
  }
  
  if (!is.null(top) & !is.null(filter)) {
    if (nrow(dat) > top)
      dat <- dat[seq(1, top), ]
    
  }
  
  return(dat)
}




