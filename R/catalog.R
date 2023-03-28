
# Accessor Functions ------------------------------------------------------




#' @title Create a data source catalog
#' @description The \code{catalog} function returns a data catalog
#' for a data source. 
#' @param source The source for the data.  This parameter is required.
#' @param engine The data engine to use for this data source. This parameter
#' is required.
#' @param pattern A pattern to use when loading the data source.  The pattern
#' can be a name or a vector of names.  Names also accept wildcards.
#' @param filter One or more filter expressions to use when fetching 
#' the data. This filter will apply to all fetch operations on this catalog.
#' @param import_specs The import specs to use for any fetch operation on 
#' this catalog.
#' @return The loaded data catalog.
#' @seealso The \code{\link{fetch}} function to retrieve data from the catalog.
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' @export
catalog <- function(source, engine, 
                    pattern = NULL, filter = NULL, import_specs = NULL) {
  
    if (is.null(engine))
      stop("engine parameter cannot be null")
  
    if (is.null(engine))
      stop("engine parameter cannot be null")
    
    if (length(engine) > 1)
      stop("engine parameter does not accept more than one value.")
    
    if (!tolower(engine) %in% names(engines))
      stop(paste0("Invalid engine parameter value: ", engine))
    
    if (!is.null(import_specs)) {
      if (!"specs" %in% class(import_specs))
        stop("import_specs parameter value must be of class 'specs'.")
      
    }
  
  
  # Create new structure of class "dcat"
  ret <- structure(list(), class = c("dcat", "list"))
  
  
  attr(ret, "source") <- source
  attr(ret, "engine") <- engine
  attr(ret, "pattern") <- pattern
  attr(ret, "filter") <- filter
  attr(ret, "import_specs") <- import_specs
  
  ret <- load_catalog(ret, source, engine, pattern, filter, import_specs)

  
  return(ret)
  
}


#' @import tibble
load_catalog <- function(ret, source, engine, pattern  = NULL, 
                         filter = NULL, import_specs = NULL) {
  
  
  # Get the file list according to the engine type
  lst <- list.files(source, pattern = paste0("\\.", engine, "$"), 
                    ignore.case = TRUE)
  
  # if (!is.null(pattern))
  #   lst <- dofilter(pattern, lst, engine)
  
  for (fl in lst) {
    fp <- file.path(source, fl)
    ext <-  getExtension(fl)
    nm <- getFileName(fl)
    
    if (length(ext) > 0) { 
      
      dat <- NULL
      
      if (tolower(ext) == "csv") {
        
        dat <- get_dinfo_csv(fp, nm, filter = filter, import_specs = import_specs)
        
      } else if (tolower(ext) == "rds") {
        
        dat <- get_dinfo_rds(fp, nm, filter = filter, import_specs = import_specs)
        
      } else if (tolower(ext) == "rda") {
        
        dat <- get_dinfo_rda(fp, nm, filter = filter, import_specs = import_specs)
        
      } else if (tolower(ext) == "rdata") {
        
        dat <- get_dinfo_rdata(fp, nm, filter = filter, import_specs = import_specs)
        
      } else if (ext == "sas7bdat") {
        
        dat <- get_dinfo_sas7bdat(fp, nm, filter = filter, import_specs = import_specs)
        
      } else if (tolower(ext) == "dbf") {
        
        dat <- get_dinfo_dbf(fp, nm, filter = filter, import_specs = import_specs)
        
      } else if (tolower(ext) == "xpt") {
        
        dat <- get_dinfo_xpt(fp, nm, filter = filter, import_specs = import_specs)
        
      } else if (tolower(ext) == "xlsx") {
        
        dat <- get_dinfo_xlsx(fp, nm, filter = filter, import_specs = import_specs)
        
        
      } else if (tolower(ext) == "xls") {
        
        dat <- get_dinfo_xls(fp, nm, filter = filter, import_specs = import_specs)
      } 
      
      if (any(class(dat) == "data.frame")) {
        
        if (nm %in% names(ret))
          warning(paste("The name", nm, "already exists in the catalog.",
                        "Data will be replaced."))
        
        attr(dat, "engine") <- engine
        
        ret[[nm]] <- dat
      }
    }
    
  }
  
  return(ret)
  
}



#' @title Print a data catalog
#' @description A class-specific instance of the \code{print} function for 
#' data accessors.  The function prints the accessor in a summary manner.  
#' Use \code{verbose = TRUE} to print the accessor as a list.
#' @param x The library to print.
#' @param ... Any follow-on parameters.
#' @param verbose Whether or not to print the accessor in verbose style.
#' By default, the parameter is FALSE, meaning to print in summary style.
#' @return The object, invisibly.
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' @import crayon
#' @export
print.dcat <- function(x, ..., verbose = FALSE) {
  

    
  # Prepare color
  grey60 <- make_style(grey60 = "#999999")
  
  # attr(ret, "name") <- nm
  # attr(ret, "path") <- fp
  # attr(ret, "filter") <- as.character(filter)
  # attr(ret, "top") <- top
  # attr(ret, "import_specs") <- import_specs
  # attr(ret, "nrow") <- nrow(dat)
  # attr(ret, "ncol") <- ncol(dat)
  
  
  # Print a nice header
  cat(grey60(paste0("# data catalog: ", length(x), " items\n")))
  
  if (!is.null(attr(x, "source")))
    cat(paste0("- Source: ", attr(x, "source"), "\n"))
  
  if (!is.null(attr(x, "engine")))
    cat(paste0("- Engine: ", attr(x, "engine"), "\n"))
  
  if (!is.null(attr(x, "pattern")))
    cat(paste0("- Pattern: ", attr(x, "pattern"), "\n"))
  
  if (!is.null(attr(x, "filter"))) 
    cat(paste0("- Filter: ", attr(x, "filter"), "\n"))
  
  
  if (length(x) > 0) {
    cat("- Items:\n")
    
    
    for (itm in x) {
      
      print(itm, verbose = verbose) 
      
    }
  
  }

    
  
  
  invisible(x)
}
