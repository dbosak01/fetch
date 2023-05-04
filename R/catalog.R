
# Catalog Functions ------------------------------------------------------


#' @title Create a data source catalog
#' @encoding UTF-8
#' @description The \code{catalog} function returns a data catalog
#' for a data source. A data catalog is like a collection of data dictionaries
#' for all the datasets in the data source.  The catalog allows you to 
#' examine the datasets in the data source without yet loading anything
#' into memory.  Once you decide which data items you want to load, 
#' use the \code{\link{fetch}} function to load that item into memory.
#' @param source The source for the data.  This parameter is required. Normally
#' the source is passed as a full or relative path.
#' @param engine The data engine to use for this data source. This parameter
#' is required. The available data engines are available on the \code{\link{engines}}
#' enumeration.  For example, \code{engines$csv} will specify the CSV engine, 
#' and \code{engines$rdata} will specify the RDATA engine.
#' @param pattern A pattern to use when loading the data source.  The pattern
#' can be a name or a vector of names.  Names also accept wildcards.
#' @param where A where expression to use when fetching 
#' the data. This expression will apply to all fetch operations on this catalog.
#' The where expression should be defined with the Base R \code{\link{expression}}
#' function.
#' @param import_specs The import specs to use for any fetch operation on 
#' this catalog.  The import spec can be used to control the data types
#' on the incoming columns. You can create separate import specs for each 
#' dataset, or one import spec to use for all datasets. 
#' See the \code{\link{import_spec}} and 
#' \code{\link{specs}} functions for more information about this capability. 
#' @return The loaded data catalog.
#' @seealso The \code{\link{fetch}} function to retrieve data from the catalog.
#' Also see the \code{\link{import_spec}} function to create import specifications.
#' @examples 
#' # Get data directory
#' pkg <- system.file("extdata", package = "fetch")
#' 
#' # Create catalog
#' ct <- catalog(pkg, engines$csv)
#' 
#' # Example 1: Catalog all rows
#' 
#' # View catalog
#' ct
#' # data catalog: 6 items
#' # - Source: C:/packages/fetch/inst/extdata
#' # - Engine: csv
#' # - Items:
#'   # data item 'ADAE': 56 cols 150 rows
#'   # data item 'ADEX': 17 cols 348 rows
#'   # data item 'ADPR': 37 cols 552 rows
#'   # data item 'ADPSGA': 42 cols 695 rows
#'   # data item 'ADSL': 56 cols 87 rows
#'   # data item 'ADVS': 37 cols 3617 rows
#' 
#' # View catalog item
#' ct$ADEX
#' # data item 'ADEX': 17 cols 348 rows
#' # - Engine: csv
#' # - Size: 70.7 Kb
#' # - Last Modified: 2020-09-18 14:30:22
#' #    Name   Column     Class Label Format NAs MaxChar
#' # 1  ADEX  STUDYID character  <NA>     NA   0       3
#' # 2  ADEX  USUBJID character  <NA>     NA   0      10
#' # 3  ADEX   SUBJID character  <NA>     NA   0       3
#' # 4  ADEX   SITEID character  <NA>     NA   0       2
#' # 5  ADEX     TRTP character  <NA>     NA   8       5
#' # 6  ADEX    TRTPN   numeric  <NA>     NA   8       1
#' # 7  ADEX     TRTA character  <NA>     NA   8       5
#' # 8  ADEX    TRTAN   numeric  <NA>     NA   8       1
#' # 9  ADEX   RANDFL character  <NA>     NA   0       1
#' # 10 ADEX    SAFFL character  <NA>     NA   0       1
#' # 11 ADEX   MITTFL character  <NA>     NA   0       1
#' # 12 ADEX  PPROTFL character  <NA>     NA   0       1
#' # 13 ADEX    PARAM character  <NA>     NA   0      45
#' # 14 ADEX  PARAMCD character  <NA>     NA   0       8
#' # 15 ADEX   PARAMN   numeric  <NA>     NA   0       1
#' # 16 ADEX     AVAL   numeric  <NA>     NA  16       4
#' # 17 ADEX AVALCAT1 character  <NA>     NA  87      10
#' 
#' 
#' # Example 2: Catalog with where expression
#' ct <- catalog(pkg, engines$csv, where = expression(SUBJID == '049'))
#' 
#' # View catalog item - Now only 4 rows
#' ct$ADEX
#' # data item 'ADEX': 17 cols 4 rows
#' #- Where: SUBJID == "049"
#' #- Engine: csv
#' #- Size: 4.5 Kb
#' #- Last Modified: 2020-09-18 14:30:22
#' #Name   Column     Class Label Format NAs MaxChar
#' #1  ADEX  STUDYID character  <NA>     NA   0       3
#' #2  ADEX  USUBJID character  <NA>     NA   0      10
#' #3  ADEX   SUBJID character  <NA>     NA   0       3
#' #4  ADEX   SITEID character  <NA>     NA   0       2
#' #5  ADEX     TRTP character  <NA>     NA   0       5
#' #6  ADEX    TRTPN   numeric  <NA>     NA   0       1
#' #7  ADEX     TRTA character  <NA>     NA   0       5
#' #8  ADEX    TRTAN   numeric  <NA>     NA   0       1
#' #9  ADEX   RANDFL character  <NA>     NA   0       1
#' #10 ADEX    SAFFL character  <NA>     NA   0       1
#' #11 ADEX   MITTFL character  <NA>     NA   0       1
#' #12 ADEX  PPROTFL character  <NA>     NA   0       1
#' #13 ADEX    PARAM character  <NA>     NA   0      45
#' #14 ADEX  PARAMCD character  <NA>     NA   0       8
#' #15 ADEX   PARAMN   numeric  <NA>     NA   0       1
#' #16 ADEX     AVAL   numeric  <NA>     NA   0       4
#' #17 ADEX AVALCAT1 character  <NA>     NA   1      10
#'
#' @export
catalog <- function(source, engine, 
                    pattern = NULL, where = NULL, import_specs = NULL) {
  
    if (is.null(engine))
      stop("engine parameter cannot be null")
  
    if (is.null(engine))
      stop("engine parameter cannot be null")
    
    if (length(engine) > 1)
      stop("engine parameter does not accept more than one value.")
    
    if (!tolower(engine) %in% names(engines))
      stop(paste0("Invalid engine parameter value: ", engine))
    
    if (!is.null(import_specs)) {
      if (!("specs" %in% class(import_specs) | 
            "import_spec" %in% class(import_specs)))
        stop("import_specs parameter value must be of class 'specs' or 'import_spec'.")
      
    }
  
  
  # Create new structure of class "dcat"
  ret <- structure(list(), class = c("dcat", "list"))
  
  
  attr(ret, "source") <- source
  attr(ret, "engine") <- engine
  attr(ret, "pattern") <- pattern
  attr(ret, "where") <- where
  attr(ret, "import_specs") <- import_specs
  
  ret <- load_catalog(ret, source, engine, pattern, where, import_specs)

  
  return(ret)
  
}


#' @import tibble
load_catalog <- function(ret, source, engine, pattern  = NULL, 
                         where = NULL, import_specs = NULL) {
  
  
  # Get the file list according to the engine type
  lst <- list.files(source, pattern = paste0("\\.", engine, "$"), 
                    ignore.case = TRUE)
  
  if (!is.null(pattern)) {
    
    mlst <- gsub(paste0("\\.", engine, "$"), "", lst)
    
    pos <- grep(glob2rx(pattern), mlst)
    
    lst <- lst[pos]
  }
  
  for (fl in lst) {
    fp <- file.path(source, fl)
    ext <-  getExtension(fl)
    nm <- getFileName(fl)
    
    if (length(ext) > 0) { 
      
      dat <- NULL
      
      if (tolower(ext) == "csv") {
        
        dat <- get_dinfo_csv(fp, nm, where = where, import_specs = import_specs)
        
      } else if (tolower(ext) == "rds") {
        
        dat <- get_dinfo_rds(fp, nm, where = where, import_specs = import_specs)
        
      } else if (tolower(ext) == "rda") {
        
        dat <- get_dinfo_rda(fp, nm, where = where, import_specs = import_specs)
        
      } else if (tolower(ext) == "rdata") {
        
        dat <- get_dinfo_rdata(fp, nm, where = where, import_specs = import_specs)
        
      } else if (ext == "sas7bdat") {
        
        dat <- get_dinfo_sas7bdat(fp, nm, where = where, import_specs = import_specs)
        
      } else if (tolower(ext) == "dbf") {
        
        dat <- get_dinfo_dbf(fp, nm, where = where, import_specs = import_specs)
        
      } else if (tolower(ext) == "xpt") {
        
        dat <- get_dinfo_xpt(fp, nm, where = where, import_specs = import_specs)
        
      } else if (tolower(ext) == "xlsx") {
        
        dat <- get_dinfo_xlsx(fp, nm, where = where, import_specs = import_specs)
        
        
      } else if (tolower(ext) == "xls") {
        
        dat <- get_dinfo_xls(fp, nm, where = where, import_specs = import_specs)
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
#' @encoding UTF-8
#' @description A class-specific instance of the \code{print} function for 
#' a data catalog.  The function prints the catalog in a summary manner.  
#' Use \code{verbose = TRUE} option to print the catalog as a list.
#' @param x The catalog to print.
#' @param ... Any follow-on parameters.
#' @param verbose Whether or not to print the catalog in verbose style.
#' By default, the parameter is FALSE, meaning to print in summary style.
#' @return The object, invisibly.
#' @examples 
#' # Get data directory
#' pkg <- system.file("extdata", package = "fetch")
#' 
#' # Create catalog
#' ct <- catalog(pkg, engines$csv)
#' 
#' # View catalog
#' print(ct)
#' # data catalog: 6 items
#' # - Source: C:/packages/fetch/inst/extdata
#' # - Engine: csv
#' # - Items:
#'   # data item 'ADAE': 56 cols 150 rows
#'   # data item 'ADEX': 17 cols 348 rows
#'   # data item 'ADPR': 37 cols 552 rows
#'   # data item 'ADPSGA': 42 cols 695 rows
#'   # data item 'ADSL': 56 cols 87 rows
#'   # data item 'ADVS': 37 cols 3617 rows
#' 
#' @import crayon
#' @export
print.dcat <- function(x, ..., verbose = FALSE) {
  

    
  # Prepare color
  grey60 <- make_style(grey60 = "#999999")
  
  # attr(ret, "name") <- nm
  # attr(ret, "path") <- fp
  # attr(ret, "where") <- as.character(where)
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
  
  if (!is.null(attr(x, "where"))) 
    cat(paste0("- Where: ", as.character(attr(x, "where")), "\n"))
  
  
  if (length(x) > 0) {
    cat("- Items:\n")
    
    
    for (itm in x) {
      
      print(itm, verbose = verbose) 
      
    }
  
  }

    
  
  
  invisible(x)
}
