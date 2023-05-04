#' @title Fetch data from many data sources
#' @encoding UTF-8
#' @description 
#' The \strong{fetch} package allows you to retrieve data from many different
#' data sources.  The package retrieves data in a memory-efficient manner.
#' You first identify the data by defining a data catalog.  Then fetch
#' the data from the catalog.  Catalogs can be defined for many popular 
#' data formats: csv, rds, sas7bdat, excel, etc.
#' 
#' The functions contained in the \strong{fetch} package are as follows:
#' \itemize{
#'   \item{\code{\link{catalog}}: Creates a data library}
#'   \item{\code{\link{fetch}}: Creates a data dictionary}
#'   \item{\code{\link{import_spec}}: Defines an import spec for a specific dataset}
#' }
#' @docType package
#' @name fetchpackage
#' @keywords internal
NULL




#' @title Fetch a dataset from a data catalog
#' @encoding UTF-8
#' @description The \code{fetch} function retrieves a dataset from a data
#' catalog.  The function accepts a catalog item as the first parameter.  The
#' catalog item is the only required parameter. The "select" parameter allows
#' you to pull only some of the columns.  The "where" and "top" parameters
#' may be used to define a subset of the data to retrieve.  The "import_specs"
#' parameter accepts an \code{\link{import_spec}} object, which can be used
#' to control how data is read into the data frame.
#' @param catalog The catalog item to fetch data from.
#' @param select A vector of column names or column numbers to extract from the
#' data item. Note that the column names can be easily obtained as a vector
#' from the catalog item, and then manipulated to suit your needs.
#' @param where An optional expression to be used to filter the fetched data.  
#' Use the base R \code{\link{expression}} function to define the expression.  The
#' expression allows logical operators and Base R functions. Column names
#' can be unquoted.
#' @param top A number of records to return from the data source.  Valid 
#' value is an integer.
#' @param import_specs The import specs to use for the fetch operation. Import
#' specs can be used to control the data types of the fetched dataset.
#' An import specification is created with the \code{\link{import_spec}}
#' function. See the documentation of this function for additional details
#' and an example.
#' @seealso The \code{\link{catalog}} function to create a data catalog.
#' Also see the \code{\link{import_spec}} function to create import specifications.
#' @examples 
#' # Get data directory
#' pkg <- system.file("extdata", package = "fetch")
#' 
#' # Create catalog
#' ct <- catalog(pkg, engines$csv)
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
#' # Example 1: Fetch Entire Dataset
#' 
#' # Get data from the catalog
#' dat1 <- fetch(ct$ADEX)
#' 
#' # View Data
#' dat1
#' # A tibble: 348 × 17                                                                                      
#' #   STUDYID USUBJID   SUBJID SITEID TRTP  TRTPN TRTA  TRTAN RANDFL SAFFL
#' #   <chr>   <chr>     <chr>  <chr>  <chr> <dbl> <chr> <dbl> <chr>  <chr>
#' #  1 ABC     ABC-01-0… 049    01     ARM D     4 ARM D     4 Y      Y    
#' #  2 ABC     ABC-01-0… 049    01     ARM D     4 ARM D     4 Y      Y    
#' #  3 ABC     ABC-01-0… 049    01     ARM D     4 ARM D     4 Y      Y    
#' #  4 ABC     ABC-01-0… 049    01     ARM D     4 ARM D     4 Y      Y    
#' #  5 ABC     ABC-01-0… 050    01     ARM B     2 ARM B     2 Y      Y    
#' #  6 ABC     ABC-01-0… 050    01     ARM B     2 ARM B     2 Y      Y    
#' #  7 ABC     ABC-01-0… 050    01     ARM B     2 ARM B     2 Y      Y    
#' #  8 ABC     ABC-01-0… 050    01     ARM B     2 ARM B     2 Y      Y    
#' #  9 ABC     ABC-01-0… 051    01     ARM A     1 ARM A     1 Y      Y    
#' # 10 ABC     ABC-01-0… 051    01     ARM A     1 ARM A     1 Y      Y    
#' #  338 more rows
#' #  7 more variables: MITTFL <chr>, PPROTFL <chr>, PARAM <chr>,
#' #  PARAMCD <chr>, PARAMN <dbl>, AVAL <dbl>, AVALCAT1 <chr>
#' #  Use `print(n = ...)` to see more rows
#'
#' # Example 2: Fetch a Subset
#' 
#' # Get data with selected columns and where expression
#' dat2 <- fetch(ct$ADEX, select = c("SUBJID", "TRTA", "RANDFL", "SAFFL"),
#'               where = expression(SUBJID == '051'))
#' 
#' # View Data
#' dat2
#' # A tibble: 4 x 4
#' #   SUBJID TRTA  RANDFL SAFFL
#' #   <chr>  <chr> <chr>  <chr>
#' # 1 051    ARM A Y      Y    
#' # 2 051    ARM A Y      Y    
#' # 3 051    ARM A Y      Y    
#' # 4 051    ARM A Y      Y    
#'
#' @export
fetch <- function(catalog, select = NULL, where = NULL, top = NULL, import_specs = NULL) {
  
  ret <- NULL
  
  if ("dinfo" %in% class(catalog)) {
    
    ret <- load_data(catalog, where, top, import_specs, select)
    
  } else {
    
    stop("Function requires a data catalog item as input.") 
  }
  
  return(ret)
}




#' @import tibble
load_data <- function(dinfo, where = NULL, top = NULL, import_specs = NULL, 
                      select = NULL) {
  
  
  # Get the file list according to the engine type
  if (!"dinfo" %in% class(dinfo))
    stop("Class must by 'dinfo'")
      
  dat <- NULL
  eng <- attr(dinfo, "engine")
  pth <- attr(dinfo, "path")
  nm <- attr(dinfo, "name")
  fl <- attr(dinfo, "where")
  
  if (is.null(import_specs)) {
    spc <- attr(dinfo, "import_specs")
    if (!is.null(spc))
      import_specs <- spc
    
  }
  
  # Combine filters if necessary
  if (!is.null(fl)) {
    if (!is.null(where)) {
      
      where <- str2expression(paste(fl, "&", 
                                     as.character(where), collapse = ""))
    } else {
      
      where <- str2expression(fl) 
    }
  }
  
  if (eng == engines$csv) {
    
    
    dat <- get_data_csv(pth, nm, where = where, top = top, 
                        import_specs = import_specs)
    
  } else if (eng == engines$rds) {
    
    dat <- get_data_rds(pth, nm, where = where, top = top, 
                        import_specs = import_specs)
    
  } else if (eng  %in% c(engines$rdata, engines$rda)) {
    
    dat <- get_data_rda(pth, nm, where = where, top = top, 
                        import_specs = import_specs)
    
    
  } else if (eng  == engines$sas7bdat) {
    
    dat <- get_data_sas7bdat(pth, nm, where = where, top = top, 
                        import_specs = import_specs)
    
  } else if (eng  == engines$dbf) {
    
    
    dat <- get_data_dbf(pth, nm, where = where, top = top, 
                        import_specs = import_specs)
    
  } else if (eng == engines$xpt) {
    
    
    dat <- get_data_xpt(pth, nm, where = where, top = top, 
                        import_specs = import_specs)
    
  } else if (eng  == engines$xlsx) {
    
    
    
    dat <- get_data_xlsx(pth, nm, where = where, top = top, 
                        import_specs = import_specs)
    
  } else if (eng == engines$xls) {
    
    
    dat <- get_data_xls(pth, nm, where = where, top = top, 
                        import_specs = import_specs)
  } 
      
  # Deal with select parameter
  if (!is.null(select)) {
    
    nms <- names(dat)
    
    if (typeof(select) == "character") {
      snms <- select
      
      if (all(snms %in% nms)) {
        
        dat <-  dat[ , snms]
        
      } else {
        
        rnms <- c()
        for (nm in snms) {
          if (!nm %in% nms)
            rnms[length(rnms) + 1] <- nm
          
        }
        
        stop(paste0("Select parameter names not found in data: ",
                    paste(rnms, collapse = " ")))
        
      }
      
      
      
    } else if (typeof(select) %in% c("integer", "numeric")) {
      
      snms <- nms[select]
    
      if (any(is.na(snms))) {
        
        pos <- select[is.na(snms)]
        
        stop(paste0("Select parameter positions not found in data: ",
                                 paste(pos, collapse = " ")))
        
      } else {
        
        dat <-  dat[ , snms]
      }
    
    
    } else {
      
      stop("Select parameter type invalid.") 
      
    }
  }
  
  return(dat)
  
}



