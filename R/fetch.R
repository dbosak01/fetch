#' @title Fetch data from many data sources
#' @encoding UTF-8
#' @description 
#' The \strong{fetch} package allows you to retrieve data from many different
#' data sources.  The package retrieves data in a memory-efficient manner.
#' You first identify the data by defining a data accessor.  Then fetch
#' the data from the accessor.  Accessors can be defined for many popular 
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




#' @title Gets a dataset from a data catalog
#' @encoding UTF-8
#' @description The \code{fetch} function retrieves a dataset from a data
#' catalog.  The function accepts a catalog item as the first parameter.  The
#' data item is the only required parameter. The "filter" and "top" parameters
#' may be used to define a subset of the data to retrieve.  The "import_specs"
#' parameter accepts an \code{\link{import_spec}} object, which can be used
#' to control how data is read into the data frame.
#' @param catalog The catalog item to fetch data from.
#' @param filter An optional expression to be used to filter the fetched data.  
#' Use the base R \code{expression} function to define the expression.  The
#' expression allows logical operators and Base R functions. Column names
#' can be unquoted.
#' @param top A number of records to return from the data source.  Valid 
#' value is an integer.
#' @param import_specs The import specs to use for the fetch operation. Import
#' specs can be used to control the data types.
#' @seealso The \code{\link{catalog}} function to create a data catalog.
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
#' # Get data with filter expression
#' dat2 <- fetch(ct$ADEX, filter = expression(SUBJID == '051'))
#' 
#' # View Data
#' dat2
#' # A tibble: 4 × 17
#' #  STUDYID USUBJID    SUBJID SITEID TRTP  TRTPN TRTA  TRTAN RANDFL SAFFL
#' #   <chr>   <chr>      <chr>  <chr>  <chr> <dbl> <chr> <dbl> <chr>  <chr>
#' # 1 ABC     ABC-01-051 051    01     ARM A     1 ARM A     1 Y      Y    
#' # 2 ABC     ABC-01-051 051    01     ARM A     1 ARM A     1 Y      Y    
#' # 3 ABC     ABC-01-051 051    01     ARM A     1 ARM A     1 Y      Y    
#' # 4 ABC     ABC-01-051 051    01     ARM A     1 ARM A     1 Y      Y    
#' #  7 more variables: MITTFL <chr>, PPROTFL <chr>, PARAM <chr>,
#' #  PARAMCD <chr>, PARAMN <dbl>, AVAL <dbl>, AVALCAT1 <chr>
#'
#' @export
fetch <- function(catalog, filter = NULL, top = NULL, import_specs = NULL) {
  
  ret <- NULL
  
  if ("dinfo" %in% class(catalog)) {
    
    ret <- load_data(catalog, filter, top, import_specs)
    
  } else {
    
    stop("Function requires a data catalog item as input.") 
  }
  
  return(ret)
}




#' @import tibble
load_data <- function(dinfo, filter = NULL, top = NULL, import_specs = NULL) {
  
  
  # Get the file list according to the engine type
  if (!"dinfo" %in% class(dinfo))
    stop("Class must by 'dinfo'")
      
  dat <- NULL
  eng <- attr(dinfo, "engine")
  pth <- attr(dinfo, "path")
  nm <- attr(dinfo, "name")
  fl <- attr(dinfo, "filter")
  
  if (is.null(import_specs)) {
    spc <- attr(dinfo, "import_specs")
    if (!is.null(spc))
      import_specs <- spc
    
  }
  
  # Combine filters if necessary
  if (!is.null(fl)) {
    if (!is.null(filter)) {
      
      filter <- str2expression(paste(fl, "&", 
                                     as.character(filter), collapse = ""))
    } else {
      
      filter <- str2expression(fl) 
    }
  }
  
  if (eng == engines$csv) {
    
    
    dat <- get_data_csv(pth, nm, filter = filter, top = top, 
                        import_specs = import_specs)
    
  } else if (eng == engines$rds) {
    
    dat <- get_data_rds(pth, nm, filter = filter, top = top, 
                        import_specs = import_specs)
    
  } else if (eng  %in% c(engines$rdata, engines$rda)) {
    
    dat <- get_data_rda(pth, nm, filter = filter, top = top, 
                        import_specs = import_specs)
    
    
  } else if (eng  == engines$sas7bdat) {
    
    dat <- get_data_sas7bdat(pth, nm, filter = filter, top = top, 
                        import_specs = import_specs)
    
  } else if (eng  == engines$dbf) {
    
    
    dat <- get_data_dbf(pth, nm, filter = filter, top = top, 
                        import_specs = import_specs)
    
  } else if (eng == engines$xpt) {
    
    
    dat <- get_data_xpt(pth, nm, filter = filter, top = top, 
                        import_specs = import_specs)
    
  } else if (eng  == engines$xlsx) {
    
    
    
    dat <- get_data_xlsx(pth, nm, filter = filter, top = top, 
                        import_specs = import_specs)
    
  } else if (eng == engines$xls) {
    
    
    dat <- get_data_xls(pth, nm, filter = filter, top = top, 
                        import_specs = import_specs)
  } 
      
      
  
  return(dat)
  
}



