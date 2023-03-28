#' @title Fetch data from many data sources
#'
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
#' @name fetch
NULL




#' @title A generalized function to fetch data
#' @param catalog The catalog item to fetch data from.
#' @param filter An expression to be used to filter the fetched data.
#' @param top A number of records to return from the data source.  Valid 
#' value is an integer.
#' @param import_specs The import specs to use for the fetch operation. Import
#' specs can be used to control the data types.
#' @export
fetch <- function(catalog, filter = NULL, top = NULL, import_specs = NULL) {
  
  
  ret <- NULL
  
  
  if ("dinfo" %in% class(catalog)) {
    
    
    ret <- load_data(catalog, filter, top, import_specs)
    
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



