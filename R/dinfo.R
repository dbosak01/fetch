

as.dinfo <- function(dat) {
  
  if (!"dinfo" %in% class(dat))
    class(dat) <- c("dinfo", class(dat))
  
  return(dat)
  
}





#' @title Print a data catalog item
#' @description A class-specific instance of the \code{print} function for 
#' data catalog items.  The function prints the info in a summary manner.  
#' Use \code{verbose = TRUE} to print the data info as a list.
#' @param x The library to print.
#' @param ... Any follow-on parameters.
#' @param verbose Whether or not to print the info in verbose style.
#' By default, the parameter is FALSE, meaning to print in summary style.
#' Verbose style includes a full data dictionary and printing of all 
#' attributes.
#' @return The data catalog object, invisibly.
#' @import crayon
#' @export
print.dinfo <- function(x, ..., verbose = TRUE) {
  
  
    
  # Prepare color
  grey60 <- make_style(grey60 = "#999999")

  # Print a nice header
  
  if (!is.null(attr(x, "nrow"))) {
    cat(grey60(paste0("# data item '", attr(x, "name"), "': ", 
                    attr(x, "ncol"), " cols ", attr(x, "nrow"), " rows\n")))
    
  } else {
    cat(grey60(paste0("# data item '", attr(x, "name"), "':\n"))) 
    
  }
  
  if (!is.null(attr(x, "top")))
    cat(paste0("- Top: ", attr(x, "top"), "\n"))
  if (!is.null(attr(x, "where"))) {
    
    #print(attr(x, "where"))
    if (nchar(as.character(attr(x, "where"))) > 0)
      cat(paste0("- Where: ", as.character(attr(x, "where")), "\n"))
    
  }
  
  if (verbose) {
    
    if (!is.null(attr(x, "engine")))
      cat(paste0("- Engine: ", attr(x, "engine"), "\n"))
    
    if (!is.null(attr(x, "size")))
      cat(paste0("- Size: ", attr(x, "size"), "\n"))
    
    if (!is.null(attr(x, "modified")))
      cat(paste0("- Last Modified: ", attr(x, "modified"), "\n"))
    
    if (nrow(x) > 0) {
      #cat("- dictionary:\n")
      print(as.data.frame(x))
    }
  }

  
  
  invisible(x)
}
