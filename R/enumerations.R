#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Creates an enumeration for accessing key/value pairs of information
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param enum_values
#'        [string]: An atomic (scalar) vector of values.
#'
#' @param enum_names
#'        [string]: An atomic (scalar) vector of values. Default is enum_values.
#'
#' @param descriptions
#'        [string]: Description of each enumeration value.  Default is enum_names.
#'
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ list ]
#'
#'    A list of key/value pairs of enumerations
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- create_enum(
#'        enum_values       = c('table1', 'table2', 'listing1', 'listing2'),
#'        enum_names        = c('Table1', 'Table2', 'Listing1', 'Listing2')
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family create Functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   create_enum
#' @title  create_enum
#' @rdname create_enum
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
create_enum <- function(enum_values,
                        enum_names  = if (is.null(names(enum_values))) enum_values else names(enum_values),
                        descriptions = enum_names) {

    if (!is.atomic(enum_values))
        stop(paste("'enum_values' does not contain an atomic (scalar) vector but is a", mode(enum_values), "with class =", class(enum_values)))

    if (!is.atomic(enum_names))
        stop(paste("'enum_names' does not contain an atomic (scalar) vector but is a", mode(enum_names), "with class =", class(enum_names)))

    if (length(enum_values) < 1)
        stop("Enums may not be empty. 'enum_values' must contain at least one element." )

    if (length(enum_values) != length(enum_names))
        stop(paste0("'enum_values' [", length(enum_values), "] and 'enum_names' [", length(enum_names), "] must have the same length"))

    if (length(descriptions) != length(enum_values))
        stop(paste0("'descriptions' [", length(descriptions), "] and 'enum_values' [", length(enum_values), "] must have the same length"))

    new_enum <- as.list(enum_values)
    names(new_enum) <- enum_names

    attr(new_enum, "descriptions") <- descriptions

    class(new_enum) <- append("enumeration", class(new_enum))

    return(new_enum)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Valid table, figure, or listing names.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @export tflname
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family create_enum
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   tflname
#' @title  tflname
#' @rdname tflname
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
tflname <- create_enum(enum_names = PKG_Constants$TFL$Name, enum_values = PKG_Constants$TFL$Name)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Valid data connection types.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @export dctype
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family create_enum
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   dctype
#' @title  dctype
#' @rdname dctype
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
dctype <- create_enum(enum_names = PKG_Constants$dctype$Name, enum_values = PKG_Constants$dctype$Value)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Create the enumeration of filters used for the dc types when ssh applied.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family create_enum
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   dc_ssh_filter
#' @title  dc_ssh_filter
#' @rdname dc_ssh_filter
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
dc_ssh_filter <- create_enum(enum_names = PKG_Constants$dctype$Name, enum_values = PKG_Constants$dctype$SSH_Filter)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Create the enumeration of filters used for the dc types.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family create_enum
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   dc_filter
#' @title  dc_filter
#' @rdname dc_filter
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
dc_filter <- create_enum(enum_names = PKG_Constants$dctype$Name, enum_values = PKG_Constants$dctype$Filter)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Valid output type formats for exporting reports.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @export
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family create_enum
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   output_type
#' @title  output_type
#' @rdname output_type
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
output_type <- create_enum(enum_names = PKG_Constants$outtype$Name, enum_values = PKG_Constants$outtype$Value)

