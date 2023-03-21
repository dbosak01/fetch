#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Check if a report object
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param report
#'        [string]: A valid report object.
#'
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ boolean ]
#'
#'        Returns True if the report is a report object else false
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- is.report(
#'        name  = rpt
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family is Functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   is.report
#' @title  is.report
#' @rdname is.report
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

is.report <- function(report) {

    return(ifelse(inherits(report, "report"), TRUE, FALSE))

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Check if a valid TFL object
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param tfl
#'        [string | TFL Class]: A valid TFL object
#'
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ boolean ]
#'
#'        Returns TRUE if a valid TFL object else FALSE
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- is.tfl(
#'        tfl = tfl_Object
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family is Functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   is.tfl
#' @title  is.tfl
#' @rdname is.tfl
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

is.tfl <- function(tfl) {

    if ( inherits(tfl, PKG_Constants$TFL$Class) ) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Check if the ssh parameter is a valid ssh object
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param ssh
#'        [shinyauth::SSH]: A shinyauth SSH object.
#'
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ boolean ]
#'
#'    TRUE if a valid ssh object else FALSE
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- is.ssh(
#'        ssh  = shinyauth::SSH$new(user_name = <user_name>, user_pass = <password>)
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family is Functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   is.ssh
#' @title  is.ssh
#' @rdname is.ssh
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

is.ssh <- function(ssh) {

    return(ifelse(inherits(ssh, "SSH"), TRUE, FALSE))

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Check if the data_connection parameter is a valid data connection object
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param data_connection
#'        [class]: A data_connection object.
#'
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ boolean ]
#'
#'    TRUE if a valid data_connection object else FALSE
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- is.data_connection(
#'        data_connection  = dc_Object
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family is Functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   is.data_connection
#' @title  is.data_connection
#' @rdname is.data_connection
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

is.data_connection <- function(data_connection) {

    return(ifelse(inherits(data_connection, "data_connection"), TRUE, FALSE))

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Check if a character string is blank
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param string
#'        [string]: A valid character string
#'
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ boolean ]
#'
#'        Returns TRUE if the string is NULL, NA, or all white space else FALSE
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- is.blank(
#'        string = "blah"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family is Functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   is.blank
#' @title  is.blank
#' @rdname is.blank
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

is.blank <- function(string) {

    string <- trimws(string)

    if (is.na(string) || length(string) == 0 || nchar(string) == 0 ) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Check if a value is na or null (for use against attribute values to detect whether they are
#'    set to anything.)
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param value
#'        [object]: A value to be checked for na or null
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ boolean ]
#'
#'        Returns TRUE if the value is NULL or NA else FALSE
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- is.null_na(
#'        value = "blah"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family is Functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   is.null_na
#' @title  is.null_na
#' @rdname is.null_na
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

is.null_na <- function(value) {
    # This function is intended for use against attribute values to detect whether they are set to anything.
    # If the input value is > length 1, it has some sort of structure in it and should not be considered NULL or NA
    # Else if the input value has a length of zero, only check for NULL since a single NA value returns a length of 1
    # Else there is exactly one item to check for NA or NULL
    if (length(value) > 1) {
        return(FALSE)
    } else if (length(value) == 0) {
        return(is.null(value))
    } else {
        return(is.null(value) || is.na(value))
    }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Check if a value is a valid reviewer type.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param value
#'        [object]: A reviewer type to be checked
#'
#' @param logger
#'        [MessageRecorder Class]: Logger object for storing error log messages.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ boolean ]
#'
#'        Returns TRUE if the value is valid else FALSE
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- is.valid_reviewer_type(
#'        value = "blah"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @family is Functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   is.valid_reviewer_type
#' @title  is.valid_reviewer_type
#' @rdname is.valid_reviewer_type
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

is.valid_reviewer_type <- function(value, logger) {

    if (value %in% PKG_Constants$reviewer_type$Value) {
        return(TRUE)
    } else {
        logger$addError(glue::glue("The reviewer type ({value}) was not valid!") )
        return(FALSE)
    }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Filter the data
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param data
#'        [data_frame]: Input data to filter.
#'
#' @param filter
#'        [string]: A list of filter strings
#'
#' @param popflag
#'        [string]: The default value for popflag if required.
#'
#' @param logger
#'        [MessageRecorder Class]: Logger object for storing error log messages.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ data_frame ]
#'
#'    A data frame containing the filtered data
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- filter_data(
#'        data    = adae,
#'        filter  = list(adsl = "enrlfl == 'Y'")
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   filter_data
#' @title  filter_data
#' @rdname filter_data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

filter_data <- function(data = NULL, filter = NULL, popflag = NULL, logger = NULL) {

    # Retrieve the name of the dataset for logging purpose
    dsname <- deparse(substitute(data))

    # Add the popflag if required for filtering
    if (!is.null_na(popflag) ) {

        # Build the popflag condition from the default value
        popflag_filter <- stringr::str_c(popflag, "== 'Y'", collapse = "")

        # Append the popflag filter to the main filter condition
        filter <- stringr::str_c(filter, popflag_filter, sep = " & ")
    }

    # Apply the filter to the data
    if (!is.null_na(filter)) {

        tryCatch(
            {
                prev_nrows <- nrow(data)
                data       <- dplyr::filter(data, !!rlang::parse_expr(as.character(filter)))

                # Add logging messages
                if (!is.null(logger)) {

                    logger$addInfo(glue::glue("{dsname} contains {prev_nrows} records."))

                    nrows <- nrow(data)
                    msg   <- glue::glue("There were {nrows} records returned from {dsname} after applying the filter: {filter}")

                    if (nrows == 0) {
                        logger$addAlert(msg)
                    } else {
                        logger$addInfo(msg)
                    }
                }
            },

            error = function(err)
            {
                stop(paste0("Problem with `filter()` condition: ", filter, "\n", err))
            })

    }

    return(data)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Build the treatment columns and return only records containing the specified treatments.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param data
#'        [data.frame]: A data frame containing the treatment columns.
#'
#' @param treatment_var
#'        [string]: Name of the treatment variable.
#'
#' @param header_definition
#'        [list]: Header definition attributes containing treatment names, values, and labels.
#'
#' @param add_total
#'        [Logical]: Adds the total records if set to TRUE and treatments is null or empty.
#'
#' @param logger
#'        [MessageRecorder Class]: Logger object for storing error log messages.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ data.frame ]
#'
#'    A data frame containing only the selected treatment records.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- filter_treatment_labels(
#'        data              = adsl,
#'        treatment_var     = "trt01p",
#'        header_definition = tfl$attributes$header_definition
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   filter_treatment_labels
#' @title  filter_treatment_labels
#' @rdname filter_treatment_labels
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

filter_treatment_labels <- function(data = NULL, treatment_var = NULL, header_definition = NULL, add_total = TRUE, logger = NULL) {

    anl_data <- data

    if (!is.null_na(treatment_var) && is.data.frame(data)) {

        # Retrieve the name of the dataset for logging purpose
        dsname                <- deparse(substitute(data))
        has_header_definition <- FALSE

        # Use Header definition for getting the treatment labels and filter if exists
        if (is.data.frame(header_definition) && nrow(header_definition) > 0 && treatment_var %in% (header_definition$Variable) ) {

            has_header_definition <- TRUE

            # Get the header_definition attributes for the specified treatment variable preserving the order read in
            trt_df <- header_definition %>%
                dplyr::filter(Variable == treatment_var) %>%
                tidyr::unnest(cols = Value) %>%
                dplyr::rename(!!rlang::sym(treatment_var) := Value, trt = Column_Label)

            # Create a unique ordering value for each treatment label based on how it was read in
            trt_order <- trt_df %>%
                dplyr::distinct(trt) %>%
                tibble::rowid_to_column("trtn")

            # Join the numeric order back to the treatments
            trt_df <- trt_df %>%
                dplyr::left_join(trt_order, by = "trt") %>%
                dplyr::select(treatment_var, trt, trtn, trt_var = Variable)

            # Check the treatment variable type matches the variable in the data else convert
            if (typeof(trt_df[[treatment_var]]) != typeof(anl_data[[treatment_var]]) ) {

                if (typeof(anl_data[[treatment_var]]) == "character") {
                    trt_df[[treatment_var]] <- as.character(trt_df[[treatment_var]])
                } else {
                    trt_df[[treatment_var]] <- as.numeric(trt_df[[treatment_var]])
                }
            }

            # Remove the total column records if not required
            if (!add_total) {

                trt_df <- trt_df %>%
                    dplyr::filter(!grepl("overall", trt, ignore.case = TRUE) ) %>%
                    dplyr::filter(!grepl("total", trt, ignore.case = TRUE) )

            }

        } else {  # Use the data to create the trt, trtn variables along with the total records

            # Initialize sorting and character treatment variables
            sortordervars   <- treatment_var
            treatment_var_c <- treatment_var

            # Check if a numeric treatment variable exists when the treatment variable is a character variable
            if (is.character(anl_data[[treatment_var]]) ) {

                # Add the numeric treatment variable if exists in data to the sortordervars by adding character (N) to end of variable name
                if (paste0(treatment_var, "n") %in% colnames(data) ) {
                    sortordervars <- c(paste0(treatment_var, "n"),  sortordervars)
                }

            } else { # Check if a character treatment variable exists when treatment variable is numeric

                # Assign the character treatment variable by removing the last character (N) from the string if exists in data
                if (stringr::str_sub(treatment_var, start = -1) == "n" && substr(treatment_var, 1, nchar(treatment_var) - 1) %in% colnames(data) ) {
                    treatment_var_c <- substr(treatment_var, 1, nchar(treatment_var) - 1)
                }

            }

            # Assign the order using the numeric treatment variable if exist else just the character variable
            trt_df <- anl_data %>%
                dplyr::arrange(!!!rlang::syms(sortordervars)) %>%
                dplyr::mutate(trt = as.character(!!rlang::sym(treatment_var_c) )) %>%
                dplyr::distinct(!!rlang::sym(treatment_var), trt) %>%
                tibble::rowid_to_column("trtn")


            # Add in the total records if requested
            if (add_total) {

                # Add in the total column records
                tot_df <- trt_df %>%
                    dplyr::mutate(trt = "Overall", trtn = as.integer(max(trtn) + 1))

                # Combine the treatment and total
                trt_df <- dplyr::bind_rows(trt_df, tot_df)
            }

        }

        prev_nrows <- nrow(anl_data)

        # Join the treatment values back into the main data and subset out treatments not in both
        anl_data <- anl_data %>%
            dplyr::inner_join(trt_df, by = treatment_var) %>%
            dplyr::mutate(trt_var = treatment_var)

        nrows <- nrow(anl_data)

        # Add logging messages
        if (!is.null(logger)) {

            if (has_header_definition) {

                logger$addInfo(glue::glue("{dsname} contains {prev_nrows} records."))
                logger$addInfo(glue::glue("Filtering {dsname} using header definition values contained in the {treatment_var} column."))

                msg <- glue::glue("There were {nrows} records returned from {dsname} after merging with the header definition data.")

                if (nrows == 0) {
                    logger$addAlert(msg)
                } else {
                    logger$addInfo(msg)
                }

                # Check if any header definition values were not found in the analysis data
                missing_values <- setdiff(trt_df[[treatment_var]], anl_data[[treatment_var]])

                if (length(missing_values) > 0)
                    logger$addAlert(glue::glue("The following value from the header definition data was not found in {treatment_var} of {dsname}: {missing_values}."))
            }
        }

    }

    return(anl_data)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'   Filter a data set by one or more subject level data sets.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param data
#'        [data frame]: The data frame to apply the subject level data filter to.
#'
#' @param data_connection
#'        [Data Connection Class]: Data connection object.
#'
#' @param subject_level_data
#'        [string]: One or more subjct level data sets within the data connection to filter by.
#'
#' @param filter
#'        [string]: A valid R filter syntax for sub setting the subject level data data
#'
#' @param logger
#'        [MessageRecorder Class]: Logger object for storing error log messages.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [data frame]
#'
#'    A data frame containing the filtered out records from the subject level data set.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'    result <- filter_subject_level_data(
#'        data               = "adsl",
#'        data_connection    = dc,
#'        subject_level_data = c("biomark1", "biomark2")
#'        filter             = list(biomark1 = "usubjid = '11111'")
#'        logger             = mc
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   filter_subject_level_data
#' @title  filter_subject_level_data
#' @rdname filter_subject_level_data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

filter_subject_level_data <- function(data = NULL, data_connection = NULL, subject_level_data = NULL, filter = NULL, logger = NULL) {

    # Initialize the data frame that will capture all usubjid records to subset from main data
    subject_df <- NULL

    if (!is.null_na(subject_level_data)) {

        # Loop through all the subject level data set names applying any custom filtering and extract the remaining usubjid values
        for(subj_ds_name in subject_level_data) {

            # Check if the subject level data set name is part of the data connection
            if (subj_ds_name %in% names(data_connection)) {

                # Check if this is a valid subject level data set before processing
                if (check_subject_level_data(name = subj_ds_name, data_connection = data_connection, logger = logger)) {

                    # Extract the subject level data set from the data connection and apply any custom filtering
                    df <- data_connection[[subj_ds_name]]()
                    df <- filter_data(data = df, filter = filter[[subj_ds_name]]) %>%
                        dplyr::select(usubjid)

                    # Combine the subject ids from the current data set with any other subject level data sets subject ids keeping only the
                    # subject ids that exist in both
                    if (is.null(subject_df)) {

                        subject_df <- df

                    } else {

                        subject_df <- dplyr::inner_join(df, subject_df, by = "usubjid")
                    }

                } else {

                    logger$addError(glue::glue("The subject level data set name ({subj_ds_name}) was not valid for filtering!") )
                    data <- NULL
                }

            } else {

                logger$addError(glue::glue("The subject level data set name ({subj_ds_name}) was not found in the input data!") )
                data <- NULL

            }
        }

        # Filter out the subjects from the data that were not found in the subject level data sets
        if (is.data.frame(subject_df) && is.data.frame(data)) {

            dsname     <- deparse(substitute(data))
            prev_nrows <- nrow(data)

            data <- data %>%
                dplyr::inner_join(subject_df, by = "usubjid")

            logger$addInfo(glue::glue("{dsname} contains {prev_nrows} records."))

            nrows <- nrow(data)
            msg   <- glue::glue("There were {nrows} records returned from {dsname} after applying the subject level data filter.")

            if (nrows == 0) {
                logger$addError(msg)
            } else {
                logger$addInfo(msg)
            }

        } else {

            data <- NULL
        }
    }

    return(data)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Check the data matches whats expected for the tfl and return the data or NULL if issues found.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param name
#'        [string]: A data set name to be checked.
#'
#' @param attributes
#'        [list]: TFL attributes.
#'
#' @param data_connection
#'        [Data Connection Class]: Data connection object.
#'
#' @param logger
#'        [MessageRecorder Class]: Logger object for storing error log messages.
#'
#' @param ...
#'        [string]: Additional columns to check.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ Data Frame ]
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- check_data(
#'        name            = "adsl"
#'        attributes      = "tfl$attributes",
#'        data_connection = dc,
#'        logger          = tfl$logger
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   check_data
#' @title  check_data
#' @rdname check_data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

check_data <- function(name = NULL, attributes = NULL, data_connection = NULL, logger = NULL, ...) {

    args <- list(...)
    args <- args[lengths(args) != 0]

    data <- NULL

    tryCatch(
        {
            # Verify the data connector contains the expected data set
            if (name %in% names(data_connection)) {

                data <- data_connection[[name]]()

                logger$addInfo(glue::glue("{name} was read in from the data connection with {nrow(data)} rows and {ncol(data)} columns."))

                if ( !is.null_na(attributes$datasets[[name]]) ) {

                    expected_columns <- unique(c(args, attributes$datasets[[name]]))

                    # Check required columns exist in the data.  Regular expression accepted.
                    missing_columns <- purrr::map_chr(expected_columns, function(name, colnames) {

                        # Add start and end anchors so column names have exact matches
                        if (!any(grepl(paste0("^", name, "$"), colnames))) {

                            return(gsub("|", " or ", name, fixed = TRUE))

                        } else {

                            return(NA_character_)

                        }
                    }, colnames(data))

                    # Remove any na values from the character vector
                    missing_columns <- missing_columns[!is.na(missing_columns)]

                    if (length(missing_columns) > 0) {

                        logger$addError(stringr::str_c("The following columns in the data set '", name, "' are required to exist: (",
                                                       stringr::str_c(missing_columns, collapse = ", "), ")"))

                        # Reset the data to null since missing columns were found
                        data <- NULL
                    }
                }
            }
            else {

                # Reset the data to NULL since the data set was not found in the connection object
                data <- NULL
                logger$addError(stringr::str_c("The required data set '", name, "' was not found in the source data!"))
            }
        },
        warning = function(war)
        {
            data <- NULL
            tfl$logger$addWarning(stringr::str_c("Warning occurred while checking for expected columns in the data.\n",  war))
        },
        error = function(err)
        {
            data <- NULL
            tfl$logger$addError(stringr::str_c("Error occurred while checking for expected columns in the data.\n",  err))
        })

    return(data)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Check the data matches whats expected for the tfl
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param tfl
#'        [list]: TFL class object.
#'
#' @param filter
#'        [string]: A list contain the names of data sets along with there filter conditions.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ Logical ]
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- check_filter(
#'        tfl    = tfl_object
#'        filter = list(adsl = "sex == 'F'")
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   check_filter
#' @title  check_filter
#' @rdname check_filter
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

check_filter <- function(tfl = NULL, filter = NULL) {

    chk_filter <- FALSE

    if (is.tfl(tfl)) {

        tryCatch(
            {
                # Get all the data sets required to build the TFL
                required_datasets <- c(names(tfl$attributes$datasets), tfl$attributes$subject_level_data)

                if ( !is.null_na(filter) ) {

                    # Check the data sets in the filtering condition are required by the TFL
                    missing_domains <- setdiff(names(filter), required_datasets)

                    if (length(missing_domains) > 0) {
                        tfl$logger$addError(stringr::str_c("The following domain(s) '", stringr::str_c(missing_domains, collapse = ", "),
                                                           "' in the filter condition are not valid for the TFL.\n",
                                                           "Valid domains include: ", stringr::str_c(required_datasets, collapse = ", ")))
                    } else {
                        chk_filter <-TRUE
                    }

                } else {
                    chk_filter <- TRUE
                }
            },
            warning = function(war)
            {
                tfl$logger$addWarning(stringr::str_c("Warning occurred while checking filter data set names.\n",  war))
            },
            error = function(err)
            {
                tfl$logger$addError(stringr::str_c("Error occurred while checking filter data set names.\n",  err))
            })
    }

    return(chk_filter)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Check the path exist depending on if non ssh is used
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param path
#'        [string]: The path to check.
#'
#' @param ssh
#'        [string]: A valid ssh object.
#'
#' @param logger
#'        [R6 Class]: A message recorder object.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ Logical ]
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- chk_path_exists(
#'        path  = "/userdata/cfda/data_science/projects/oncvis"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   chk_path_exists
#' @title  chk_path_exists
#' @rdname chk_path_exists
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

chk_path_exists <- function(path, ssh = NULL, logger = NULL) {

    path_exists <- FALSE

    if (!is.ssh(ssh)) {

        path_exists <- file.exists(path)

    } else {

        # Verify the ssh connection is logged in
        verify_ssh_login(ssh, logger)

        path_exists <- ssh$path_exists(path)

    }

    return(path_exists)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Check if write access is allowed to a directory
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param path
#'        [string]: The path to check.
#'
#' @param ssh
#'        [string]: A valid ssh object.
#'
#' @param logger
#'        [R6 Class]: A message recorder object.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ Logical ]
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- chk_write_access(
#'        path  = "/userdata/cfda/data_science/projects/oncvis"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   chk_write_access
#' @title  chk_write_access
#' @rdname chk_write_access
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

chk_write_access <- function(path, ssh = NULL, logger = NULL) {

    is_writable <- FALSE

    tryCatch(
        {
            if (!is.ssh(ssh)) {

                # Check if a directory is writable by touching a file to it and removing.
                # This was added because of an issue in file.access not working correctly on the DSW
                tmp_file    <- file.path(path, tempfile(".tmp", ""))
                is_writable <- file.create(tmp_file, showWarnings = FALSE)

                # Remove the temporary file if created
                if (is_writable) {
                    file.remove(tmp_file)
                }

            } else {

                # Verify the ssh connection is logged in
                verify_ssh_login(ssh, logger)

                is_writable <- ssh$is_directory_writable(path)

            }
        },

        error = function(err)
        {
            # An error occurred trying to access the location.  is_writable will return false to the calling function
            return(NULL)
        })

    return(is_writable)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Add a data connection object to the report
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param report
#'        [string]: A report object.
#'
#' @param data_connection
#'        [string]: A data connection object.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'
#'    Type: [list]
#'
#'    A report object with the data connection attached.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- add_data_connection(
#'        report          = rpt,
#'        data_connection = dc
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   add_data_connection
#' @title  add_data_connection
#' @rdname add_data_connection
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

add_data_connection <- function(report = NULL, data_connection = NULL) {

    if (is.report(report)) {

        # Initialize the attribute to NULL for report object that already contain an existing data connection.
        report$data_source     <- NULL
        report$data_type       <- NULL
        report$data_connection <- NULL

        if (is.data_connection(data_connection)) {

            # Assign the data source to the report object that points to the location of the data
            report$data_source <- convert_path(attributes(data_connection)$data_source)

            # Assign the data type to the report object that contains the type of data being pulled
            report$data_type <- attributes(data_connection)$data_type

            # Attach the data connection object
            report$data_connection <- data_connection
        }

    }

    return(report)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Verify an ssh connection is logged in else perform a login.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param ssh
#'        [ssh class]: shinyauth SSH object.
#'
#' @param logger
#'        [R6 Class]: A message recorder object.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'
#'    Type: [list]
#'
#'    A logged in ssh connection.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- verify_ssh_login(
#'        ssh = ssh_conn
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   verify_ssh_login
#' @title  verify_ssh_login
#' @rdname verify_ssh_login
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

verify_ssh_login <- function(ssh = NULL, logger = NULL) {

    if (is.ssh(ssh) && !ssh$is_logged_in()) {

        ssh$login()

        if (!is.null(logger)) {
            logger$addInfo(paste0("Login to ssh connection complete!"))
        }

    }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Function to accept the current titles and adjust them based on the new and old popflag.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param titles
#'        [character]: Character value of titles, delimited by the value in the delimiter parameter
#'
#' @param popflag_old
#'        [character]: The previous popflag value (corresponds with the input titles).
#'
#' @param popflag_new
#'        [character]: The new popflag value (titles will be updated to align with this popflag).
#'
#' @param delimiter
#'        [character]: The delimiter used between lines in the input and output titles.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [character]
#'
#'    The input titles, modified to remove any popflag title for popflag_old and to add a popflag title for popflag_new.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'    result <- update_titles_for_popflag(
#'        titles      = "Title 1{\\line}Title 2{\\line}(Safety Analysis Set)",
#'        popflag_old = "saffl",
#'        popflag_new = "enrlfl",
#'        delimiter   = "{\\line}"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   update_titles_for_popflag
#' @title  update_titles_for_popflag
#' @rdname update_titles_for_popflag
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
update_titles_for_popflag <- function(titles, popflag_old, popflag_new, delimiter = "{\\line}") {

    if (is.null_na(titles)) {
        return(titles)
    }

    # Find the title for the old and new popflags
    old_popflag_title <- trim_to_empty(PKG_Constants$popflag %>% dplyr::filter(Name == trim_to_empty(popflag_old, convert_null = TRUE)) %>% dplyr::pull(Title))
    new_popflag_title <- trim_to_empty(PKG_Constants$popflag %>% dplyr::filter(Name == trim_to_empty(popflag_new, convert_null = TRUE)) %>% dplyr::pull(Title))

    old_popflag_title <- ifelse(length(old_popflag_title) == 0, "", old_popflag_title)
    new_popflag_title <- ifelse(length(new_popflag_title) == 0, "", new_popflag_title)

    # If the old and new titles are different, modify the titles accordingly
    if (old_popflag_title != new_popflag_title) {
        if (old_popflag_title != "") {
            # If the old popflag's title is present, replace it with the new popflag's title
            out_titles <- gsub(old_popflag_title, new_popflag_title, titles, fixed = TRUE)
        } else {
            out_titles <- titles
        }

        # If the title was unchanged by the replacement and the new title is not already present, add the new popflag title to the end
        if (out_titles == titles && new_popflag_title != "" && !grepl(new_popflag_title, out_titles, fixed = TRUE)) {
            out_titles <- paste0(out_titles, ifelse(is.null_na(delimiter), "", delimiter), "(", new_popflag_title, ")")
        }
    } else {
        out_titles <- titles
    }

    return(out_titles)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Function to accept the current titles and adjust them based on the new and old Reviewer Type.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param titles
#'        [character]: Character value of titles, delimited by the value in the delimiter parameter
#'
#' @param reviewer_type_old
#'        [character]: The previous reviewer_type value (corresponds with the input titles).
#'
#' @param reviewer_type_new
#'        [character]: The new reviewer_type value (titles will be updated to align with this reviewer_type).
#'
#' @param delimiter
#'        [character]: The delimiter used between lines in the input and output titles.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [character]
#'
#'    The input titles, modified to remove any reviewer_type title for reviewer_type_old and to add a reviewer_type title for reviewer_type_new.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'    result <- update_titles_for_reviewer_type(
#'        titles            = "Title 1 (local){\\line}Title 2",
#'        reviewer_type_old = "Investigator",
#'        reviewer_type_new = "Independent Assessor",
#'        delimiter         = "{\\line}"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   update_titles_for_reviewer_type
#' @title  update_titles_for_reviewer_type
#' @rdname update_titles_for_reviewer_type
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
update_titles_for_reviewer_type <- function(titles, reviewer_type_old, reviewer_type_new, delimiter = "{\\line}") {

    if (is.null_na(titles)) {
        return(titles)
    }

    # Find the title for the old and new reviewer_types (add parentheses now since the titles may be a common word)
    old_reviewer_type_title <- trim_to_empty(PKG_Constants$reviewer_type %>% dplyr::filter(Value == trim_to_empty(reviewer_type_old, convert_null = TRUE)) %>% dplyr::pull(Title))
    new_reviewer_type_title <- trim_to_empty(PKG_Constants$reviewer_type %>% dplyr::filter(Value == trim_to_empty(reviewer_type_new, convert_null = TRUE)) %>% dplyr::pull(Title))

    old_reviewer_type_title <- ifelse(length(old_reviewer_type_title) == 0, "", paste0("(", old_reviewer_type_title, ")"))
    new_reviewer_type_title <- ifelse(length(new_reviewer_type_title) == 0, "", paste0("(", new_reviewer_type_title, ")"))

    # If the old and new titles are different, modify the titles accordingly
    if (old_reviewer_type_title != new_reviewer_type_title) {
        if (old_reviewer_type_title != "") {
            # If the old reviewer_type's title is present, replace it with the new reviewer_type's title (properly clean up any trailing white space if old title is removed rather than replaced)
            if (new_reviewer_type_title != "") {
                out_titles <- gsub(old_reviewer_type_title, new_reviewer_type_title, titles, fixed = TRUE)
            } else {
                out_titles <- gsub(paste0("\\s*\\Q", old_reviewer_type_title, "\\E"), "", titles, fixed = FALSE, perl=TRUE)
            }
        } else {
            out_titles <- titles
        }

        # If the title was unchanged by the replacement and the new title is not already present, add the new reviewer_type title to the end of the first line
        if (out_titles == titles && new_reviewer_type_title != "" && !grepl(new_reviewer_type_title, out_titles, fixed = TRUE)) {
            delimiter <- ifelse(is.null_na(delimiter), "", delimiter)
            if (delimiter == "" || !grepl(delimiter, out_titles, fixed = TRUE)) {
                # Single line title, add to the end.  Use trimws in case there is no other title text.
                out_titles <- trimws(paste(out_titles, new_reviewer_type_title))
            } else {
                # Multiple line title, add to the end of the first line
                out_titles <- sub(delimiter, paste0(" ", new_reviewer_type_title, delimiter), out_titles, fixed = TRUE)
            }
        }
    } else {
        out_titles <- titles
    }

    return(out_titles)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Remove whitespace from the front and back of strings, leave NA and NULL alone.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param the_value
#'        [character]: The value to trim
#'
#' @param convert_null
#'        [boolean]: Convert nulls to space if TRUE.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [character]
#'
#'    **trimmed value**
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'  assertthat::assert_that(is.na(trim_to_empty(NA)))
#'  assertthat::assert_that(identical(NULL, trim_to_empty(NULL)))
#'  assertthat::assert_that(identical(c(), trim_to_empty(c())))
#'  assertthat::assert_that(identical(c(NULL), trim_to_empty(c(NULL))))
#'  assertthat::assert_that(identical(c(NA), trim_to_empty(c(NA))))
#'  assertthat::assert_that(""    == trim_to_empty(""))
#'  assertthat::assert_that(""    == trim_to_empty(" "))
#'  assertthat::assert_that(""    == trim_to_empty(c(" \n ")) )
#'  assertthat::assert_that("something"    == trim_to_empty(" something "))
#'  assertthat::assert_that("something\nelse"    == trim_to_empty(" something \n else "))
#'  assertthat::assert_that(
#'            identical(
#'                c("one","two\nthree"),
#'                trim_to_empty(c(" one ", " two \n  three "))
#'            )
#'  )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   trim_to_empty
#' @title  trim_to_empty
#' @rdname trim_to_empty
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
trim_to_empty <- function(the_value, convert_null = FALSE){
    if (convert_null && (is.null(the_value))) {
        the_value <- ""
    }

    len <- length(the_value)

    if (len > 0) {
        for ( i in 1:len) {
            tv <- the_value[[i]]
            if (convert_null && (is.null(tv) || (length(tv) == 1 && is.na(tv)))) {
                tv <- ""
            }

            if (is.character(tv)) {
                tmp <- unlist(strsplit(tv,"\n"))
                if (length(tmp) == 0) {
                    tmp <- ""
                }
                tmp <- trimws(tmp, "both")
                if (!all(is.na(tmp))) {
                    tv  <- trimws(paste0(tmp, collapse = "\n"))
                } else {
                    tv <- tmp
                }
            }
            the_value[[i]] <- tv
        }
    }

    return(the_value)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'   Check if a subject level data set is valid.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param name
#'        [string]: The name of the data set to check.
#'
#' @param data_connection
#'        [Data Connection Class]: Data connection object.
#'
#' @param logger
#'        [MessageRecorder Class]: Logger object for storing error log messages.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [logical]
#'
#'    True:  The data is a valid subject level data set.
#'    False: The data is not a valid subject level data set.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'    result <- check_subject_level_data(
#'        data             = "biomark1",
#'        data_connection  = dc,
#'        logger           = mc
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   check_subject_level_data
#' @title  check_subject_level_data
#' @rdname check_subject_level_data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

check_subject_level_data <- function(name = NULL, data_connection = NULL, logger = NULL) {

    valid_subject_data <- TRUE

    if (!is.null_na(name) && is.data_connection(data_connection)) {

        # Check if the name of the data set exists
        if (name %in% names(data_connection)) {

            data <- data_connection[[name]]()

            if (!('usubjid' %in% colnames(data))) {

                # Produce an error if the data set does not contains the required usubjid column
                valid_subject_data <- FALSE
                logger$addError(glue::glue("The data set ({name}) does not contain a column for usubjid!") )

            } else if (any(duplicated(data$usubjid))) {

                # Produce an error if the data set is not a subject level data set with one record per usubjid
                valid_subject_data <- FALSE
                logger$addError(glue::glue("The data set ({name}) does not contain a unique record for each usubjid. ",
                                           "A subject level data set containing unique usubjid records is required!") )

            }

        } else {

            # Produce an error if the data set name was not found
            valid_subject_data <- FALSE
            logger$addError(glue::glue("The data set name ({name}) was not found while verifying the subject level data!") )
        }
    } else {

        valid_subject_data <- FALSE
        logger$addError(glue::glue("A valid name and data connection object is required for checking the subject level data!") )

    }

    return(valid_subject_data)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'   Function to convert a path to a unix style path.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param path
#'        [string]: A string containing a filename path.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [string]
#'
#'    A string with the path converted to unix form
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'    result <- convert_path(
#'        path = "\\userdata\\test"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   convert_path
#' @title  convert_path
#' @rdname convert_path
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

convert_path <- function(path = NULL) {

    if (!is.null(path) && is.character(path)) {

        # Convert any backslashed to forward slashes
        path <- gsub("\\\\", "/", path)
    }

    return(path)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'   Update a string to add the escape character when writing to a file.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param string
#'        [string]: A character vector containing escape character.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [character vector]
#'
#'    A character vector with the escape characters updated for printing to a file
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'    result <- add_escape_char(
#'        string = "this is a test\" "
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   add_escape_char
#' @title  add_escape_char
#' @rdname add_escape_char
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

add_escape_char <- function(string = NULL) {

    if (!is.null(string) && is.character(string)) {

        # Update backslash to double backslash
        string <- gsub(x = string, pattern = "\\", replacement = "\\\\", fixed = TRUE)

        # Updated double quote to backslash double quote
        string <- gsub(x = string, pattern = '\"', replacement = '\\\"', fixed = TRUE)

        # Updated newline to backslash newline
        string <- gsub(x = string, pattern = '\n', replacement = '\\n', fixed = TRUE)
    }

    return(string)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Generate a random report ID
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param prefix
#'        [string]: A character string to prefix to the beginning of the randomly generated report ID
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ string ]
#'
#'        Returns a 5 digit report id with the value of prefix attached to the beginning.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- generate_report_id(
#'        string = "f"
#'    )
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   generate_report_id
#' @title  generate_report_id
#' @rdname generate_report_id
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

generate_report_id <- function(prefix = "r") {

    prefix <- ifelse(is.blank(prefix), "", prefix)

    report_id <- paste0(prefix, paste0(sample(0:9, 5), collapse = ""))

    return(report_id)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Update the report object and the TFLs it contains to the requested report_id value
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param object
#'        [class]: A valid report object.
#'
#' @param report_id
#'        [string]: A report ID that will be assigned on the report object and all of its TFLs.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ object ]
#'
#'        Returns a report object with the report ID set on the object and all its TFLs.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- set_report_id(
#'        object    = report,
#'        report_id = "r12345"
#'    )
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   set_report_id
#' @title  set_report_id
#' @rdname set_report_id
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

set_report_id <- function(object = NULL, report_id = NULL) {

    # NA is used to retain the report_id attribute in the list
    if (is.blank(report_id))
        report_id <- NA_character_

    if (is.report(object)) {

        # Update the report level id to the new report id
        object$attributes$report_id <- report_id

        # Reset the report_id on all the tfl objects to the new report id
        for (index in seq_along(object$tfl)) {
            object$tfl[[index]]$attributes$report_id <- report_id
        }
    }

    return(object)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Update the report and all TFLs within it to the requested app_version value
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param object
#'        [class]: A valid report object.
#'
#' @param app_version
#'        [string]: An app_version that will be assigned on the report object and all of its tfl's.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ object ]
#'
#'        Returns a report object with the app_version set on the object and all its TFLs.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'    result <- set_app_version(
#'        object      = report,
#'        app_version = "01.02.00"
#'    )
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   set_app_version
#' @title  set_app_version
#' @rdname set_app_version
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

set_app_version <- function(object = NULL, app_version = NULL) {

    # NA is used to retain the app_version attribute in the list
    if (is.blank(app_version))
        app_version <- NA_character_

    if (is.report(object)) {

        # Update the report level app_version to the new app_version
        object$app_version <- app_version

        # Reset the app_version on all the tfl objects to the new app_version
        for (index in seq_along(object$tfl)) {
            object$tfl[[index]]$app_version <- app_version
        }
    }

    return(object)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Get the name for the run_all file
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param report_id
#'        [string]: A report ID that gets added to the end of the run script name.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ string ]
#'
#'        Returns a string containing the name of the run_all file.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- get_run_all_script_name(
#'        report_id = "r12345"
#'    )
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   get_run_all_script_name
#' @title  get_run_all_script_name
#' @rdname get_run_all_script_name
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

get_run_all_script_name <- function(report_id = NULL) {

    script_name <- PKG_Constants$run_all_base_name

    # Add report id if provided
    if (!is.blank(report_id))
        script_name <- paste0( script_name, "-", report_id)

    # Add the extension
    script_name <- paste0(script_name, ".R")

    return(script_name)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Get the lockfile path
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param folder
#'        [string]: A folder where the lockfile will reside.
#'
#' @param version
#'        [string]: The version of oncvis the lockfile is associated with for the scripts.
#'
#' @param report_id
#'        [string]: A report ID that gets added to the end of the run script name.
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ string ]
#'
#'        Returns a string containing the lock file path.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- get_lockfile_path(
#'        folder  = "/test",
#'        version = "01.01.01"
#'    )
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   get_lockfile_path
#' @title  get_lockfile_path
#' @rdname get_lockfile_path
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

get_lockfile_path <- function(folder = NULL, version = NULL, report_id = NULL) {

    if (!is.blank(folder)) {

        folder <- paste0(folder, "/", "renv")

        if (!is.blank(version)) {

            # Replace dots with dashes to meet file naming standards
            version <- gsub(x = version, pattern = ".", replacement = "-", fixed = TRUE)

            folder <- paste0(folder, "-v", version)
        }

        # Add report id if provided
        if (!is.blank(report_id))
            folder <- paste0( folder, "-", report_id)

        folder <- paste0(folder, ".lock")
    }

    return(folder)
}
