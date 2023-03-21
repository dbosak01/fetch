#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Reads in a SAS data set from a specified location
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param filename
#'        [string]: Location to a SAS data set.
#'
#' @param ssh
#'        [ssh class]: shinyauth SSH object.
#'
#' @param use_cache
#'        [logical]: Use caching when retrieving the data.
#'
#' @param env
#'        [environment]: Environment in which to get the cache and assign the memoise functions.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ dataframe ]
#'
#'    Returns a dataframe containing the data read in from a sas data set.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- get_sas_data(
#'        filename  = "/userdata/stat/amg999/onc/20190101/analysis/adam/adsl.sas7bdat"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   get_sas_data
#' @title  get_sas_data
#' @rdname get_sas_data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

get_sas_data <- function(filename = NULL, ssh, use_cache = FALSE, env = NULL) {

    if (!is.null(filename)) {

        df = tryCatch(
        {
            if (is.ssh(ssh)) {

                # Ensure the ssh connection is logged in
                verify_ssh_login(ssh)

                if (use_cache) {

                    # Setup the cache for reading in sas7bdat files from the ssh object if it does not already exist
                    if (is.null(env$read_ssh_sas_file) || !memoise::is.memoised(env$read_ssh_sas_file))
                        env$read_ssh_sas_file <- memoise::memoise(ssh$read_sas_file,
                                                                  cache = env$.cache_m
                                                                 )

                    result <- env$read_ssh_sas_file(filename)
                } else {
                    result <- haven::read_sas(filename)
                }
            } else {

                if (use_cache) {

                    # Setup the cache for reading in sas7bdat files from haven if it does not already exist
                    if (is.null(env$read_sas_file) || !memoise::is.memoised(env$read_sas_file))
                        env$read_sas_file <- memoise::memoise(haven::read_sas,
                                                              cache = env$.cache_m
                                                             )

                    result <- env$read_sas_file(filename)
                } else {
                    result <- haven::read_sas(filename)
                }
            }
        },

        warning = function(war)
        {
            warning(stringr::str_c("WARNING occurred while retrieving the SAS data from: ", filename, " ", war$message))
        },

        error = function(err)
        {
            stop(stringr::str_c("ERROR occurred while retrieving the SAS data from: ", filename, " ", err$message))
        })

        # Apply some data cleaning to the data set returned
        if (is.data.frame(df)) {

            # Lowercase all the column names for consistency
            colnames(df) = stringr::str_to_lower(colnames(df))
        }
    } else {
        df <- NULL
    }

    return(df)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Reads in a parquet file from a specified location
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param filename
#'        [string]: Location of the parquet file.
#'
#' @param ssh
#'        [ssh class]: shinyauth SSH object.
#'
#' @param use_cache
#'        [logical]: Use caching when retrieving the data.
#'
#' @param env
#'        [environment]: Environment in which to get the cache and assign the memoise functions.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ dataframe ]
#'
#'    Returnes a dataframe containing the data read in from a sas data set.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- get_parquet_data(
#'        filename  = "/userdata/stat/amg999/onc/20190101/analysis/adam/adsl.parquet"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   get_parquet_data
#' @title  get_parquet_data
#' @rdname get_parquet_data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

get_parquet_data <- function(filename = NULL, ssh, use_cache = FALSE, env = NULL) {

    if (!is.null(filename)) {

        df = tryCatch(
        {
            if (is.ssh(ssh)) {

                # Ensure the ssh connection is logged in
                verify_ssh_login(ssh)

                if (use_cache) {

                    # Setup the cache for reading in sas7bdat files from the ssh object if it does not already exist
                    if (is.null(env$read_ssh_parquet_file) || !memoise::is.memoised(env$read_ssh_parquet_file))
                        env$read_ssh_parquet_file <- memoise::memoise(ssh$read_parquet_file,
                                                                      cache = env$.cache_m
                                                                     )

                    result <- env$read_ssh_parquet_file(filename)
                } else {
                    result <- ssh$read_parquet_file(filename)
                }
            } else {

                if (use_cache) {

                    # Setup the cache for reading in sas7bdat files from haven if it does not already exist
                    if (is.null(env$read_parquet_file) || !memoise::is.memoised(env$read_parquet_file))
                        env$read_parquet_file <- memoise::memoise(arrow::read_parquet,
                                                                  cache = env$.cache_m
                                                                 )

                    result <- env$read_parquet_file(filename)
                } else {
                    result <- arrow::read_parquet(filename)
                }
            }
        },

        warning = function(war)
        {
            warning(stringr::str_c("WARNING occurred while retrieving the Parquet data from: ", filename, " ", war$message))
        },

        error = function(err)
        {
            stop(stringr::str_c("ERROR occurred while retrieving the Parquet data from: ", filename, " ", err$message))
        })

        # Apply some data cleaning to the data set returned
        if (is.data.frame(df)) {

            # Uppercase all the colnames for consistency
            colnames(df) = stringr::str_to_lower(colnames(df))
        }
    } else {
        df <- NULL
    }

    return(df)
}
