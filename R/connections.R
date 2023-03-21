#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Get the table names stored in a data source location
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param data_source
#'        [string]: Location of source data.
#'
#' @param type
#'        [string]: Type of data.
#'
#' @param logger
#'        [R6 Class]: A message recorder object.
#'
#' @param ssh
#'        [string]: Optional, A shinyauth SSH object.
#'
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [ list ]
#'
#'    List of table names associated with the data source and type of data.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- get_table_names(
#'        data_source  = "/userdata/stat/amg999/onc/20190101/analysis/adam",
#'        type         = "sas7bdat"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   get_table_names
#' @title  get_table_names
#' @rdname get_table_names
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

get_table_names <- function(data_source = NULL, type = NULL, logger = NULL, ssh = NULL) {

    tables       <- NULL
    file_pattern <- switch(type,
                               sas7bdat = ifelse(is.ssh(ssh), dc_ssh_filter$sas7bdat, dc_filter$sas7bdat),
                               parquet  = ifelse(is.ssh(ssh), dc_ssh_filter$Parquet, dc_filter$Parquet),
                               NULL
                          )

    tryCatch(
    {
        if (is.ssh(ssh)) {

            # Verify the ssh connection is logged in
            verify_ssh_login(ssh, logger)

            data <- ssh$list_files(data_source, filter = file_pattern)

            if (nrow(data) > 0) {
                tables <- data$Path
            }

        } else {
            if (dir.exists(data_source)) {
                tables <- list.files(path = data_source, pattern = file_pattern, full.names = TRUE, ignore.case = TRUE)
            } else {
                stop("The data_source does not exist!")
            }
        }
    },

    warning = function(war)
    {
        logger$addWarning(stringr::str_c("Unable to get the table names from ", data_source, "\n",  war$message))
    },

    error = function(err)
    {
        logger$addError(stringr::str_c("Unable to get the table names from ", data_source, "\n",  err$message))
    })

    return(tables)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Assigns a table connection function to a table name.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param table_name
#'        [string]: Name of the table.
#'
#' @param type
#'        [string]: Type of table. Valid values are "sas7bdat", "parquet".
#'                  Default is sas7bdat.
#'
#' @param env
#'        [environment]: Environment in which to create the table accessor functions, such
#'                       as the global environment or a package namespace.
#'                       Default is parent.frame()
#'
#' @param table_formatter
#'        [function]: Optionally, a function to clean the table name
#'                    before turning it into a function name, such as removing prefixes.
#'                    Default is \code{\link[snakecase]{to_snake_case}}.
#'
#' @param table_post
#'        [function]: Optionally, post-processing to perform on each table before
#'                    returning it.
#' @param logger
#'       [R6 Class]: A message recorder object.
#'
#' @param ssh
#'        [logical]: Optional, A shinyauth SSH object.
#'
#' @param use_cache
#'        [logical]: Use caching when retrieving the data.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [function]
#'
#'    An accessor function for a table.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- assign_table_function(
#'        table_name       = NULL,
#'        type             = 'sas7bdat',
#'        env              = parent.frame()
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   assign_table_function
#' @title  assign_table_function
#' @rdname assign_table_function
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

assign_table_function <- function(table_name,
                                  type,
                                  env = parent.frame(),
                                  table_formatter = snakecase::to_snake_case,
                                  table_post,
                                  logger,
                                  ssh,
                                  use_cache
                                 )
{

    tryCatch(
    {
        # Get the function associated with the type of connection
        fun <- switch(type,
                         sas7bdat = function() table_post(get_sas_data(table_name, ssh, use_cache, env)),
                         parquet  = function() table_post(get_parquet_data(table_name, ssh, use_cache, env)),
                         NULL
                        )

        # Assign the table name as an attribute to the function
        attr(fun, 'source') <- table_name

        # Strip off the file type extension
        name <- sub('\\..*$', '', basename(table_name))

        # Format the table name
        function_name <- table_formatter(name)

        # Inserts the table function into the environment
        assign(function_name, fun, pos = env)
    },

    warning = function(war)
    {
        logger$addWarning(stringr::str_c("Unable to assign table function: ", table_name, "\n",  war$message))
    },

    error = function(err)
    {
        logger$addError(stringr::str_c("Unable to assign table functions: ",  table_name, "\n", err$message))

    })
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Build the connections for accessing each of the data source names depending of the type of
#'    data being accessed.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param data_source
#'        [string]: Location of source data.
#'
#' @param type
#'        [string]: Type of table. Valid values are "sas7bdat", "parquet".
#'                  Default is sas7bdat.
#'
#' @param ssh
#'        [shinyauth::SSH]: Optional, A shinyauth SSH object.
#'
#' @param use_cache
#'        [logical]: Use caching when retrieving the data.
#'
#' @param env
#'        [environment]: Environment in which to create the table accessor functions, such
#'                       as the global environment or a package namespace.
#'                       Default is parent.frame()
#' @param logger
#'        [R6 Class]: A message recorder object.
#'
#' @param table_formatter
#'        [function]: Optionally, a function to clean the table name
#'                    before turning it into a function name, such as removing prefixes.
#'                    By default, \code{\link[snakecase]{to_snake_case}}.
#'
#' @param ...
#'        [string]: Additional arguments.
#'
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [list]
#'
#'    A list of table accessor functions.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- build_ds_connections(
#'        data_source      = "/userdata/stat/amg999/onc/20190101/analysis/adam",
#'        type             = "sas7bdat"
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   ds_init
#' @title  ds_init
#' @rdname ds_init
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

build_ds_connections <- function(data_source, type, ssh, use_cache, env, logger, table_formatter = snakecase::to_snake_case, ...) {

    # Retrieve the data source names from the specified location
    tables <- get_table_names(data_source, type, logger, ssh)

    # Add additional functions if needed

    # Create a function for each of the data source names based on the type of data requested
    invisible(purrr::map(tables,
                         assign_table_function,
                         type,
                         env,
                         table_formatter,
                         table_post = identity,
                         logger,
                         ssh,
                         use_cache
                        )
              )
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @description
#'    Connects to a data source for accessing data.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
# Function Arguments:
#'
#' @param data_source
#'        [string]: Location of source data.
#'
#' @param type
#'        [string]: Type of table. Valid values are "sas7bdat", "parquet".
#'                  Default is sas7bdat.
#'
#' @param ssh
#'        [shinyauth::SSH]: Optional, A valid shinyauth SSH object for accessing a connection path
#'        using ssh.
#'
#' @param rebuild_cache
#'        [logical]: Force a rebuild of the cache.
#'
#' @param use_cache
#'        [logical]: Use caching when retrieving the data.
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @section Returns:
#' \preformatted{
#'
#'    Type: [list]
#'
#'    A list of data source table accessor functions.
#'
#' }
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
#' @examples
#'
#' \dontrun{
#'
#'
#'    result <- connect_to_data(
#'        data_source  = "/userdata/stat/amg999/onc/20190101/analysis/adam",
#'        type         = "sas7bdat",
#'        ssh          = shinyauth::SSH$new(user_name = <username>, user_pass = <password>))
#'    )
#'
#'}
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @export
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++|
#' @name   connect_to_data
#' @title  connect_to_data
#' @rdname connect_to_data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

connect_to_data <- function(data_source = NULL, type = dctype$sas7bdat, ssh = NULL, rebuild_cache = FALSE, use_cache = FALSE) {

    .env <- new.env()
    ds   <- list()

    logger <- MessageRecorder$new()
    logger$addInfo("Initializing the data source connection.")
    logger$addInfo(paste0("Data Source: ", data_source))

    if (!is.null(data_source)) {

        # Ensure the data_source path is in unix form
        data_source <- convert_path(data_source)

        # Instantiate the cache if requested
        if (use_cache) {
            logger$addInfo("Using in memory caching for accessing data.")
            .env$.cache_m <- cachem::cache_mem(max_age = PKG_Constants$cache$max_age, max_size = PKG_Constants$cache$max_size)
        }

        # Build all the data source table connections by associating them with a function to retrieve there data
        build_ds_connections(data_source, type, ssh, use_cache, env = .env, logger)

        ds <- tryCatch(
        {
            # Grab all the function names from the environment
            ds_funs_names <- ls(.env)

            # Get all the functions into a list
            ds_funs <- purrr::map(ds_funs_names, ~
            {
                f <- .env[[.x]]
                return(f)
            })

            # Associate the names with their corresponding functions
            names(ds_funs) <- ds_funs_names

            # Return all the non-missing functions
            ds_funs[purrr::map_lgl(ds_funs, ~ !is.null(.x))]
        },

        warning = function(war) {
            logger$addWarning(stringr::str_c("Warning occurred retrieving the table functions. \n",  war$message))
            return(list())
        },

        error = function(err) {
            logger$addError(stringr::str_c("Error occurred retrieving the table functions. \n", err$message))
            return(list())
        })
    }

    # cleanup the temp environment but keep the .cache
    rm(list = ls(envir = .env), envir = .env)

    # Check for any errors that occurred when the data connection was added.
    if (logger$hasErrorMessages()) {
        message("An error occurred while attempting to create the data connection.\n", "See log for additional information.\n")
    }

    # Set the class name for the object
    class(ds) <- c("data_connection", "list")

    # Assign the data_source location as an attribute on the object
    attr(ds, 'data_source') <- data_source

    # Assign the type of data as an attribute on the object
    attr(ds, 'data_type') <- type

    # Assign the memory cache as an attribute on the data connection object if requested
    if (use_cache)
        attr(ds, 'cache_env') <- .env

    # Assign the logger for tracking log message
    ds$logger <- logger

    return(ds)
}
