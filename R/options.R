# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
ERATOS_OPTIONS <- settings::options_manager(
    eratos = list(
        workspace_url = "https://api.eratos.com/api/workspace/v1",
        key = Sys.getenv("ERATOS_KEY", ""),
        secret = Sys.getenv("ERATOS_SECRET", "")
    ),
    senaps = list(
        sensor_url = "https://senaps.eratos.com/api/sensor/v2/",
        datasource_url = "https://senaps.eratos.com/api/datasource/v1/",
        tmd_url = "https://senaps.io/thredds/ncss/",
        username = Sys.getenv("SENAPS_USER", ""),
        password = Sys.getenv("SENAPS_PASS", ""),
        apikey = Sys.getenv("SENAPS_APIKEY", ""),
        token = Sys.getenv("SENAPS_TOKEN", "")
    )
)


#' Set or get options for reratos
#'
#' This function allows users to get or set configuration options for the
#' Eratos and Senaps APIs.
#'
#' @param ... Option names to retrieve or key-value pairs to set.
#'
#' @details
#' The options are managed via a nested structure that distinguishes between
#' the Eratos and Senaps APIs.
#'
#' @section Supported options:
#' \itemize{
#'   \item{\code{eratos.root}}{ Base URL for the Eratos Workspace API. }
#'   \item{\code{eratos.key}}{ Eratos API key (client ID). }
#'   \item{\code{eratos.secret}}{ Eratos API secret (client secret). }
#'   \item{\code{senaps.sensor_url}}{ Base URL for Senaps Sensor API. }
#'   \item{\code{senaps.datasource_url}}{ Base URL for Senaps Datasource API. }
#'   \item{\code{senaps.tmd_url}}{ Base URL for Senaps TMD API. }
#'   \item{\code{senaps.username}}{ Username for Senaps. }
#'   \item{\code{senaps.password}}{ Password for Senaps. }
#'   \item{\code{senaps.apikey}}{ API key for Senaps. }
#'   \item{\code{senaps.token}}{ Bearer token for Senaps (if available). }
#'   \item{\code{default_api}}{ Default API to use ("eratos" or "senaps"). }
#' }
#'
#' @return If called with no arguments, returns all current options.
#' If called with named arguments, updates and returns the modified options.
#'
#' @examples
#' # Get all options
#' eratos_options()
#'
#' # Set Eratos credentials
#' eratos_options(eratos.key = "mykey", eratos.secret = "mysecret")
#'
#' # Set Senaps API key
#' eratos_options(senaps.apikey = "xyz123")
#'
#' @export
eratos_options <- function(...) {
    settings::stop_if_reserved(...)
    args <- list(...)
    # Return all options if no args given
    if (length(args) == 0) {
        return(ERATOS_OPTIONS())
    }

    # Validate named arguments
    if (is.null(names(args)) || any(nchar(names(args)) == 0)) {
        stop("All arguments must be named.", call. = FALSE)
    }

    # Flatten keys (e.g., eratos.key)
    flat_keys <- names(args)
    for (key in flat_keys) {
        value <- args[[key]]

        # Support nested fields like eratos.key or senaps.apikey
        if (grepl("\\.", key)) {
            parts <- strsplit(key, "\\.")[[1]]
            if (length(parts) != 2) {
                stop(sprintf("Invalid option name: '%s'", key), call. = FALSE)
            }

            group <- parts[1]
            subkey <- parts[2]

            if (!group %in% c("eratos", "senaps")) {
                stop(sprintf("Unknown group: '%s' (must be 'eratos' or 'senaps')", group), call. = FALSE)
            }

            # Update the nested structure
            current <- ERATOS_OPTIONS()[[group]]
            current[[subkey]] <- value
            do.call(ERATOS_OPTIONS, setNames(list(current), group))
        } else {
            # Handle top-level fields (e.g., default_api)
            ERATOS_OPTIONS(setNames(list(value), key))
        }
    }

    invisible(ERATOS_OPTIONS())
}


#' Reset global options for reratos
#'
#' @export
eratos_reset <- function() {
    settings::reset(ERATOS_OPTIONS)
}
