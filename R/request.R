#' Generic API request for Eratos and Senaps
#'
#' @param type One of: "eratos.workspace", "senaps.sensor", "senaps.datasource", "senaps.tmd"
#' @param path API endpoint path (e.g. "/workspaces" or "streams/list")
#' @param method HTTP method, e.g. "GET", "POST", "PUT", "DELETE"
#' @param query List of query parameters
#' @param body Optional JSON body for POST/PUT requests
#' @param ...
#' @return Parsed JSON content or response object
eratos_request <- function(type = c("eratos.workspace", "senaps.sensor", "senaps.datasource", "senaps.tmd"),
                        path = "/",
                        method = "GET",
                        query = NULL,
                        body = NULL,
                        ...) {
    type <- match.arg(type)

    opts <- ERATOS_OPTIONS()

    # Select base URL based on type
    base_url <- switch(type,
        "eratos.workspace" = opts$eratos$workspace_url,
        "senaps.sensor" = opts$senaps$sensor_url,
        "senaps.datasource" = opts$senaps$datasource_url,
        "senaps.tmd" = opts$senaps$tmd_url
    )
    base_url <- sub("/+$", "", base_url)
    path <- gsub("^/+", "", path)
    full_url <- paste0(base_url, "/", path)
    
    # Create request
    req <- httr2::request(full_url) |>
        httr2::req_method(method) |>
        httr2::req_url_query(!!!query)

    # Authentication handling
    if (grepl("^eratos", type)) {
        # Basic Auth using key and secret
        req <- req |> httr2::req_auth_basic(opts$eratos$key, opts$eratos$secret)
    } else if (!is.null(opts$senaps$apikey) && nzchar(opts$senaps$apikey)) {
        req <- req |> httr2::req_headers(apikey = opts$senaps$apikey)
    } else if (!is.null(opts$senaps$token) && nzchar(opts$senaps$token)) {
        req <- req |> httr2::req_auth_bearer_token(opts$senaps$token)
    } else if (nzchar(opts$senaps$username) && nzchar(opts$senaps$password)) {
        req <- req |> httr2::req_auth_basic(opts$senaps$username, opts$senaps$password)
    } else {
        stop("No valid authentication found. Please set ERATOS_KEY/SECRET or SENAPS credentials.")
    }

    # Add body if provided
    if (!is.null(body)) {
        req <- req |> httr2::req_body_json(body)
    }

    # Perform request
    resp <- req |> httr2::req_perform(...)

    # Check for HTTP errors
    httr2::resp_check_status(resp)

    # Try parsing as JSON
    content_type <- httr2::resp_content_type(resp)
    if (grepl("application/json", content_type)) {
        return(httr2::resp_body_json(resp, simplifyVector = TRUE))
    } else {
        return(httr2::resp_body_string(resp))
    }
}



