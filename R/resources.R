#' Get Resources from Eratos
#'
#' Find resources using the Eratos workspace API with various search and filter options.
#'
#' @param query Character string. Free text search query - supports wildcards (*) and simple AND/OR logic.
#' @param limit Integer (1-100). Limit the number of results. Defaults to 20.
#' @param skip Integer (>= 0). Starting index for results. Defaults to 0.
#' @param exclude Character string. Comma separated list of ERNs or IDs to exclude.
#' @param type Character string. Comma separated list of ERNs or IDs of schema types to include.
#' @param excludeType Character string. Comma separated list of ERNs or IDs of schema types to exclude.
#' @param owner Character string. Comma separated list of owner ERNs.
#' @param collections Character string. Comma separated list of collection ERNs.
#' @param facets Character string. Comma separated list of facets.
#' @param sort Character string. Sort order. Defaults to "@date:asc". 
#'   If sorting other than name or @date, type must be specified.
#' @param scope Character string. Scope of the query (e.g. "Public" or "Private").
#' @param property_filters Named list. Advanced property filtering in the form 
#'   list(property_name = list(op = "eq", value = "search_value")). 
#'   Supported operations: "eq" (equal), "ne" (not equal), "gt" (greater than), 
#'   "lt" (less than), "ge" (greater than or equal), "le" (less than or equal).
#'   Note: Property functions require type to also be set.
#'
#' @return A list containing the API response with resource data.
#'
#' @details
#' This function queries the Eratos workspace API to find resources matching the 
#' specified criteria. The API supports both simple text search and advanced 
#' property-based filtering.
#'
#' For advanced querying using property_filters, only properties that exist in 
#' the schema type definition can be queried, and not all properties within a 
#' schema are queryable.
#'
#' @examples
#' \dontrun{
#' # Simple text search
#' resources <- get_resources(query = "temperature*")
#' 
#' # Limit results and skip first 10
#' resources <- get_resources(query = "sensor", limit = 50, skip = 10)
#' 
#' # Filter by type and owner
#' resources <- get_resources(
#'   type = "ern:e-pn.io:resource:eratos.sensor", 
#'   owner = "ern:e-pn.io:resource:eratos.user.john"
#' )
#' 
#' # Advanced property filtering
#' resources <- get_resources(
#'   type = "block",
#'   property_filters = list(
#'     creator = list(op = "eq", value = "ern:e-pn.io:resource:eratos.creator.eratos")
#'   )
#' )
#' 
#' # Sort by date descending
#' resources <- get_resources(sort = "@date:desc")
#' }
#'
#' @export
get_resources <- function(query = NULL,
                        limit = 20,
                        skip = 0,
                        exclude = NULL,
                        type = NULL,
                        excludeType = NULL,
                        owner = NULL,
                        collections = NULL,
                        facets = NULL,
                        sort = "@date:asc",
                        scope = NULL,
                        property_filters = NULL) {
    
    # Validate parameters
    if (!is.null(limit) && (limit < 1 || limit > 100)) {
        stop("limit must be between 1 and 100")
    }
    
    if (!is.null(skip) && skip < 0) {
        stop("skip must be >= 0")
    }
    
    # Build query parameters
    query_params <- list()
    
    if (!is.null(query)) query_params$query <- query
    if (!is.null(limit)) query_params$limit <- limit
    if (!is.null(skip)) query_params$skip <- skip
    if (!is.null(exclude)) query_params$exclude <- exclude
    if (!is.null(type)) query_params$type <- type
    if (!is.null(excludeType)) query_params$excludeType <- excludeType
    if (!is.null(owner)) query_params$owner <- owner
    if (!is.null(collections)) query_params$collections <- collections
    if (!is.null(facets)) query_params$facets <- facets
    if (!is.null(sort)) query_params$sort <- sort
    if (!is.null(scope)) query_params$scope <- scope
    
    # Handle property filters (fn[property,op]=value format)
    if (!is.null(property_filters)) {
        if (!is.list(property_filters)) {
            stop("property_filters must be a named list")
        }
        
        for (prop_name in names(property_filters)) {
            prop_filter <- property_filters[[prop_name]]
            
            if (!is.list(prop_filter) || is.null(prop_filter$op) || is.null(prop_filter$value)) {
                stop("Each property filter must be a list with 'op' and 'value' elements")
            }
            
            # Validate operation
            valid_ops <- c("eq", "ne", "gt", "lt", "ge", "le")
            if (!prop_filter$op %in% valid_ops) {
                stop(paste("Invalid operation:", prop_filter$op, 
                        ". Must be one of:", paste(valid_ops, collapse = ", ")))
            }
            
            # Create fn[property,op] parameter name
            param_name <- paste0("fn[", prop_name, ",", prop_filter$op, "]")
            query_params[[param_name]] <- prop_filter$value
        }
        
        # Warn if type is not specified (required for property filters)
        if (is.null(type)) {
            warning("Property filters require 'type' parameter to be specified for optimal results")
        }
    }
    
    # Make API request
    eratos_request(
        type = "eratos.workspace",
        path = "/resources",
        method = "GET",
        query = query_params
    )
}
