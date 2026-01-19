# default localhost url
LOCALHOST_URL <- "http://localhost:8000"
LOCALHOST <- "localhost"
LOCALHOST_IP <- "127.0.0.1"

# environment variables
TAGBIO_HOST_URL <- "TAGBIO_HOST_URL"
TAGBIO_API_KEY <- "TAGBIO_API_KEY"

# kung services (for data product discovery)
KUNG_CAPACITORS <- "/kung-services/db/capacitors"

# home dir and config file
HOME_ENV <- "HOME"
CONFIG_FILE <- ".tagbio.json"

print_error <- function(message) {
  if (rlang::is_list(message)) {
    write(rjson::toJSON(message, auto_unbox = TRUE), file = stderr())
  } else {
    write(message, file = stderr())
  }
}

# From https://github.com/jeroen/jsonlite/issues/70
# - drops NULLS from JSON results
null_to_na_recurse <- function(obj) {
  if (is.list(obj)) {
    obj <- jsonlite:::null_to_na(obj)
    obj <- lapply(obj, null_to_na_recurse)
  }

  obj
}

#' An S3 class representing a connection to a Tag.bio server
#'
#' The tagConnect class establishes a connection to a local or remote
#' Tag.bio server.
#'
#' @section Server Address:
#' The Tag.bio server URL can be specified in multiple ways.
#'
#' Methods are listed here in priority order and the URL is obtained from
#' the first successful method.
#' \itemize{
#' \item If \code{host_url} is passed as a parameter, this URL is used and
#' overrides all other methods.  This method is not considered best practice
#' as hard-coded URLs will make software less portable.
#' \item If \code{TAGBIO_HOST_URL} exists as a system environment variable,
#' then the URL is obtained from this variable.
#' \item If a \code{.tagbio.json} file exists in the  \code{HOME} directory,
#' then the URL is looked for in this configuration file.  The URL should be
#' called \code{TAGBIO_HOST_URL} in the configuration file.
#' \item If not specified by any of the above methods, \code{tagConnect}
#' assumes that the Tag.bio server is running locally.
#' }
#'
#' @section API Key:
#' Connecting to a remote Tag.bio server requires the user to have a
#' Tag.bio API key. See this site (TODO) on how to obtain an API key.
#' Once an API key has been obtained, it may be specified in multiple ways:
#' \itemize{
#' \item The API key may be specified directly through the \code{api_key}
#' parameter.  Note that this method is not recommended as it is a security risk
#' to maintain API keys in code.
#' \item If \code{TAGBIO_API_KEY} is present as a system variable, this value
#' will be used.
#' \item If a \code{.tagbio.json} file exists in the \code{HOME} directory, then
#' the API key is looked for in this configuration file. The URL should be
#' called \code{TAGBIO_API_KEY} in the configuration file.
#' \item If not specified by any other method, it is assumed no API key
#' is required. This will always be the case when connection to a local
#' Tag.bio server.
#' }
#'
#' @section Warning:
#' Note that the connection is 'lazy'  meaning that no
#' communication with the Tag.bio server is attempted until data is
#' required.  Connection errors will be deferred until that point.
#'
#' @section Methods:
#' \code{summary(tc)}: Shows all data products the user has access to
#' through tagConnect \code{tc}.
#'
#' Summary results are returned as a table with columns for \code{key},
#' \code{site}, \code{description}, \code{displayname} and \code{url}.
#'
#' @param host_url The URL to the tag.bio server
#' @param url An alternative host_url parameter
#' @param api_key A Tag.bio API key
#' @param token An alternative authentication based on bearer token
#'
#' @examples
#' \dontrun{
#' #connect to local host, no API key required
#' tag_con <- tagConnect()
#'
#' #connect to a remote tag.bio server with
#' #  explicit parameters (not recommended)
#' tag_con <- tagConnect(host_url = "", api_key = "")
#' }
#'
#' @export
tagConnect <- function(host_url = "", api_key = "", url = "", token = "") {
  tc <- list(
    host_url = host_url,
    api_key = api_key,
    url = url,
    token = token
  )
  class(tc) <- "tagConnect"

  # get configuration from sys variables or file
  config_data <- tag_load_config()

  if (url == "") {
    url <- host_url
  }

  # look other places for url/api key
  if (url == "") {
    if (Sys.getenv(TAGBIO_HOST_URL) != "") {
      url <- Sys.getenv(TAGBIO_HOST_URL)
    } else {
      if (TAGBIO_HOST_URL %in% names(config_data)) {
        url <- config_data[[TAGBIO_HOST_URL]]
      } else {
        url <- LOCALHOST_URL
      }
    }
  }

  # drop trailing slash if it exists
  nurl <- nchar(url)
  if (substr(url, nurl, nurl) == "/") {
    url <- substr(url, 1, nurl - 1)
  }

  tc$url <- url

  if (api_key == "") {
    if (Sys.getenv(TAGBIO_API_KEY) != "") {
      api_key <- Sys.getenv(TAGBIO_API_KEY)
    } else {
      if (TAGBIO_API_KEY %in% names(config_data)) {
        api_key <- config_data[[TAGBIO_API_KEY]]
      }
    }
  }
  tc$api_key <- api_key
  tc$token <- token

  tc
}

# Internal method for gathering config data
tag_load_config <- function() {
  home <- Sys.getenv(HOME_ENV)
  config_file <- file.path(home, CONFIG_FILE)

  if (file.exists(config_file)) {
    config_data <- rjson::fromJSON(file = config_file)
  } else {
    config_data <- list()
  }

  config_data
}

#' Create a table from a Tag.bio data product
#'
#' Connects to a data product via a tagConnect connection.
#'
#' @examples
#' \dontrun{
#' #connect to local host, no API key required
#' tag_con <- tagConnect()
#'
#' #pulls a data table from the data product
#' fc <- tbl(tag_con) |> collect()
#' }
#'
#' @importFrom dplyr tbl
#' @inheritParams dplyr::tbl
#' @param src tagConnect object
#' @param fc data product name
#' @export
tbl.tagConnect <- function(src = "tagConnect", fc = "") {
  return(tagFC(fc = fc, con = src))
}

#' An S3 class representing the API authentication for a Tag.bio server
#'
#' The api_auth_header class establishes models the authentication token
#' for a local or remote Tag.bio server.
#'
#' @param object A tagConnect object
#' @param ... Reserved for future extensions
#'
#' @section API Authentication Notes:
#' There are three different modes for authenticating to the API:
#' \itemize{
#' \item 1. For an API call connected via localhost, no authentication is
#'    required.  Localhost is the assumed endpoint if:
#'    - no URL is provided
#'    - URL contains "localhost" or "127.0.0.1"
#' \item 2. When an API key is provided, connection is made using basic
#'    authentication with user name and password extracted from the
#'    API key.
#' \item 3. If neither, but a token is provided, use token
#'    authentication.
#' }
#' @export
api_auth_header <- function(object, ...) {
  UseMethod("api_auth_header", object)
}


#' An S3 class representing a connection to a Tag.bio server
#'
#' The tagConnect class establishes a connection to a local or remote
#' Tag.bio server.
#'
#' @param api_auth_header An authenticaion header object from a
#'   Tag.bio server connection
#' @param url A URL to connect to
#'
#' @export
api_auth_header.tagConnect <- function(object, url) {
  # decide which authentication mode to use
  if (grepl(LOCALHOST, url, ignore.case = TRUE) || grepl(LOCALHOST_IP, url)) {
    # local mode - no auth
    return(httr::authenticate("", "", type = "basic")) # does this work?
  }

  # try use api key
  api_data <- unlist(strsplit(object$api_key, ":"))

  if (length(api_data) == 2) {
    return(httr::authenticate(api_data[1], api_data[2], type = "basic"))
  }

  # try token
  if (object$token != "") {
    return(httr::add_headers(Authorization = paste("Bearer", object$token)))
  }

  # TODO - error?
  list()
}

#' An S3 class representing a Tag.bio API data product getter
#'
#' The tagConnect class establishes a connection to a local or remote
#' Tag.bio server.
#'
#' Connects to a data product via a tagConnect connection.
#'
#' @param tagConnect A tagConnect object
#' @param ... Reserved for future extensions
#'
#' @examples
#' \dontrun{
#' # Connected to Tag.bio data host using API key
#' }
#'
#' @export
api_get <- function(object, ...) {
  UseMethod("api_get", object)
}

#' Tag.bio API data product getter
#'
#' Connects to a data product via a tagConnect connection.
#'
#' @param api_auth_header An authenticaion header object from a
#'   Tag.bio server connection
#' @param url URL to connect to
#'
#' @examples
#' \dontrun{
#' # Connected to Tag.bio data host using API key
#' }
#'
#' @export
api_get.tagConnect <- function(object, url) {
  api_head <- api_auth_header(object, url)
  response <- tryCatch(
    {
      httr::GET(url, api_head, encode = "json")
    },
    error = function(cond) {
      print_error(paste0(
        "Error.  Was not able to connect to: ",
        url,
        ".  Please check URL."
      ))
      print_error(paste0(" Auth head: ", api_head))
    }
  )

  if (is.null(response)) {
    return()
  }

  # check status!
  call_status <- response$status_code
  if (call_status != 200) {
    if (call_status == 401) {
      print_error("Authentication failed.  Please check API key.")
      status_message <- httr::content(response)
      print_error(status_message$message)
      return()
    }
    if (call_status == 500) {
      print_error("Server error.")
      status_message <- httr::content(response)
      print_error(status_message$message)
      return()
    }
    print_error("Error connecting to tag.bio API!")
    status_message <- httr::content(response)
    print_error(status_message$message)
    return()
  }

  response
}

#' An S3 class representing a Tag.bio API data product post
#'
#' The tagConnect class establishes a connection to a local or remote
#' Tag.bio server.
#'
#' Connects to a data product via a tagConnect connection.
#'
#' @param tagConnect object
#' @param ... future extensions
#'
#' @examples
#' \dontrun{
#' # Connected to Tag.bio data host using API key
#' }
#'
#' @export
api_post <- function(object, ...) {
  UseMethod("api_post", object)
}

#' Tag.bio API data product post
#'
#' Connects to a data product via a tagConnect connection.
#'
#' @param api_auth_header An authenticaion header object from a
#'   Tag.bio server connection
#' @param query_type The type of query to execute on the Tag.bio server
#' @param url The URL of the Tag.bio server to connect to
#' @param return_type The results return type (ex. "json" or "table")
#' @param jsonPayload The JSON payload for POST
#'
#' @examples
#' \dontrun{
#' # Connected to Tag.bio data host using API key
#' }
#'
#' @export
api_post.tagConnect <- function(
  object,
  query_type,
  url,
  return_type = "json",
  jsonPayload = NA
) {
  # set up url
  url <- paste0(url, "/", query_type)

  # determine authentication
  api_head <- api_auth_header(object, url)

  if (query_type == "s") {
    response <- tryCatch(
      {
        httr::POST(url, api_head, encode = "json")
      },
      error = function(cond) {
        print_error(paste0(
          "Error.  Was not able to connect to: ",
          url,
          ".  Please check URL."
        ))
      }
    )
  } else {
    response <- tryCatch(
      {
        httr::POST(url, body = jsonPayload, api_head, encode = "json")
      },
      error = function(cond) {
        print_error(paste0(
          "Error.  Was not able to connect to: ",
          url,
          ".  Please check URL."
        ))
        api_head <- list()

        response <- httr::POST(
          url,
          body = jsonPayload,
          api_head,
          encode = "json"
        )

        response
      }
    )
  }

  if (is.null(response)) {
    return()
  }

  # check status!
  call_status <- response$status_code
  if (call_status != 200) {
    if (call_status == 401) {
      print_error("Authentication failed.  Please check API key.")
      print_error("Request:")
      print_error(response$request)
      print_error("Status:")
      status_message <- httr::content(response)
      print_error(status_message$message)
      return()
    }
    if (call_status == 500) {
      print_error("Internal server error.")
      print_error("Request:")
      print_error(response$request)
      print_error("Status:")
      status_message <- httr::content(response)
      print_error(status_message$message)
      return()
    }
    if (call_status == 502) {
      print_error("FC appears to be offline.")
      print_error("Request:")
      print_error(response$request)
      print_error("Status:")
      status_message <- httr::content(response)
      print_error(status_message$message)
      return()
    }
    print_error("Error connecting to tag.bio API")
    print_error("Request:")
    print_error(response$request)
    print_error("Status:")
    status_message <- httr::content(response)
    print_error(status_message$message)
    return()
  }

  if (return_type == "json") {
    return(httr::content(response))
  } else {
    # wrote a parser here as content was giving floats as strings
    res <- paste0(httr::content(
      response,
      as = "text",
      type = "text/csv",
      encoding = "UTF-8"
    ))
    res_table <- utils::read.table(
      text = res,
      header = TRUE,
      sep = ",",
      check.names = FALSE,
      quote = "\"", comment.char = ""
    )
    return(tibble::tibble(res_table))
  }
}

#' Create a table from a Tag.bio data product
#'
#' Connects to a data product via a tagConnect connection.
#'
#' @examples
#' \dontrun{
#' # connect to local host, no API key required
#' tag_con <- tagConnect()
#'
#' # pulls a data table from the data product
#' fc <- tbl(tag_con) |> collect()
#' }
#'
#' @inheritParams base::summary
#' @param ... future extensions
#' @export
summary.tagConnect <- function(object, ...) {
  # returns FC data as a tibble
  kung_url <- paste0(object$url, KUNG_CAPACITORS)

  # make api call
  response <- api_get(object, kung_url)

  fcs_json <- null_to_na_recurse(httr::content(response))
  fcs_tbl <- fcs_json |> purrr::map_df(purrr::flatten_df)

  if (!("key" %in% colnames(fcs_tbl))) {
    if ((grepl(LOCALHOST, object$url)) || (grepl(LOCALHOST_IP, object$url))) {
      tibble::tibble(
        key = c(LOCALHOST),
        site = c(LOCALHOST),
        description = c(LOCALHOST),
        displayname = c(LOCALHOST),
        url = c(object$url)
      )
    } else {
      print_error("Unable to get FC data")

      # default to localhost?
      NULL
    }
  } else {
    # remove extraneous rows and columns
    fcs_tbl <- fcs_tbl |>
      dplyr::filter(object$site != "NULL") |>
      dplyr::select(
        object$key,
        object$site,
        object$description,
        object$displayname,
        object$url
      )

    fcs_tbl
  }
}

#' An S3 class to list all data products on a Tag.bio server
#'
#' The tagConnect class establishes a connection to a local or remote
#' Tag.bio server.
#'
#' @importFrom rlang .data
#' @param .data tagConnect object
#' @export
tagListFCs <- function(.data) {
  UseMethod("tagListFCs", .data)
}

#' List all Tag.bio data products using a tagConnect connection
#'
#' Connects to a data product via a tagConnect connection.
#'
#' @importFrom rlang .data
#' @export
tagListFCs.tagConnect <- function(.data) {
  fcs_tbl <- summary(.data)

  if (is.null(fcs_tbl)) {
    c()
  } else {
    unlist(fcs_tbl |> dplyr::pull(name = "key"))
  }
}
