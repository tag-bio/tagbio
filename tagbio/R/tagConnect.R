
# default localhost url
LOCALHOST_URL <- "http://localhost:8000"
LOCALHOST <- "localhost"
LOCALHOST_IP <- "128.0.0.1"

# environment variables
TAGBIO_HOST_URL <- "TAGBIO_HOST_URL"
TAGBIO_API_KEY <- "TAGBIO_API_KEY"

# kung services (for data product discovery)
KUNG_CAPACITORS <- "/kung-services/db/capacitors"

# home dir and config file
HOME_ENV <- "HOME"
CONFIG_FILE <- ".tagbio.json"

#' An S3 class representing a connection to a tag.bio server
#'
#' The tagConnect class establishes a connection to a local or remote
#' tag.bio server.
#'
#' @section Server Address:
#' The tag.bio server URL can be specified in multiple ways.  Methods are listed here
#' in priority order and the URL is obtained from the first successful method.
#' \itemize{
#' \item If \code{host_url} is passed as a parameter, this URL is used and overrides
#' all other methods.  This method is not considered best practice as hard-coded URLs
#' will make software less portable.
#' \item If \code{TAGBIO_HOST_URL} exists as a system environment variable, then the URL is
#' obtained from this variable.
#' \item If a \code{.tagbio.json} file exists in the  \code{HOME} directory, then the URL is
#' looked for in this configuration file.  The URL should be called \code{TAGBIO_HOST_URL}
#' in the configuration file.
#' \item If not specified by any of the above methods, \code{tagConnect} assumes that the
#' tag.bio server is running locally.
#' }
#'
#' @section API Key:
#' Connecting to a remote tag.bio server requires the user to have a tag.bio API key.
#' See this site (TODO) on how to obtain an API key.  Once an API key has been obtained
#' it may be specified in multiple ways:
#' \itemize{
#' \item The API key may be specified directly through the \code{api_key} parameter.  Note
#' that this method is not recommended as it is a security risk to maintain API keys
#' in code.
#' \item If \code{TAGBIO_API_KEY} is present as a system variable, this value will be used.
#' \item If a \code{.tagbio.json} file exists in the  \code{HOME} directory, then the API key is
#' looked for in this configuration file.  The URL should be called \code{TAGBIO_API_KEY}
#' in the configuration file.
#' \item If not specified by any other method, it is assumed no API key is required.  This will
#'  always be the case when connection to a local tag.bio server.
#'  }
#'
#' @section Warning:
#' Note that the connection is 'lazy'  meaning that no
#' communication with the tag.bio server is attempted until data is
#' required.  Connection errors will be deferred until that point.
#'
#' @section Methods:
#' \code{summary(tc)}: Shows all data products the user has access to through tagConnect \code{tc}.
#' Summary results are returned as a table with columns for \code{key}, \code{site},
#' \code{description}, \code{displayname} and \code{url}.
#'
#' @param host_url URL to the tag.bio server
#' @param api_key tag.bio api key
#' @param token alternative authentication based on bearer token
#'
#' @examples
#' # connect to local host, no API key required
#' tag_con <- tagConnect()
#'
#' # connect to a remote tag.bio server with
#' # explicit parameters (not recommended)
#' tag_con <- tagConnect(host_url = "", api_key = "")
#' @export
tagConnect <- function(host_url = "", api_key = "", url = "", token = "") {
  tc <- list(host_url = host_url,
             api_key = api_key,
             url = url,
             token = token)
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
    url <- substr(url, 1, nurl-1)
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
  return(config_data)
}

#' Create a table from a tag.bio data product
#'
#' Connects to a data product via a tagConnect connection.
#'
#' @examples
#' # connect to local host, no API key required
#' tag_con <- tagConnect()
#'
#' # pulls a data table from the data product
#' fc <- tbl(tag_con) %>% collect()
#'
#'
#' @inheritParams dplyr::tbl
#' @export
#' @importFrom dplyr tbl
tbl.tagConnect <- function(src = "tagConnect", fc = "") {
  return(tagFC(fc = fc, con = src))
}

# API authentication notes
# There are three different modes for authenticating to the API:
# 1. For an API call connected via localhost, no authentication is
#    required.  Localhost is the assumed endpoint if:
#    - no URL is provided
#    - URL contains "localhost" or "127.0.0.1"
# 2. When an API key is provided, connection is made using basic
#    authentication with user name and password extracted from the
#    API key.
# 3. If not (1) and (2) and a token is provided, use token 
#    authentication.

#' @export
api_auth_header <- function(object, ...) {
  UseMethod("api_auth_header", object)
}

#' @export
api_auth_header.tagConnect <- function(object, url) {
  # decide which authentication mode to use
  if (grepl(LOCALHOST, url, ignore.case=T) | grepl(LOCALHOST_IP, url)) {
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
    return(httr::add_headers(Authorization = object$token))
  }

  # TODO - error?
  return("")
}

#' @export
api_get <- function(object, ...) {
  UseMethod("api_get", object)
}

#' @export
api_get.tagConnect <- function(object, url) {

  api_head <- api_auth_header(object, url)

  r <- tryCatch({httr::GET(url, api_head, encode = "json")},
                 error=function(cond) {
                   print(paste0("Error.  Was not able to connect to: ", url, ".  Please check URL."))
                   return()
                })
    
  if (is.null(r)) {
    return()
  }

  # check status!
  call_status <- r$status_code
  if (call_status != 200) {
    if (call_status == 401) {
      print("Authentication failed.  Please check API key.")
      return()
    }
    if (call_status == 500) {
      print("Server error.")
      return()
    }
    print("Error connecting to tag.bio API.")
    print(call_status)
    return()
  }
  return(r)
}

#' @export
api_post <- function(object, ...) {
  UseMethod("api_post", object)
}

# TODO: Document - all API posts should go through here.
#' @export
api_post.tagConnect <- function(object, query_type, url,
                                return_type = "json", jsonPayload = NA) {

  # set up url
  url <- paste0(url, "/", query_type)

  # determine authen
  api_head <- api_auth_header(object, url)

  if (query_type == "s") {
    r <- tryCatch({httr::POST(url, api_head, encode = "json")},
                   error=function(cond) {
                     print(paste0("Error.  Was not able to connect to: ",url,".  Please check URL."))
                     return()
                  })
  } else {
    r <- tryCatch({httr::POST(url,
                              body = jsonPayload,
                              api_head,
                              encode = "json")},
                  error=function(cond) {
                    print(paste0("Error.  Was not able to connect to: ",url,".  Please check URL."))
                    return()
                  })
  } 

  if (is.null(r)) {
    return()
  }

  # check status!
  call_status <- r$status_code
  if (call_status != 200) {
    if (call_status == 401) {
      print("Authentication failed.  Please check API key.")
      return()
    }
    if (call_status == 500) {
      print("Server error.")
      return()
    }
    print("Error connecting to tag.bio API.")
    print(call_status)
    return()
  }
  
  if (return_type == "json") {
    return(httr::content(r))
  } else {
    # wrote a parser here as content was giving floats as strings
    res <- paste0(httr::content(r, as = "text", type = "text/csv", encoding = "UTF-8"))
    res_table <- read.table(text = res, header = T, sep = ",", check.names = F)
    return(tibble(res_table))
  }
}

#' Create a table from a tag.bio data product
#'
#' Connects to a data product via a tagConnect connection.
#'
#' @examples
#' # connect to local host, no API key required
#' tag_con <- tagConnect()
#'
#' # pulls a data table from the data product
#' fc <- tbl(tag_con) %>% collect()
#'
#'
#' @inheritParams base::summary
#' @export
summary.tagConnect <- function(object, ...) {

  # returns FC data as a tibble
  kung_url <- paste0(object$url, KUNG_CAPACITORS)

  # make api call
  r <- api_get(object, kung_url)

  fcs_json <- null_to_na_recurse(httr::content(r))
  fcs_tbl <- fcs_json %>% map_df(flatten_df)

  if (!("key" %in% colnames(fcs_tbl))) {
    if ((grepl(LOCALHOST, object$url)) | (grepl(LOCALHOST_IP,object$url))) {
      return(
        tibble::tibble(key = c(LOCALHOST), site = c(LOCALHOST), description = c(LOCALHOST),
               displayname = c(LOCALHOST), url = c(object$url))
      )
    } else {
      print("Unable to get FC data")
      # default to localhost?
      return(NULL)
    }
  } else {

    # remove extraneous rows and columns
    fcs_tbl <- fcs_tbl %>%
      filter(site != "NULL") %>%
      select(key, site, description, displayname, url)

    return(fcs_tbl)
  }
}

#' @export
tagListFCs <- function (.data) {
  UseMethod("tagListFCs", .data)
}

tagListFCs.tagConnect <- function(.data) {
  fcs_tbl <- summary(.data)

  if (is.null(fcs_tbl)) {
    return(c())
  } else {
    return(unlist(fcs_tbl %>% pull(key)))
  }
}



