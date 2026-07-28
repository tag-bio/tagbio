
# default localhost url
LOCALHOST_URL <- "http://localhost:8000"
LOCALHOST <- "localhost"
LOCALHOST_IP <- "127.0.0.1"

# environment variables
TAGBIO_HOST_URL <- "TAGBIO_HOST_URL"
# The deployed notebook sets the host as TAGBIO_BASE_URL, not TAGBIO_HOST_URL; accept either.
TAGBIO_BASE_URL <- "TAGBIO_BASE_URL"
TAGBIO_API_KEY <- "TAGBIO_API_KEY"
# Sentinel set by the plugin runner (connect_tagbio.R). When present, this is a plugin run and the
# SDK must never read env/config for connection/auth (see tagConnect).
TAGBIO_PLUGIN_CONTEXT <- "TAGBIO_PLUGIN_CONTEXT"
# Explicit dev opt-in to read env/config even inside a plugin, for locally testing a remote
# cross-FC call (a test has no user token, so nothing to escalate over). Off by default.
TAGBIO_PLUGIN_ALLOW_CONFIG <- "TAGBIO_PLUGIN_ALLOW_CONFIG"

# kung services (for data product discovery)
KUNG_CAPACITORS <- "/kung-services/db/capacitors"

# home dir and config file
HOME_ENV <- "HOME"
CONFIG_FILE <- ".tagbio.json"

# Resolve a setting with the FILE (~/.tagbio.json) beating ENV, per key: try each candidate key in
# the config file first, then the environment, then the default. `keys` lets the host accept either
# TAGBIO_HOST_URL or TAGBIO_BASE_URL. Mirrors _get_env_setting() in the Python SDK.
resolve_setting <- function(config_data, keys, default = "") {
  for (k in keys) {
    if (k %in% names(config_data) && nzchar(config_data[[k]])) return(config_data[[k]])
  }
  for (k in keys) {
    v <- Sys.getenv(k)
    if (nzchar(v)) return(v)
  }
  default
}

print_error <- function(message) {
  if (is_list(message)) {
    write(jsonlite::toJSON(message,auto_unbox=TRUE), file=stderr())
  } else {
    write(message, file=stderr())
  }
}

# Best-effort extraction of the server's own error message from a response. Never throws itself
# (a failed content() parse must not mask the real error the way the old raw-request leak did).
tag_server_message <- function(r) {
  tryCatch({
    body <- httr::content(r)
    if (is.list(body) && !is.null(body$message)) as.character(body$message)
    else if (is.character(body) && length(body) == 1) body
    else NULL
  }, error = function(e) NULL)
}

# Raise ONE legible error for a failed FC API call, classified by failure mode so the message says
# WHAT happened and WHERE (address + the server's own message), instead of the old print-to-stderr +
# return(NULL) that let callers blow up cryptically downstream. Signals condition class
# "tagbio_api_error" so callers (e.g. a readiness poll) can tryCatch and degrade / retry.
tag_api_stop <- function(url, status = NULL, server_msg = NULL, transport_msg = NULL) {
  headline <-
    if (!is.null(transport_msg)) {
      "Could not reach the tag.bio server. Is it running, and are the host and your network / VPN correct?"
    } else if (isTRUE(status == 401)) {
      "Authentication failed (HTTP 401). Check your API key."
    } else if (isTRUE(status == 503)) {
      "The FC is up but not ready yet (HTTP 503) -- it may still be loading its archive or running startup tests. Retry shortly."
    } else if (isTRUE(status == 502)) {
      "The FC appears to be offline (HTTP 502 from the gateway)."
    } else if (isTRUE(status == 500)) {
      "The FC returned an internal server error (HTTP 500). If it was just (re)started it may still be loading its archive / running startup tests -- retry shortly; if it persists it is a real server error."
    } else if (!is.null(status)) {
      paste0("The tag.bio API returned an unexpected status (HTTP ", status, ").")
    } else {
      "The tag.bio API call failed."
    }

  lines <- c(headline, paste0("  Address: ", url))
  if (!is.null(transport_msg) && nzchar(transport_msg)) lines <- c(lines, paste0("  Details: ", transport_msg))
  if (!is.null(server_msg)    && nzchar(server_msg))    lines <- c(lines, paste0("  Server said: ", server_msg))

  stop(structure(
    class = c("tagbio_api_error", "error", "condition"),
    list(message = paste(lines, collapse = "\n"), call = NULL)
  ))
}

# From https://github.com/jeroen/jsonlite/issues/70
# - drops NULLS from JSON results
null_to_na_recurse <- function(obj) {
  if (is.list(obj)) {
    obj <- jsonlite:::null_to_na(obj)
    obj <- lapply(obj, null_to_na_recurse)
  }
  return(obj)
}

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
#' Obtain one from the \code{API keys} link on your tag.bio instance's home page.  Once an API key has been obtained
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

  # Config (~/.tagbio.json) AND ambient env vars are for AD-HOC use only. A plugin (the runner
  # connect_tagbio.R sets the TAGBIO_PLUGIN_CONTEXT sentinel) must NEVER resolve its host or key from
  # the file OR the environment: it would pick up the developer's carte-blanche API key (privilege
  # escalation) or dial the wrong server (e.g. a TAGBIO_BASE_URL the notebook sets -> a plugin's
  # self-query goes to the services host with a bare /q -> 405). A plugin's connection comes ONLY from
  # the engine packet (explicit url/host_url + the invoking user's token); localhost needs no auth.
  # A dev can opt back in for a local test with TAGBIO_PLUGIN_ALLOW_CONFIG. Mirrors the Python SDK.
  skip_config <- Sys.getenv(TAGBIO_PLUGIN_CONTEXT) != "" && Sys.getenv(TAGBIO_PLUGIN_ALLOW_CONFIG) == ""
  config_data <- if (skip_config) list() else tag_load_config()

  if (url == "") {
    url <- host_url
  }

  # look other places for url — FILE beats ENV, per key; host may be TAGBIO_HOST_URL or TAGBIO_BASE_URL.
  # In a plugin (skip_config) do NOT consult env/file at all — fall straight to localhost.
  if (url == "") {
    url <- if (skip_config) LOCALHOST_URL else resolve_setting(config_data, c(TAGBIO_HOST_URL, TAGBIO_BASE_URL), LOCALHOST_URL)
  }

  # drop trailing slash if it exists
  nurl <- nchar(url)
  if (substr(url, nurl, nurl) == "/") {
    url <- substr(url, 1, nurl-1)
  }

  # "localhost" is a proxy keyword for the full local URL: prepend the http scheme and default the
  # port to 8000 when none is given, so a self-query can write host_url = "localhost" (or
  # "localhost:7999") instead of the full "http://localhost:8000". Case-sensitive lowercase "localhost"
  # ONLY -- matching the flux-http no-auth loophole (name-based: not 127.0.0.1, not mixed case) and the
  # Python SDK's _is_localhost. A host that already carries a scheme, or any other name, is untouched.
  if (grepl("^localhost(:[0-9]+)?$", url)) {
    if (!grepl(":[0-9]+$", url)) url <- paste0(url, ":8000")
    url <- paste0("http://", url)
  }

  tc$url <- url

  # api key — FILE beats ENV, per key (same rule as the host); a plugin (skip_config) uses none.
  if (api_key == "") {
    api_key <- if (skip_config) "" else resolve_setting(config_data, TAGBIO_API_KEY, "")
  }
  tc$api_key <- api_key
  tc$token <- token

  tc
}

#' Print a tagConnect connection
#'
#' Surfaces the resolved host and whether the connection is in localhost mode (one unnamed FC, no
#' auth) or deployed mode (named FCs via \code{/fc-svc/<name>}, auth required), so a
#' localhost-vs-named mixup is visible before a request is built.
#' @param x a tagConnect object
#' @param ... unused
#' @export
print.tagConnect <- function(x, ...) {
  is_local <- x$url == "" || grepl(LOCALHOST, x$url, ignore.case = TRUE) || grepl(LOCALHOST_IP, x$url)
  mode <- if (is_local) "localhost (one unnamed FC, no auth)" else
                        "deployed (named FCs via /fc-svc/<name>, auth required)"
  has_auth <- function(v) !is.null(v) && length(v) == 1 && nzchar(v)
  auth <- if (is_local) "none (localhost needs none)"
          else if (has_auth(x$token)) "token"
          else if (has_auth(x$api_key)) "api_key"
          else "none (WARNING: deployed host with no credentials)"
  cat("<tagConnect>\n",
      "  host: ", if (nzchar(x$url)) x$url else "(unset -> localhost)", "\n",
      "  mode: ", mode, "\n",
      "  auth: ", auth, "\n", sep = "")
  invisible(x)
}

#' Wait until a tag.bio FC is ready to serve queries
#'
#' Polls the FC's schema endpoint until it responds, or the timeout elapses -- so callers don't
#' hand-roll a retry loop against a dev server still loading its archive or running startup tests
#' (which otherwise surfaces as a \code{tagbio_api_error}). Returns invisibly \code{TRUE} once ready,
#' \code{FALSE} on timeout, so it composes: \code{if (fc_ready(con)) tbl(con) \%>\% ...}.
#'
#' @param con a tagConnect connection (or a tagFC)
#' @param fc optional named FC on a deployed services host; omit for a localhost single-FC serve
#' @param timeout seconds to wait before giving up (default 120)
#' @param interval seconds between polls (default 3)
#' @param quiet suppress progress messages (default FALSE)
#' @return invisibly TRUE if the FC became ready within \code{timeout}, else FALSE
#' @export
fc_ready <- function(con, fc = "", timeout = 120, interval = 3, quiet = FALSE) {
  target <- if (inherits(con, "tagFC")) con else tbl(con, fc)
  deadline <- Sys.time() + timeout
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    ready <- tryCatch({
      api_post(target$con, "s", target$url)   # schema call: succeeds only once the archive is loaded
      TRUE
    },
    tagbio_api_error = function(e) FALSE,
    error = function(e) FALSE)

    if (isTRUE(ready)) {
      if (!quiet) message("FC ready at ", target$url, " (after ", attempt,
                          if (attempt == 1) " check)." else " checks).")
      return(invisible(TRUE))
    }
    if (Sys.time() >= deadline) {
      if (!quiet) message("FC not ready at ", target$url, " after ", timeout, "s (", attempt,
                          " checks) -- giving up.")
      return(invisible(FALSE))
    }
    if (!quiet) message("FC not ready yet at ", target$url, "; retrying in ", interval, "s ...")
    Sys.sleep(interval)
  }
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
    return(httr::add_headers(Authorization = paste("Bearer", object$token)))
  }

  # TODO - error?
  return(list())
}

#' @export
api_get <- function(object, ...) {
  UseMethod("api_get", object)
}

#' @export
api_get.tagConnect <- function(object, url) {

  api_head <- api_auth_header(object, url)
  r <- tryCatch(
    httr::GET(url, api_head, encode = "json"),
    error = function(cond) tag_api_stop(url, transport_msg = conditionMessage(cond))
  )

  call_status <- r$status_code
  if (call_status != 200) {
    tag_api_stop(url, status = call_status, server_msg = tag_server_message(r))
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

  # determine authentication
  api_head <- api_auth_header(object, url)

  if (query_type == "s") {
    r <- tryCatch(
      httr::POST(url, api_head, encode = "json"),
      error = function(cond) tag_api_stop(url, transport_msg = conditionMessage(cond))
    )
  } else {
    r <- tryCatch(
      httr::POST(url, body = jsonPayload, api_head, encode = "json"),
      error = function(cond) tag_api_stop(url, transport_msg = conditionMessage(cond))
    )
  }

  # check status! (tag_api_stop classifies 401 auth / 503 not-ready / 502 offline / 500 server / other)
  call_status <- r$status_code
  if (call_status != 200) {
    tag_api_stop(url, status = call_status, server_msg = tag_server_message(r))
  }

  if (return_type == "json") {
    return(httr::content(r))
  } else {
    # wrote a parser here as content was giving floats as strings
    res <- paste0(httr::content(r, as = "text", type = "text/csv", encoding = "UTF-8"))
    res_table <- read.table(text = res, header = T, sep = ",", check.names = F,
                            quote = "\"", comment.char = "")
    # FC downloads can carry duplicate column names (e.g. a collection exposed as
    # both categorical and a same-named numeric variable). read.table keeps them;
    # tibble() rejects duplicates by default and hard-errors. Repair to unique
    # names so the pull succeeds instead of crashing (only affects dup'd columns).
    return(tibble(res_table, .name_repair = "unique"))
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
      print_error("Unable to get FC data")
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



