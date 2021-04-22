
library(rjson)
library(dplyr)
library(rlang)

#' An S4 class representing connection to a tagbio server
#'
#' The tagConnect class holds tagbio.data as a data.frame attribute of the
#' object.  TagbioData objects are typically created through a
#' \code{getTagData} query or created directly from the flux capacitor and
#' passed to a user defined method.  The TagbioData object may optionally
#' have a list of parameters defined.  This list is typically populated by
#' the flux capacitor and is used to direct user defined methods.
#'
#' @slot url url to the tagbio server
#' @slot api_key tagbio api key
#' @export tagConnect
#' @exportClass tagConnect
tagConnect <- setClass(
  "tagConnect",
  slots = c(host_url = "character", api_key = "character",
      url = "character")
)

tag_load_config <- function() {
  home <- Sys.getenv("HOME")
  config_file <- file.path(home, ".tagbio.json")

  if (file.exists(config_file)) {
    config_data <- rjson::fromJSON(file = config_file)
  } else {
    config_data <- list()
  }
  return(config_data)
}

setMethod("initialize", "tagConnect",
  function(.Object, host_url = "", api_key = "", url = "", ...) {

    config_data <- tag_load_config()
    url <- host_url

    # look other places for url/api key
    if (url == "") {
      if (Sys.getenv("TAGBIO_HOST_URL") != "") {
        url <- Sys.getenv("TAGBIO_HOST_URL")
      } else {
        if ("TAGBIO_HOST_URL" %in% names(config_data)) {
          url <- config_data[["TAGBIO_HOST_URL"]]
        } else {
          url <- "http://localhost:8000"
        }
      }
    }
    .Object@url <- url

    if (api_key == "") {
      if (Sys.getenv("TAGBIO_API_KEY") != "") {
        api_key <- Sys.getenv("TAGBIO_API_KEY")
      } else {
        if ("TAGBIO_API_KEY" %in% names(config_data)) {
          api_key <- config_data[["TAGBIO_API_KEY"]]
        }
      }
    }
    .Object@api_key <- api_key

    return(.Object)
  }
)

setGeneric("tbl", dplyr::tbl)

#' @export
setMethod(
  "tbl",
  signature = c(src = "tagConnect"),
  function(src, fc) {
    return(tbl_tag(fc = fc, con = src))
  }
)

#' @export
setMethod(
  "summary",
  signature = c(object = "tagConnect"),
  function(object, ...) {
    # returns FC data as a tibble
    kung_url <- paste0(object@url, "/kung-services/db/capacitors")

    # use api key
    api_data <- unlist(strsplit(object@api_key, ":"))
    if (length(api_data) != 2) {
      api_data <- c("", "") # defaults to empty user/pwd
    }

    r <- httr::GET(kung_url,
                    httr::authenticate(api_data[1], api_data[2], type = "basic"),
                    encode = "json")
    fcs_json <- httr::content(r)
    fcs_tbl <- data.frame(do.call(rbind, fcs_json))

    if (!("key" %in% colnames(fcs_tbl))) {
      if (("localhost" %in% object@url) | ("128.0.0.1" %in% object@url)) {
        return(
          tibble(key = c("localhost"), site = c("localhost"), description = c("localhost"),
            displayname = c("localhost"), url = c(object@url))
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
)

#' @export
setGeneric(
  "tagListFCs",
  def = function(.data = "tagConnect") {
    standardGeneric("tagListFCs")
  }
)

setMethod(
  "tagListFCs",
  signature = c(.data),
  function(.data) {
    fcs_tbl <- summary(.data)

    if (is.null(fcs_tbl)) {
      return(c())
    } else {
      return(unlist(fcs_tbl %>% pull(key)))
    }
  }
)

#' @export
setMethod(
  "tbl",
  signature = c(src = "tagConnect"),
  function(src, fc) {
    return(tbl_tag(fc = fc, con = src))
  }
)

#############################################
## tagbio_fc

parse_collection_values <- function(res_values) {

  # 2.52.4 after use data_reference_type, before uses variable_type
  # data_reference_type (new)

  if (is.null(res_values$data_function_type)) {
    if (is.null(res_values$data_reference_type)) {
      variable_type <- res_values$variable_type
    } else {
      variable_type <- res_values$data_reference_type
    }
  } else {
    variable_type <- res_values$data_function_type
  }

  tag_coll <- switch(variable_type,
                     numeric = NumericCollection(collection = res_values$collection$name,
                                                 collection_size = res_values$`collection-size`),
                     categorical = CategoricalCollection(collection = res_values$collection$name,
                                                         collection_size = res_values$`collection-size`,
                                                         collection_entity_count = res_values$`collection-entity-count`))

  return(tag_coll)
}

parse_collection_query <- function(query_res) {
  # parses collection query results to determine collections
  res <- query_res$results
  collection_defs <- lapply(res, function(x) {parse_collection_values(x$values)})
  collection_defs <- set_names(collection_defs, lapply(collection_defs, function(x) {x@collection}))
  return(collection_defs)
}

get_collection_defs <- function(url, api_key) {

  # use the collections method to pull summary data from FC
  url <- paste0(url, "/q")

  jsonPayload <- list(
    zip = TRUE,
    groups = c("developer")
  )

  script <- list(
    method = "collection"
  )

  jsonPayload[['script']] = script

  # use api key
  api_data <- unlist(strsplit(api_key, ":"))
  if (length(api_data) != 2) {
    api_data <- c("", "") # defaults to empty user/pwd
  }

  r <- httr::POST(url,
                  body = jsonPayload,
                  #httr::verbose(),
                  httr::authenticate(api_data[1], api_data[2], type = "basic"),
                  encode = "json")
  collections_json <- httr::content(r)

  return(parse_collection_query(collections_json))

}

get_info <- function(url, api_key) {

  # use the /s method to pull info about the FC
  url <- paste0(url, "/s")
  jsonPayload <- list()

  # use api key
  api_data <- unlist(strsplit(api_key, ":"))
  if (length(api_data) != 2) {
    api_data <- c("", "") # defaults to empty user/pwd
  }

  r <- httr::POST(url,
                  body = jsonPayload,
                  #httr::verbose(),
                  httr::authenticate(api_data[1], api_data[2], type = "basic"),
                  encode = "json")
  info_json <- httr::content(r)

  # update time stamps
  info_json$start_time <- as.POSIXct(as.numeric(info_json$start_time) / 1000,
                                     origin = "1970-01-01",tz = "GMT")
  info_json$data_timestamp <- as.POSIXct(as.numeric(info_json$data_timestamp) / 1000,
                                         origin = "1970-01-01",tz = "GMT")

  return(info_json)
}

#' An S4 class representing tagbio_fc
#'
#'
#' @export tagbio_fc
#' @exportClass tagbio_fc

tagbio_fc <- setClass(
  "tagbio_fc",
  slots = c(fc = "character",
            con = "tagConnect",
            qdelim = "character",
            qselect = "list",
            qfilter = "list",
            collection_defs = "list",
            url = "character")
)

setMethod("initialize", "tagbio_fc",
          function(.Object, fc, con,  qdelim = " = ", qselect = list(), qfilter = list(), ...) {
            .Object@fc <- fc
            .Object@con <- con
            .Object@qselect <- qselect
            .Object@qfilter <- qfilter
            .Object@qdelim <- qdelim

            # set up FC URL
            .Object@url <- con@url
            if (fc != "") {
              .Object@url <- paste0(.Object@url, "/fc-svc/", fc)
            }

            .Object@collection_defs <- get_collection_defs(.Object@url, con@api_key)
            return(.Object)
          }
)

tag_summary_row <- function(x) {
  res <- switch(x@data_function_type,
                numeric = tibble(collection = x@collection,
                                 collection_type = x@data_function_type,
                                 collection_size =  x@collection_size,
                                 collection_entity_count = NA),
                categorical = tibble(collection = x@collection,
                                     collection_type = x@data_function_type,
                                     collection_size =  x@collection_size,
                                     collection_entity_count = x@collection_entity_count))
  return(res)
}

#' @export
setMethod(
  "summary",
  signature = c(object = "tagbio_fc"),
  function(object, ...) {
    # return collections as a tibble
    coll_tibs <- lapply(object@collection_defs, tag_summary_row)
    return(do.call(rbind, coll_tibs))
  }
)

#' @export
setGeneric(
  "info",
  def = function(.data) {
    standardGeneric("info")
  }
)

setMethod(
  "info",
  signature = c(.data = "tagbio_fc"),
  function(.data) {
    # returns FC provenance information
    return(get_info(.data@url, .data@con@api_key))
  }
)

#' @export
setMethod(
  "print",
  signature = c(x = "tagbio_fc"),
  function(x) {
    list(x@fc)
  }
)

#' @export
setGeneric(
  "get_analysis_variables",
  def = function(.data) {
    standardGeneric("get_analysis_variables")
  }
)

setMethod(
  "get_analysis_variables",
  signature = c(.data = "tagbio_fc"),
  function(.data) {

    # build up analysis variables from select
    if (length(.data@qselect) == 0) {
      print("ERROR:  Must perform a select before executing FC query")
      return(NA)
    }

    # loop through selects and add to analysis variables
    analysis_variables <- list()
    cnt <- 1
    for (i in c(1:length(.data@qselect))) {
      # TODO - check that results are allowable types...
      av <- tag_select_eval(.data, !!.data@qselect[[i]])
      #if (is(av, "character")) {
      #  # attempt to convert to tag variables

      #} else {
      analysis_variables[[cnt]] <- av
      cnt <- cnt + 1
      #}
    }

    return(analysis_variables)
  }
)

setGeneric("select", dplyr::select)

#' @export
setMethod(
  "select",
  signature = c(.data = "tagbio_fc"),
  function(.data, ...) {
    .data@qselect <- c(.data@qselect, rlang::enexprs(...))
    return(.data)
  }
)

setGeneric("filter", dplyr::filter)

#' @export
setMethod(
  "filter",
  signature = c(.data = "tagbio_fc"),
  function(.data, ...) {
    .data@qfilter <- c(.data@qfilter, rlang::exprs(...))
    return(.data)
  }
)

#' @export
setGeneric(
  "get_background",
  def = function(.data) {
    standardGeneric("get_background")
  }
)

setMethod(
  "get_background",
  signature = c(.data = "tagbio_fc"),
  function(.data) {
    # empty background if no filters have been specified
    if (length(.data@qfilter) == 0) {
      # no background selected
      return(NULL)
    }

    # loop through filters and add to criteria
    criteria_list <- list()
    for (i in c(1:length(.data@qfilter))) {
      # TODO - check that results are allowable types...
      criteria_list[[i]] <- to_tag(.data, !!.data@qfilter[[i]])
    }
    bkg <- CategoricalCompound(criteria = criteria_list, operator = "AND")
    return(bkg)
  }
)

setGeneric("collect", dplyr::collect)

#' @export
setMethod(
  "collect",
  signature = c(x = "tagbio_fc"),
  function(x) {

    # use the download method to pull data from FC
    tc <- x@con
    url <- paste0(x@url, "/q")
    qdelim <- paste0("\\s*", x@qdelim, "\\s*")
    jsonPayload <- list(
      zip = TRUE,
      groups = c("developer")
    )

    # select
    analysis_variables <- tag_select_eval(x, !!!x@qselect)

    # filter
    background <- get_background(x)

    analysis_variables_json <- lapply(analysis_variables, to_json)
    names(analysis_variables_json) <- NULL

    script <- list(
      method = "download",
      analysis_variables = analysis_variables_json
    )

    if (!is.null(background)) {
      script[['background']] <- to_json(background)
    }

    jsonPayload[['script']] = script

    # use api key
    api_data <- unlist(strsplit(tc@api_key, ":"))
    if (length(api_data) != 2) {
      api_data <- c("", "") # defaults to empty user/pwd
    }

    r <- httr::POST(url,
                    body = jsonPayload,
                    #httr::verbose(),
                    httr::authenticate(api_data[1], api_data[2], type = "basic"),
                    encode = "json")

    tag_data_frame <- httr::content(r, as = "parsed", type = "text/csv",
                                    encoding = "UTF-8")


    tibble(tag_data_frame)
  }
)


tbl_tag <- function(fc, con = NULL) {
  # creates a structure to hold the connection, FC name
  tagbio_fc(fc = fc, con = con, qselect = list(), qfilter = list())
}

