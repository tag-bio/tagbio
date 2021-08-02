
#' An S3 class representing a data product (aka "FC") on a tag.bio server
#'
#' The tagFC class allows access to a tag.bio data product as if it were an
#' in-memory object.  An instance of this class may be used to filter items,
#' select attributes and ultimately return a table of data from a tag.bio data
#' product.
#'
#' @section Construction:
#' \code{tagFC} objects are not normally created directly, but are instead created using
#' the \code{tbl} method, much like how a database connection is made through \code{dbplyr}.
#'
#' @section Methods:
#' \code{summary(tc)}: Shows all collections in this data product.  Results are returned in a
#' table with columns...
#'
#' @section Miscellaneous:
#' Data products at tag.bio were historically called "flux capacitors" or "FC"s for short.  The
#' "fc" name lives on in some of the internal classes and methods.

#'
#' @seealso
#' Useful links:
#' \itemize{
#' \item \url{https://backtothefuture.fandom.com/wiki/Flux_capacitor} for the origin of "flux capacitor"
#' }
#'
#'
#' @slot con a tagConnect object
#' @slot fc name of the data product.  When running locally, this should be left empty
#' @slot qdelim delimiter character used to separate collection names and collection values
#' @slot qselect list of attribute selections.  Normally this should be set up using \code{select}
#' methods
#' @slot qfilter list of item filters.  Normally this should be set up using \code{filter} methods
#' @slot collection_defs definitions of available collections for this data product.  Populated when
#' a tabio_fc object is first created.
#' @slot url full URL to this data product
#'
#' @examples
#' # connect to local host, no API key required
#' tag_con <- tagConnect()
#'
#' # access to tagFC object from local instance
#' fc <- tbl(tag_con)
#'
#' @export
tagFC <- function(con, fc = "", qdelim = " = ") {
  fc_obj <- list(con = con,
                 fc = fc,
                 qdelim = qdelim,
                 qselect = list(),
                 qfilter = list(),
                 collection_defs = list(),
                 url = "")

  class(fc_obj) <- "tagFC"

  # set up FC URL
  fc_obj$url <- con$url

  if (fc != "") {
    fc_obj$url <- paste0(fc_obj$url, "/fc-svc/", fc)
  }

  fc_obj
}


# parse JSON from FC

tag_summary_row <- function(x) {
  res <- switch(x$data_function_type,
                numeric = tibble::tibble(collection = x$collection,
                                         collection_type = x$data_function_type,
                                         collection_size =  x$collection_size,
                                         collection_entity_count = NA),
                categorical = tibble::tibble(collection = x$collection,
                                             collection_type = x$data_function_type,
                                             collection_size =  x$collection_size,
                                             collection_entity_count = x$collection_entity_count))
  return(res)
}

#' @export
summary.tagFC <- function(object, ...) {
  # return collections as a tibble
  get_collection_defs(object)
  coll_tibs <- lapply(get_collection_defs(object), tag_summary_row)
  return(do.call(rbind, coll_tibs))
}

#' @export
info <- function (.data, ...) {
  UseMethod("info", .data)
}

info.tagFC <- function(.data) {
  # returns FC provenance information
  return(get_info(.data$url, .data$con$api_key))
}

#' @export
print.tagFC <- function(x) {
  list(x$fc)
}

#' @export
get_collection_defs <- function(.data) {
  UseMethod("get_collection_defs", .data)
}

get_collection_defs.tagFC <- function(.data) {

  # lazy loaded attribute
  if (length(.data$collection_defs) == 0) {
    # assume not loaded
    # use the collections method to pull summary data from FC
    jsonPayload <- list(
      zip = TRUE,
      groups = c("developer")
    )

    script <- list(
      method = "collection"
    )

    jsonPayload[['script']] = script
    collections_json <- fc_post_call("q", .data$url, .data$con$api_key, "json", jsonPayload)

    print(collections_json)
    .data$collection_defs <- parse_collection_query(collections_json)
  }
  return(.data$collection_defs)
}

#' @export
get_analysis_variables <- function(.data) {
  UseMethod("get_analysis_variables", .data)
}

get_analysis_variables.tagFC <- function(.data) {

  # build up analysis variables from select
  if (length(.data$qselect) == 0) {
    print("ERROR:  Must perform a select before executing FC query")
    return(NA)
  }

  # loop through selects and add to analysis variables
  analysis_variables <- list()
  cnt <- 1
  for (i in c(1:length(.data$qselect))) {
    # TODO - check that results are allowable types...
    av <- tag_select_eval(.data, !!.data$qselect[[i]])
    #if (is(av, "character")) {
    #  # attempt to convert to tag variables

    #} else {
    analysis_variables[[cnt]] <- av
    cnt <- cnt + 1
    #}
  }

  return(analysis_variables)
}

#' @inheritParams dplyr::select
#' @export
#' @importFrom dplyr select
#' @export
select.tagFC <- function(.data, ...) {
  .data$qselect <- c(.data$qselect, rlang::enexprs(...))
  return(.data)
}

#' @inheritParams dplyr::filter
#' @export
#' @importFrom dplyr filter
#' @export
filter.tagFC <- function(.data, ...) {
  .data$qfilter <- c(.data$qfilter, rlang::exprs(...))
  return(.data)
}

#' @export
get_background <- function(.data) {
  UseMethod("get_background", .data)
}

get_background.tagFC <- function(.data) {
  # empty background if no filters have been specified
  if (length(.data$qfilter) == 0) {
    # no background selected
    return(NULL)
  }

  # loop through filters and add to criteria
  criteria_list <- list()
  for (i in c(1:length(.data$qfilter))) {
    # TODO - check that results are allowable types...
    criteria_list[[i]] <- to_tag(.data, !!.data$qfilter[[i]])
  }
  bkg <- CategoricalCompound(criteria = criteria_list, operator = "AND")
  return(bkg)
}

#' @inheritParams dplyr::collect
#' @export
#' @importFrom dplyr collect
#' @export
collect.tagFC <- function(x) {

  # use the download method to pull data from FC
  tc <- x$con

  qdelim <- paste0("\\s*", x$qdelim, "\\s*")
  jsonPayload <- list(
    zip = TRUE,
    groups = c("developer")
  )

  # select
  analysis_variables <- tag_select_eval(x, !!!x$qselect)

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

  tag_data_frame <- fc_post_call("q", x$url, tc$api_key, "text", jsonPayload)

  tibble::tibble(tag_data_frame)
}

#' @export
run_protocol <- function(fc, protocol_instance) {
  UseMethod("run_protocol", fc)
}

run_protocol.tagFC <- function(fc, protocol_instance) {

  # use the download method to pull data from FC
  tc <- fc$con

  jsonPayload <- list(
    zip = TRUE,
    groups = c("developer"),
    protocol_instance = protocol_instance
  )

  tag_data_frame <- fc_post_call("q", fc$url, tc$api_key, "text", jsonPayload)

  # set up the tagData instance
  tag_data <- tagData(results = tibble::tibble(tag_data_frame))
  return(tag_data)
}

#' @export
run_script <- function(fc, script) {
  UseMethod("run_script", fc)
}

run_script.tagFC <- function(fc, script) {

  # use the download method to pull data from FC
  tc <- fc$con

  jsonPayload <- script

  tag_data_frame <- fc_post_call("q", fc$url, tc$api_key, "text", jsonPayload)

  # set up the tagData instance
  tag_data <- tagData(results = tibble::tibble(tag_data_frame))
  return(tag_data)
}


## GENERAL METHODS

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
  collection_defs <- purrr::set_names(collection_defs, lapply(collection_defs, function(x) {x$collection}))

  return(collection_defs)
}

# general method for all http post calls to tag

fc_post_call <- function(query_type, url, api_key, return_type = "json", jsonPayload = NA) {

  # set up url
  url <- paste0(url, "/", query_type)

  # use api key
  api_data <- unlist(strsplit(api_key, ":"))
  if (length(api_data) != 2) {
    api_data <- c("", "") # defaults to empty user/pwd
  }

  if (query_type == "s") {
    r <- httr::POST(url,
                    httr::authenticate(api_data[1], api_data[2], type = "basic"),
                    encode = "json")
  } else {
    r <- httr::POST(url,
                    body = jsonPayload,
                    httr::authenticate(api_data[1], api_data[2], type = "basic"),
                    encode = "json")
  }
  if (return_type == "json") {
    return(httr::content(r))
  } else {
    return(httr::content(r, as = "parsed", type = "text/csv",
                         encoding = "UTF-8"))
  }
}

get_info <- function(url, api_key) {

  # use s query to get fc info
  info_json <- fc_post_call("s", url, api_key)

  # update time stamps
  info_json$start_time <- as.POSIXct(as.numeric(info_json$start_time) / 1000,
                                     origin = "1970-01-01",tz = "GMT")
  info_json$data_timestamp <- as.POSIXct(as.numeric(info_json$data_timestamp) / 1000,
                                         origin = "1970-01-01",tz = "GMT")

  return(info_json)
}




