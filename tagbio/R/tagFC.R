
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

  if ((fc != "") & (fc != "fc-local")) {
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
                                             collection_entity_count = x$collection_entity_count),
                `numeric-matrix` = tibble::tibble(collection = x$collection,
                                                  collection_type = x$data_function_type,
                                                  collection_size =  x$collection_size,
                                                  collection_entity_count = x$collection_entity_count),
                `categorical-matrix` = tibble::tibble(collection = x$collection,
                                                      collection_type = x$data_function_type,
                                                      collection_size =  x$collection_size,
                                                      collection_entity_count = x$collection_entity_count),
                `data-frame-numeric` = tibble::tibble(collection = x$collection,
                                                    collection_type = x$data_function_type,
                                                    collection_size =  x$collection_size,
                                                    collection_entity_count = x$collection_entity_count))
  return(res)
}

#' @export
summary.tagFC <- function(object, ...) {
  # return collections as a tibble
  coll_tibs <- lapply(get_collection_defs(object), tag_summary_row)
  return(do.call(rbind, coll_tibs))
}

# colnames method
# - returns back a mix of categorical collections and numeric variables

#' @export
colnames.tagFC <- function(x, ...) {

  # follow the same logic as select for specifying collection names
  qselect <- rlang::enexprs(...)
  analysis_variables <- tag_select_eval(x, !!!qselect)

  if (length(analysis_variables) == 0) {
    # nothing specified, so we do all collections
    analysis_variables <- get_collection_defs(x)
  }

  # loop through collections list and find any numeric collections
  # that need to be expanded
  num_colls <- list()
  cnt <- 1

  for (av in analysis_variables) {
    if (av$data_function_type == "numeric") {
      if (is.null(av$variable)) {
        # not a variable
        num_colls[[cnt]] <- list("data_function_type" = av$data_function_type, "collection" = av$collection)
        cnt <- cnt + 1
      }
    }
  }

  # - lookup variables for num collections
  var_defs <- list()
  if (length(num_colls) > 0) {
    # use the variables method to pull variable data from FC
    jsonPayload <- list(
      zip = TRUE,
      groups = c("developer")
    )

    script <- list(
      method = "variable",
      analysis_variables = num_colls
    )

    jsonPayload[['script']] = script
    jsonPayload[['stringify_names']] = "true"

    variables_json <- api_post(x$con, "q", x$url, "json", jsonPayload)

    for (res in variables_json$results) {
      name <- res$values$collection
      if (is.null(var_defs[[name]])) {
        var_defs[[name]] <- list()
      }
      cnt <- length(var_defs[[name]]) + 1
      var_defs[[name]][[cnt]] <- list("collection" = name,
                                      "variable" = res$values$variable)
    }
  }

  # loop through and create column names
  delim <- x$qdelim
  col_names <- c()

  for (av in analysis_variables) {
    if (av$data_function_type == "numeric") {
      if (is.null(av$variable)) {
      # use the values from query
        if (!is.null(var_defs[[av$collection]])) {
          for (res in var_defs[[av$collection]]) {
            col_names <- c(col_names, paste0(res$collection, delim, res$variable))
          }
        }
      } else {
        # just use the existing numeric variable
        col_names <- c(col_names, paste0(av$collection$collection, delim, av$variable))
      }
    } else {
      col_names <- c(col_names, av$collection)
    }
  }
  return(col_names)
}

#' @export
info <- function(.data, ...) {
  UseMethod("info", .data)
}

#' @export
info.tagFC <- function(.data) {
  # returns FC provenance information
  return(get_info(.data$url, .data$con))
}

#' @export
print.tagFC <- function(x) {
  list(x$fc)
}

#' @export
get_collection_defs <- function(.data) {
  UseMethod("get_collection_defs", .data)
}

#' @export
get_collection_defs.tagFC <- function(.data) {

  # lazy loaded attribute
  if (length(.data$collection_defs) == 0) {
    # assume not loaded
    # use the collections method to pull summary data from FC
    jsonPayload <- list(
      zip = TRUE,
      groups = c("developer")
    )

    # override the default collection query limit of 50k
    collection_limit <- 1000000000

    script <- list(
      method = "collection",
      limit = collection_limit,
      minify = TRUE
    )

    jsonPayload[['script']] = script
    jsonPayload[['stringify_names']] = "true"
    #collections_json <- fc_post_call("q", .data$url, .data$con$api_key,
    #                                 "json", jsonPayload, token = .data$con$token)
    collections_json <- api_post(.data$con, "q", .data$url, "json", jsonPayload)

    .data$collection_defs <- parse_collection_query(collections_json)

  }
  return(.data$collection_defs)
}

# DEPRECATED...
#' @export
get_variable_defs <- function(.data, ...) {
  UseMethod("get_variable_defs", .data)
}

#' @export
get_variable_defs.tagFC <- function(.data, ...) {

  # user supplied list of collections if they desire just a subset
  collection_list <- as.list(substitute(list(...)))[-1]
  collection_list <- unlist(lapply(collection_list, paste))

  # all collections in FC
  collection_defs <- get_collection_defs(.data)

  if (length(collection_list) == 0) {
    # nothing supplied, so use all collections
    collection_list <- names(collection_defs)
  }

  # use the variables method to pull variable data from FC
  jsonPayload <- list(
    zip = TRUE,
    groups = c("developer")
  )

  # format collection list
  anl_vars <- lapply(
    collection_list, function(b) {
      list("data_function_type" = collection_defs[[b]]$data_function_type,
           "collection" = collection_defs[[b]]$collection)})

  script <- list(
    method = "variable",
    analysis_variables = anl_vars
  )

  jsonPayload[['script']] = script
  jsonPayload[['stringify_names']] = "true"

  variables_json <- api_post(.data$con, "q", .data$url, "json", jsonPayload)
  var_defs <- parse_variable_query(variables_json)

  return(var_defs)
}

#' @export
get_analysis_variables <- function(.data) {
  UseMethod("get_analysis_variables", .data)
}

#' @export
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

#' @export
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
  jsonPayload[['stringify_names']] = "true"

  tag_data_frame <- api_post(tc, "q", x$url, "text", jsonPayload = jsonPayload)

  tibble::tibble(tag_data_frame)
}

#' @export
run_protocol <- function(fc, protocol_instance) {
  UseMethod("run_protocol", fc)
}

#' @export
run_protocol.tagFC <- function(fc, protocol_instance) {

  # use the download method to pull data from FC
  tc <- fc$con

  jsonPayload <- list(
    zip = TRUE,
    groups = c("developer"),
    protocol_instance = protocol_instance
  )

  #tag_data_frame <- fc_post_call("q", fc$url, tc$api_key, "text",
  #                               jsonPayload, token=tc$token)
  tag_data_frame <- api_post(tc, "q", fc$url, "text", jsonPayload)

  # set up the tagData instance
  tag_data <- tagData(results = tibble::tibble(tag_data_frame))
  return(tag_data)
}

#' @export
run_script <- function(fc, script) {
  UseMethod("run_script", fc)
}

#' @export
run_script.tagFC <- function(fc, script) {

  # use the download method to pull data from FC
  tc <- fc$con

  jsonPayload <- list(
    zip = TRUE,
    groups = c("developer"),
    script = script
  )

  #tag_data_frame <- fc_post_call("q", fc$url, tc$api_key, "text",
  #                               jsonPayload, token=tc$token)
  tag_data_frame <- api_post(tc, "q", fc$url, "text", jsonPayload)

  # set up the tagData instance
  tag_data <- tagData(results = tibble::tibble(tag_data_frame))
  return(tag_data)
}


## GENERAL METHODS

parse_collection_values <- function(res_values) {

  if(!is.null(res_values$drt)) { # the minified case
    res_values$data_reference_type <- res_values$drt
    if (res_values$data_reference_type == 't') res_values$data_reference_type = 'categorical'
    if (res_values$data_reference_type == 'n') res_values$data_reference_type = 'numeric'
    if (res_values$data_reference_type == 'tx') res_values$data_reference_type = 'categorical-matrix'
    if (res_values$data_reference_type == 'nx') res_values$data_reference_type = 'numeric-matrix'
    res_values$collection <- res_values$c
    res_values$collection-size <- res_values$cs
    res_values$collection-entity-count <- res_values$cec
  }

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

  # TODO - SUPPORT FOR DATA FRAMES...
  tag_coll <- switch(variable_type,
                     numeric = NumericCollection(collection = res_values$collection,
                                                 collection_size = res_values$`collection-size`),
                     categorical = CategoricalCollection(collection = res_values$collection,
                                                         collection_size = res_values$`collection-size`,
                                                         collection_entity_count = res_values$`collection-entity-count`),
                     `numeric-matrix` = CategoricalCollection(collection = res_values$collection,
                                                              collection_size = res_values$`collection-size`,
                                                              collection_entity_count = res_values$`collection-entity-count`),
                     `categorical-matrix` = CategoricalCollection(collection = res_values$collection,
                                                                  collection_size = res_values$`collection-size`,
                                                                  collection_entity_count = res_values$`collection-entity-count`),
                     `data-frame-numeric` = CategoricalCollection(collection = res_values$collection,
                                                                collection_size = res_values$`collection-size`,
                                                                collection_entity_count = res_values$`collection-entity-count`))

  return(tag_coll)
}

parse_collection_query <- function(query_res) {
  # parses collection query results to determine collections
  res <- query_res$results
  collection_defs <- lapply(res, function(x) {
    if (!is.na(x$v)) { parse_collection_values(x$v) } # the minified case
    else { parse_collection_values(x$values) } 
  })
  collection_defs <- purrr::set_names(collection_defs, lapply(collection_defs, function(x) {x$collection}))

  return(collection_defs)
}

# TODO - parse variables

parse_variable_values <- function(res_values) {

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

  # TODO - SUPPORT FOR OTHER DATA TYPES...
  tag_var <- switch(variable_type,
                    numeric = NumericVariable(collection = res_values$collection,
                                              variable = res_values$variable),
                    categorical = CategoricalVariable(collection = res_values$collection,
                                                      variable = res_values$variable)
  )
  return(tag_var)
}

parse_variable_query <- function(query_res) {
  # parses variable query results to determine variables
  res <- query_res$results
  variable_defs <- lapply(res, function(x) {parse_variable_values(x$values)})
  variable_defs <- purrr::set_names(variable_defs, lapply(variable_defs, function(x) {x$variable}))

  return(variable_defs)
}

get_info <- function(url, tc) {

  # use s query to get fc info
  #info_json <- fc_post_call("s", url, api_key, token = token)
  info_json <- api_post(tc, "s", url)

  # drop assets for now
  info_json$asset <- NULL

  # update time stamps
  info_json$start_time <- as.POSIXct(as.numeric(info_json$start_time) / 1000,
                                     origin = "1970-01-01",tz = "GMT")
  info_json$data_timestamp <- as.POSIXct(as.numeric(info_json$data_timestamp) / 1000,
                                         origin = "1970-01-01",tz = "GMT")

  return(info_json)
}




