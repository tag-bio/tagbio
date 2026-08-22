
#' An S3 class representing data response from a protocol
#'
#' The tagData class captures the resulting data frame and pass-through
#' arguments returned from a data product protocol.
#'
#' The tagData class holds tagbio.data as a data.frame attribute of the
#' object.  TagbioData objects are typically created through a
#' \code{getTagData} query or created directly from the flux capacitor and
#' passed to a user defined method.  The TagbioData object may optionally
#' have a list of parameters defined.  This list is typically populated by
#' the flux capacitor and is used to direct user defined methods.
#'
#' @param results a data frame response from the protocol
#' @param parameters a list of pass-through parameters from the protocol
#' @export
tagData <- function(results = data.frame(),
                       parameters = list()) {
  td <- list(results = results,
             parameters = parameters)

  class(td) <- "tagData"

  td
}

#' Get parameters from a tagData object.
#'
#' This getter method returns the pass through parameters of a tagData object.
#'
#' @param tag_data tagData object
#' @return list of parameters
#' @export
get_parameters <- function(tag_data) {
  UseMethod("get_parameters", tag_data)
}

#' @export
get_parameters.tagData <- function(tag_data) {
  return(tag_data$parameters)
}

#' Get result data from a tagData object.
#'
#' This getter method returns the data.frame component of a tagData object.  If a
#' data_type is provided, only results of this type are returned in the data.frame.
#'
#' @param tag_data tagData object
#' @param data_type type of data to return in data frame (default NA)
#' @param row_name if set, use this column to create row names (default "")
#' @return data.frame of data
#' @export
#' @examples
get_results <- function(tag_data, data_type = NA, row_name = "") {
  UseMethod("get_results", tag_data)
}

#' @export
get_results.tagData <- function(tag_data, data_type = NA, row_name = "") {

  df <- tag_data$results

  if (!is.na(data_type)) {
    # escape parens
    data_type <- gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", data_type))

    type_eq <- paste0(data_type, " = ")
    df <- df %>% select(matches(paste0("^(", type_eq, ".*|", row_name, ")$"))) %>%
      set_names(~stringr::str_replace_all(., type_eq, ""))
  }

  if (row_name != "" && row_name %in% names(df)) {
    df <- df %>% column_to_rownames(var = row_name)
  }
  return(df)
}

# DEPRECATED

#' @export
getDataFrame <- function(tag_data) {
  UseMethod("getDataFrame", tag_data)
}

#' @export
getDataFrame.tagData <- function(tag_data, data_type = NA, row_name = "") {

  return(get_results(tag_data, data_type, row_name))
}

#' Get the FC server-info (/s) map from a tagData object.
#'
#' Returns the full server-info map the engine embeds in the plugin packet (no
#' network call). Keys mirror the FC's /s response (name, title, version,
#' data_timestamp, entity_count, ...) and are generic -- new keys added to /s
#' appear here without any SDK change.
#'
#' @param tag_data tagData object
#' @return named list of FC server info (empty list if none present)
#' @export
get_fc_info <- function(tag_data) {
  UseMethod("get_fc_info", tag_data)
}

#' @export
get_fc_info.tagData <- function(tag_data) {
  info <- tag_data$fc_info
  if (is.null(info)) list() else info
}

#' Get the human-readable data-snapshot timestamp from a tagData object.
#'
#' Formats the archive/snapshot creation time (epoch millis in /s) to a
#' readable local-time string, mirroring the client-side /s formatting.
#'
#' @param tag_data tagData object
#' @return character timestamp (e.g. "2026-08-11 14:32:00"), or "" if absent
#' @export
get_data_timestamp <- function(tag_data) {
  UseMethod("get_data_timestamp", tag_data)
}

#' @export
get_data_timestamp.tagData <- function(tag_data) {
  ts <- get_fc_info(tag_data)[["data_timestamp"]]
  if (is.null(ts) || is.na(ts) || ts == "") return("")
  format(as.POSIXct(as.numeric(ts) / 1000, origin = "1970-01-01"),
         "%Y-%m-%d %H:%M")
}
