#' An S3 class representing data response from a Tag.bio protocol
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
tagData <- function(results = data.frame(), parameters = list()) {
  td <- list(
    results = results,
    parameters = parameters
  )

  class(td) <- "tagData"

  td
}

#' Get parameters from a tagData object
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
  tag_data$parameters
}

#' Get result data from a tagData object
#'
#' This getter method returns the data.frame component of a tagData object.
#' If a data_type is provided, only results of this type are returned in
#' the data.frame.
#'
#' @param tag_data tagData object
#' @param data_type type of data to return in data graphics::frame (default NA)
#' @param row_name if set, use this column to create row names (default "")
#' @return data.frame of data
#' @export
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
    df <- df |>
      dplyr::select(dplyr::matches(paste0(
        "^(",
        type_eq,
        ".*|",
        row_name,
        ")$"
      ))) |>
      purrr::set_names(~ stringr::str_replace_all(., type_eq, ""))
  }

  if (row_name != "") {
    df <- df |> tibble::column_to_rownames(var = row_name)
  }

  df
}

# DEPRECATED

#' @export
getDataFrame <- function(tag_data) {
  UseMethod("getDataFrame", tag_data)
}

#' @export
getDataFrame.tagData <- function(tag_data, data_type = NA, row_name = "") {
  get_results(tag_data, data_type, row_name)
}
