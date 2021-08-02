
#' An S3 class representing data to return to the tag.bio platform
#'
#' The tagResult class allows the user to return data to the tag.bio
#' platform.  It is typically only used in user-defined protocol methods
#' called from the flux capacitor.
#'
#' Results are typically stored in a file either as a data frame or a
#' plot saved in a graphics format or HTML.  Any messages not part of
#' the results can be placed in the message file.
#'
#' A tagResult is typically prepopulated and passed to a user-
#' defined R function.  The type and paths direct the user what output
#' is expected and where results should be saved.
#'
#' @param result_data a dataframe of results
#' @param output_path path to resullts file
#' @param message_path path to a message file
#' @param result_type format of the results (html, png, pdf)
#' @export
tagResult <- function(results, output_path, message_path, result_type) {

  tr <- list(results = results,
             output_path = output_path,
             message_path = message_path,
             result_type = result_type)

  class(tr) <- "tagResult"

  tr
}

#' Add data to a tagResult.
#'
#' This method adds results to a tagResult object
#'
#' @param tag_result tagResult object
#' @param results data.frame or file path to results data
#' @param result_type either \code{data.frame}, \code{jpeg}, \code{pdf},
#'    \code{png}, \code{svg}
#'
#' @return updated tagResult object
#' @export
#' @seealso \code{\link{tagData}}
add_result <- function(tag_result, results, results_type) {
  UseMethod("add_result", tag_data, results, results_type)
}

add_result.tagData <- function(tag_result, results, results_type) {

  if (!(result_type %in% c("data.frame", "html", "jpeg", "pdf", "png", "json"))) {
    stop("Not an expected result_type")
  }

  # TODO:  Add type checks...

  return(tag_result)
}

