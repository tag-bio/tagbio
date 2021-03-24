
library("httr")
library("svglite")

#' An S4 class representing data from the tag.bio platform
#'
#' The TagbioData class holds tagbio.data as a data.frame attribute of the
#' object.  TagbioData objects are typically created through a
#' \code{getTagData} query or created directly from the flux capacitor and
#' passed to a user defined method.  The TagbioData object may optionally
#' have a list of parameters defined.  This list is typically populated by
#' the flux capacitor and is used to direct user defined methods.
#'
#' @slot data.frame a data frame typically retrieved from tag.bio
#' @slot parameters a list of parameters from tag.bio
#' @export TagbioData
#' @exportClass TagbioData
TagbioData <- setClass(
    "TagbioData",
    representation(data.frame = "data.frame",
                   parameters = "list"),
    prototype(data.frame = data.frame(), parameters = list())
)

#' Get data from a TagbioData object.
#'
#' This convenience method returns the data.frame component of a TagbioData object.  If a
#' data_type is provided, only results of this type are returned in the data.frame.
#'
#' @param tag_data TagbioData object
#' @param data_type type of data to return in data frame (default NA)
#' @param row_name if set, use this column to create row names (default "")
#' @return data.frame of data
#' @export
#' @examples

setGeneric(
    "getDataFrame",
    def=function(tag_data, data_type = NA, row_name = "") {
        standardGeneric("getDataFrame")
    }
)

setMethod(
    "getDataFrame",
    signature = "TagbioData",
    function(tag_data, data_type = NA, row_name = "") {
        df <- tag_data@data.frame

        if (!is.na(data_type)) {
            # escape parens
            data_type <- gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", data_type))

            type_eq <- paste0(data_type, " = ")
            df <- df %>% select(matches(paste0("^(", type_eq, ".*|", row_name, ")$"))) %>%
                set_names(~stringr::str_replace_all(., type_eq, ""))
        }

        if (row_name != "") {
            df <- df %>% column_to_rownames(var = row_name)
        }
        return(df)
    }
)

#' An S4 class representing a tag.bio flux capacitor (FC)
#'
#' The FC class represents a tag.bio flux capacitor (FC). A FC
#' object has a name and API key which are required for interacting
#' with the FC through API calls.
#'
#' @slot name name of the FC
#' @slot url URL of the FC.  Should end with a forward slash.
#' @slot api_key API key of the FC (if not open)
#' @examples
#' @export FC
#' @exportClass FC
FC <- setClass(
  "FC",
  representation(name = "character",
                 url = "character",
                 api_key = "character"),
  prototype(name = "", url = "", api_key = "")
)


#' Retrieve data from a tag.bio flux capacitor
#'
#' @param fc flux capacitor object
#' @param protocol_instance protocol instance
#' @param script script instance
#' @return A tagbio.data object with data.frame populated by query
#' @export
#' @seealso \code{\link{TagbioResult}},\code{\link{FC}},\code{\link{ProtocolInstance}}
#' @examples

getTagData <- function(fc, protocol_instance = NULL, script = NULL,
                       username = NULL, password = NULL) {

    print("Getting tag data")

    # if no protocol or script, we return an empty TagbioData
    if (is.null(protocol_instance) && is.null(script)) {
        tag_data <- TagbioData()
        return(tag_data)
    }

    # create payload for request
    jsonPayload <- list(
        zip = TRUE,
        api_key = fc@api_key,
        groups = c("developer")
    )

    if (!is.null(protocol_instance)) {
        jsonPayload$protocol_instance = protocol_instance
    }

    if (!is.null(script)) {
        jsonPayload = script
        jsonPayload$api_key = fc@api_key
    }

    # add query to url
    # TODO - check for trailing slash
    url <- paste0(fc@url, "q")

    r <- NA
    if (!is.null(username) && !is.null(password)) {
      # submit request to FC with authentication
      r <- httr::POST(url,
                body = jsonPayload,
                #httr::verbose(),
                httr::authenticate(username, password, type = "basic"),
                encode = "json")
    } else {
      # no authentication
      r <- httr::POST(url,
                      body = jsonPayload,
                      encode = "json")
    }
    tag_data_frame <- httr::content(r, as = "parsed", type = "text/csv",
                              encoding = "UTF-8")

    # set up the tagbio.data instance
    tag_data <- TagbioData(data.frame = tag_data_frame)
    return(tag_data)
}

#' An S4 class representing data to return to the tag.bio platform
#'
#' The TagbioResult class allows the user to return data to the tag.bio
#' platform.  It is typically only used in user-defined protocol methods
#' called from the flux capacitor.
#'
#' Results are typically stored in a file either as a data frame or a
#' plot saved in a graphics format or HTML.  Any messages not part of
#' the results can be placed in the message file.
#'
#' A TagbioResults is typically prepopulated and passed to a user-
#' defined R function.  The type and paths direct the user what output
#' is expected and where results should be saved.
#'
#' @slot result_data a dataframe of results
#' @slot output_path path to resullts file
#' @slot message_path path to a message file
#' @slot result_type format of the results (html, png, pdf)
#' @export TagbioResult
#' @exportClass TagbioResult
TagbioResult <- setClass(
  "TagbioResult",
  representation(result_data = "data.frame",
                 output_path = "character",
                 message_path = "character",
                 result_type = "character"),
  prototype(result_data = data.frame(), output_path = "", message_path = "", result_type = "")
)

#' Add data to a TagbioResult.
#'
#' This method adds results to a TagbioResult object
#'
#' @param tag_result tagResult object
#' @param result_data data.frame or file path to results data
#' @param result_type either \code{data.frame}, \code{jpeg}, \code{pdf},
#'    \code{png}, \code{svg}
#'
#' @return updated TagbioResult object
#' @export
#' @seealso \code{\link{TagbioData}}
#' @examples
setGeneric(
    "addResult",
    def=function(tag_result, result_data, result_type)
        {
            standardGeneric("addResult")
    }
)

setMethod(
    "addResult",
    signature = "TagbioResult",
    function(tag_result, result_data, result_type) {
        if (!(result_type %in% c("data.frame", "html", "jpeg", "pdf", "png", "json"))) {
            stop("Not an expected result_type")
        }

        # TODO:  Add type checks...
        if (result_type == "data.frame") {
            tag_result@data.frame = result_data
        }

        if (result_type == "jpeg") {
            tag_result@jpeg = result_data
        }

        if (result_type == "pdf") {
            tag_result@pdf = result_data
        }

        if (result_type == "png") {
            tag_result@png = result_data
        }

        if (result_type == "svg") {
            tag_result@svg = result_data
        }

        if (result_type == "html") {
            tag_result@html = result_data
        }

        return(tag_result)
    }
)
