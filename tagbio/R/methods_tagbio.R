
library("httr")

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
#' fc_hnsc <- FC(name = "fc-hnsc", 
#'      url = "https://fc-genesig-gateway-azure.dev.tag.bio/fc-svc/fc-hnsc/")
#' @export FC
#' @exportClass FC
FC <- setClass(
  "FC",
  representation(name = "character",
                 url = "character",
                 api_key = "character"),
  prototype(name = "", url = "", api_key = "")
)

#' An S4 class representing a tag.bio protocol instance.
#'
#' The ProtocolInstance class represents a tag.bio protocol.  The
#' object has the protocol name and specifies what data should
#' be downloaded from the protocol through the API query.
#'
#' @slot name name of the protocol (default 'download')
#' @slot arguments a list of arguments to specify the API query
#' @slot version protocol version (default 'N/A')
#' @slot require_auth protocol requires athetication (default FALSE)
#' @examples
#' protocol_instance <- ProtocolInstance(name = "download", 
#'      arguments = list(expression = c("A1CF","A2M"),
#'      clinical_categorical_variables = c("clinical.SAMPLE_ID")))
#' @export ProtocolInstance
#' @exportClass ProtocolInstance
ProtocolInstance <- setClass(
  "ProtocolInstance",
  representation(name = "character",
                 arguments = "list",
                 version = "character",
                 require_auth = "logical"),
  prototype(name = "download", arguments = list(), version = "N/A", require_auth = FALSE)
)
setGeneric("as.list")
setMethod("as.list", c(x = "ProtocolInstance"), function(x) {
  return(list(name = x@name, arguments = x@arguments, version = x@version, require_auth = x@require_auth))
})

#' Retrieve data from a tag.bio flux capacitor
#'
#' @param fc flux capacitor object
#' @param protocol_instance protocol instance
#' @return A tagbio.data object with data.frame populated by query
#' @export
#' @seealso \code{\link{TagbioResult}},\code{\link{FC}},\code{\link{ProtocolInstance}}
#' @examples
#' fc_hnsc <- FC(name = "fc-hnsc", 
#'      url = "https://fc-genesig-gateway-azure.dev.tag.bio/fc-svc/fc-hnsc/")
#' protocol_instance <- ProtocolInstance(name = "download", 
#'      arguments = list(expression = c("A1CF","A2M"),
#'      clinical_categorical_variables = c("clinical.SAMPLE_ID")))
#' getTagData(fc_hnsc, protocol_instance)
#'
getTagData <- function(fc, protocol_instance) {

    # create payload for request
    jsonPayload <- list(
        zip = TRUE,
        api_key = fc@api_key,
        groups = c("developer"),
        protocol_instance = as.list(protocol_instance)
    )

    # add query to url
    # TODO - check for trailing slash
    url <- paste0(fc@url, "q")

    # submit request to FC
    r <- httr::POST(url,
              body = jsonPayload,
              encode = "json")
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
#' @slot data.frame a dataframe of results
#' @slot jpeg path to a jpeg file
#' @slot pdf path to a pdf file
#' @slot png path to a png file
#' @slot svg path to a svg file
#' @export TagbioResult
#' @exportClass TagbioResult
TagbioResult <- setClass(
  "TagbioResult",
  representation(data.frame = "data.frame",
                 jpeg = "character",
                 pdf = "character",
                 png = "character",
                 svg = "character"),
  prototype(data.frame = data.frame(), jpeg = "", pdf = "", png = "", svg = "")
)

#' Add data to a TagbioResult.
#'
#' This method adds results to a tagbio.result object
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
#' tag_result <- TagbioResult()
#' addResult(tag_result, "my_pdf_file.pdf", result_type = "pdf")
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
        if (!(result_type %in% c("data.frame", "jpeg", "pdf", "png", "svg"))) {
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
            tag_result@png = result_data
        }

        return(tag_result)
    }
)
