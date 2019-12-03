
library("httr")

#### REMOVE THIS:
# API_KEY = "v52tngmq20rem90a1skbevghkq"
# args = list(expression = c("A1CF","A2M","A2M-AS1","A2ML1","A4GNT","AA"),
# clinical_categorical_variables = c("clinical.SAMPLE_ID","clinical.PATIENT_ID"))
# FC_URL = ""https://fc-skcm.fc.tag.bio/q"
# PROTOCOL_NAME = "download"

#' An S4 class representing data from the tag.bio platform
#'
#' The tagbio.data class holds tagbio.data as a data.frame attribute of the
#' object.  tagbio.data objects are typically created through a
#' \code{getTagData} query or created directly from the flux capacitor and
#' passed to a user defined method.  The tagbio.data object may optionally
#' have a list of parameters defined.  This list is typically populated by
#' the flux capacitor and is used to direct user defined methods.
#'
#' @slot data.frame a data frame typically retrieved from tag.bio
#' @slot parameters a list of parameters from tag.bio
#' @export
tagbio.data <- setClass(
    "tagbio.data",
    representation(data.frame = "data.frame",
                   parameters = "list"),
    prototype(data.frame = data.frame(), parameters = list())
)




#' Retrieve data from a tag.bio flux capacitor
#'
#' @param fcUrl flux capacitor API URL
#' @param apiKey flux capacitor API key for authentication
#' @param arguments a list of arguments to specify the API query
#' @param protocolName name of protocol.  Default is "download".
#' @return A tagbio.data object with data.frame populated by query
#' @export
#' @seealso \code{\link{tagbio.result}},\code{\link{getTagData}}
#' @examples
#' getTagData(FC_URL, API_KEY, list(expression = c("A1CF","A2M"),
#'     clinical_categorical_variables = c("clinical.SAMPLE_ID")))
#'
getTagData <- function(fcUrl, apiKey, arguments, protocolName = "download") {

    # create payload for request
    jsonPayload <- list(
        zip = TRUE,
        api_key = apiKey,
        groups = c("developer"),
        protocol_instance = list(
           arguments = arguments,
           name = protocolName,
           version = "N/A",
           require_auth = TRUE
        )
    )

    # submit request to FC
    r <- POST(fcUrl,
              body = jsonPayload,
              encode = "json")
    tag_data_frame <- content(r, as = "parsed", type = "text/csv",
                              encoding = "UTF-8")

    # set up the tagbio.data instance
    tag_data <- tagbio.data()
    tag_data@data.frame = tag_data_frame
    return(tag_data)
}

#' An S4 class representing data to return to the tag.bio platform
#'
#' The tagbio.result class allows the user to return data to the tag.bio
#' platform.  It is typically only used in user-defined protocol methods
#' called from the flux capacitor.
#'
#' @slot data.frame a dataframe of results
#' @slot jpeg path to a jpeg file
#' @slot pdf path to a pdf file
#' @slot png path to a png file
#' @slot svg path to a svg file
#' @export
tagbio.result <- setClass(
  "tagbio.result",
  representation(data.frame = "data.frame",
                 jpeg = "character",
                 pdf = "character",
                 png = "character",
                 svg = "character"),
  prototype(data.frame = data.frame(), jpeg = "", pdf = "", png = "", svg = "")
)

#' Add data to a tagbio.result.
#'
#' This method adds results to a tagbio.result object
#'
#' @param tagResult tagResult object
#' @param resultData data.frame or file path to results data
#' @param resultType either \code{data.frame}, \code{jpeg}, \code{pdf},
#'    \code{png}, \code{svg}
#'
#' @return updated tagResult object
#' @export
#' @seealso \code{\link{tagbio.data}}
#' @examples
#' addResult(tagresult, "my_pdf_file.pdf", type = "pdf")
setGeneric(
    "addResult",
    def=function(tagResult, resultData, resultType)
        {
            standardGeneric("addResult")
    }
)

setMethod(
    "addResult",
    signature = "tagbio.result",
    function(tagResult, resultData, resultType) {
        if (!(resultType %in% c("data.frame", "jpeg", "pdf", "png", "svg"))) {
            stop("Not an expected resultType")
        }

        # TODO:  Add type checks...
        if (resultType == "data.frame") {
            tagResult@data.frame = resultData
        }

        if (resultType == "jpeg") {
            tagResult@jpeg = resultData
        }

        if (resultType == "pdf") {
            tagResult@pdf = resultData
        }

        if (resultType == "png") {
            tagResult@png = resultData
        }

        if (resultType == "svg") {
            tagResult@png = resultData
        }

        return(tagResult)
    }
)
