library(tidyverse)
library(rlang)

# R objects mirroring tag's variables and collections
#' @export

setGeneric(
  "to_json",
  def = function(.Object) {
    standardGeneric("to_json")
  }
)

#' An S4 class representing tag.bio NumericCollection
#'
#'
#' @export NumericCollection
#' @exportClass NumericCollection

NumericCollection <- setClass(
  "NumericCollection",
  slots = c(collection = "character", data_function_type = "character", collection_size = "numeric")
)

setMethod("initialize", "NumericCollection",
          function(.Object, collection,  collection_size, ...) {
            .Object@data_function_type <- "numeric"
            .Object@collection <- collection
            .Object@collection_size <- collection_size
            return(.Object)
          }
)

setMethod("to_json", "NumericCollection",
          function(.Object) {
            json <- list()
            json[['variable_type']] <- .Object@data_function_type # TODO
            json[['collection']] <- .Object@collection
            return(json)
          }
)


#' An S4 class representing tag.bio CategoricalCollection
#'
#'
#' @export CategoricalCollection
#' @exportClass CategoricalCollection

CategoricalCollection <- setClass(
  "CategoricalCollection",
  slots = c(collection = "character", data_function_type = "character",
            collection_size = "numeric", collection_entity_count = "numeric")
)

setMethod("initialize", "CategoricalCollection",
          function(.Object, collection, collection_size, collection_entity_count, ...) {
            .Object@data_function_type <- "categorical"
            .Object@collection <- collection
            .Object@collection_size <- collection_size
            .Object@collection_entity_count <- collection_entity_count
            return(.Object)
          }
)

setMethod("to_json", "CategoricalCollection",
          function(.Object) {
            json <- list()
            json[['variable_type']] <- .Object@data_function_type
            json[['collection']] <- .Object@collection
            return(json)
          }
)


#' An S4 class representing tag.bio NumericVariable
#'
#'
#' @export NumericVariable
#' @exportClass NumericVariable

NumericVariable <- setClass(
  "NumericVariable",
  slots = c(collection = "NumericCollection", data_function_type = "character", variable = "character")
)

setMethod("initialize", "NumericVariable",
          function(.Object, collection, variable, ...) {
            .Object@data_function_type <- "numeric"
            .Object@collection <- collection
            .Object@variable <- variable
            return(.Object)
          }
)

setMethod("to_json", "NumericVariable",
          function(.Object) {
            json <- list()
            json[['variable_type']] <- .Object@data_function_type
            json[['collection']] <- .Object@collection@collection
            json[['variable']] <- .Object@variable
            return(json)
          }
)

#' An S4 class representing tag.bio CategoricalVariable
#'
#'
#' @export CategoricalVariable
#' @exportClass CategoricalVariable

CategoricalVariable <- setClass(
  "CategoricalVariable",
  slots = c(collection = "CategoricalCollection", data_function_type = "character", variable = "character")
)

setMethod("initialize", "CategoricalVariable",
          function(.Object, collection,  variable, ...) {
            .Object@data_function_type <- "categorical"
            .Object@collection <- collection
            .Object@variable <- variable
            return(.Object)
          }
)

setMethod("to_json", "CategoricalVariable",
          function(.Object) {
            json <- list()
            json[['variable_type']] <- .Object@data_function_type
            json[['collection']] <- .Object@collection
            json[['variable']] <- .Object@variable
            return(json)
          }
)


#' An S4 class representing tag.bio NumericSlice
#'
#'
#' @export NumericSlice
#' @exportClass NumericSlice

NumericSlice <- setClass(
  "NumericSlice",
  slots = c(data_function_type = "character", criterion = "NumericVariable", operator = "character",
            value = "numeric", percentile = "numeric")
)

setMethod("initialize", "NumericSlice",
          function(.Object, criterion, operator,
                   value = numeric(), percentile = numeric(), ...) {
            .Object@data_function_type <- "numeric-slice"
            .Object@operator <- operator
            .Object@criterion <- criterion
            .Object@value <- value
            .Object@percentile <- percentile
            return(.Object)
          }
)


setMethod("to_json", "NumericSlice",
  function(.Object) {
    json <- list()
    json[['operator']] <- .Object@operator
    json[['value']] <- .Object@value
    json[['variable_type']] <- .Object@data_function_type

    criterion <- list()
    tagvar <- .Object@criterion
    criterion[['collection']] <- tagvar@collection@collection
    criterion[['variable']] <- tagvar@variable
    criterion[['variable_type']] <- tagvar@data_function_type
    json[['criterion']] <- criterion
    return(json)
  }
)

#' An S4 class representing tag.bio CategoricalBatch
#'
#'
#' @export CategoricalBatch
#' @exportClass CategoricalBatch

CategoricalBatch <- setClass(
  "CategoricalBatch",
  slots = c(data_function_type = "character",
            collection = "CategoricalCollection",
            operator = "character",
            variables = "list")
)

setMethod("initialize", "CategoricalBatch",
          function(.Object, collection,  operator, variables, ...) {
            .Object@data_function_type <- "categorical-batch"
            .Object@operator <- operator
            .Object@collection <- collection
            .Object@variables <- variables
            return(.Object)
          }
)

setMethod("to_json", "CategoricalBatch",
          function(.Object) {
            json <- list()
            json[['operator']] <- .Object@operator
            json[['collection']] <- .Object@collection@collection
            json[['variable_type']] <- .Object@data_function_type
            json[['variables']] <- .Object@variables
            return(json)
          }
)

#' An S4 class representing tag.bio CategoricalCompound
#'
#'
#' @export CategoricalCompound
#' @exportClass CategoricalCompound

CategoricalCompound <- setClass(
  "CategoricalCompound",
  slots = c(data_function_type = "character",
            criteria = "list",
            operator = "character")
)

setMethod("initialize", "CategoricalCompound",
          function(.Object, criteria = list(),  operator, ...) {
            .Object@data_function_type <- "categorical-compound" # TODO: convert to set-operation
            .Object@operator <- operator
            .Object@criteria <- criteria
            return(.Object)
          }
)

setMethod("to_json", "CategoricalCompound",
          function(.Object) {
            json <- list()
            json[['operator']] <- .Object@operator
            json[['variable_type']] <- .Object@data_function_type # TODO
            crit_list = list()
            cnt <- 1
            for (crit in .Object@criteria) {
              crit_list[[cnt]] <- to_json(crit)
              cnt <- cnt + 1
            }
            json[['criteria']] <- crit_list

            return(json)
          }
)
