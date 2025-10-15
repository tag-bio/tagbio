# R objects mirroring tag's variables and collections

#' Translate Tag.bio data constructs as JSON
#'
#' The \code{to_json} method converts Tag.bio data constructs to JSON, typically
#' for communicating with the API or creating configuration files.
#'
#' @slot .Object a Tag.bio data object
#'
#' @examples
#' # TODO
#'
#' @export
to_json <- function(.Object, ...) {
  UseMethod("to_json", .Object)
}

#' An S3 class representing the Tag.bio NumericCollection
#'
#'
#' @export
NumericCollection <- function(collection, collection_size, ...) {
  .Object <- list(
    data_function_type = "numeric",
    collection = collection,
    collection_size = collection_size
  )
  class(.Object) <- "NumericCollection"

  .Object
}

#' @export
to_json.NumericCollection <- function(.Object) {
  json <- list()
  json[["data_function_type"]] <- .Object$data_function_type # TODO
  json[["collection"]] <- .Object$collection

  json
}


#' An S3 class representing the Tag.bio CategoricalCollection
#'
#'
#' @export
CategoricalCollection <- function(
  collection,
  collection_size,
  collection_entity_count,
  ...
) {
  .Object <- list(
    data_function_type = "categorical",
    collection = collection,
    collection_size = collection_size,
    collection_entity_count = collection_entity_count
  )
  class(.Object) <- "CategoricalCollection"
  .Object
}

#' @export
to_json.CategoricalCollection <- function(.Object) {
  json <- list()
  json[["data_function_type"]] <- .Object$data_function_type # TODO
  json[["collection"]] <- .Object$collection

  json
}


#' An S3 class representing the Tag.bio NumericVariable
#'
#' @seealso
#' Useful links:
#' \itemize{
#' \item \url{https://code.tag.bio/docs/variable-object-reference#numeric}
#' The Numeric variable data-function from Tag.bio syntax documentation.
#' }
#'
#'
#' @slot collection a numeric collection
#' @slot variable from a numeric variable containing numeric values
#'
#' @examples
#' nvr <- NumericVariable("diamond", "carat")
#'
#' @export
NumericVariable <- function(collection, variable, ...) {
  .Object <- list(
    data_function_type = "numeric",
    collection = collection,
    variable = variable
  )
  class(.Object) <- "NumericVariable"

  .Object
}

#' @export
to_json.NumericVariable <- function(.Object) {
  json <- list()
  json[["data_function_type"]] <- .Object$data_function_type
  json[["collection"]] <- .Object$collection$collection
  json[["variable"]] <- .Object$variable

  json
}


#' An S3 class representing the Tag.bio CategoricalVariable
#'
#' @seealso
#' Useful links:
#' \itemize{
#' \item \url{https://code.tag.bio/docs/variable-object-reference#categorical}
#' The Categorical variable data-function from Tag.bio syntax documentation.
#' }
#'
#'
#' @slot collection a categorical collection
#' @slot variable a categorical variable
#'
#' @examples
#' cvr <- CategoricalVariable("color", "red")
#'
#' @export
CategoricalVariable <- function(collection, variable, ...) {
  .Object <- list(
    data_function_type = "categorical",
    collection = collection,
    variable = variable
  )
  class(.Object) <- "CategoricalVariable"

  .Object
}

#' @export
to_json.CategoricalVariable <- function(.Object) {
  json <- list()
  json[["data_function_type"]] <- .Object$data_function_type
  json[["collection"]] <- .Object$collection
  json[["variable"]] <- .Object$variable

  json
}


#' An S3 class representing the Tag.bio NumericSlice
#'
#' @seealso
#' Useful links:
#' \itemize{
#' \item \url{https://code.tag.bio/docs/variable-object-reference#numericslice}
#' The Numeric-Slice data-function from Tag.bio syntax documentation.
#' }
#'
#'
#' @slot criterion a numeric variable being compared
#' @slot operator "<", "<=", "==", "!=", ">=", ">"
#' @slot value numeric comparator
#' @slot percentile percentile comparator
#'
#' @examples
#' ns <- NumericSlice("carat", ">", 0.3)
#'
#' @export
NumericSlice <- function(
  criterion,
  operator,
  value = numeric(),
  percentile = numeric(),
  ...
) {
  .Object <- list(
    data_function_type = "numeric-slice",
    operator = operator,
    criterion = criterion,
    value = value,
    percentile = percentile
  )
  class(.Object) <- "NumericSlice"

  .Object
}

#' @export
to_json.NumericSlice <- function(.Object) {
  json <- list()
  json[["operator"]] <- .Object$operator
  json[["value"]] <- .Object$value
  json[["data_function_type"]] <- .Object$data_function_type

  criterion <- list()
  tagvar <- .Object$criterion
  criterion[["collection"]] <- tagvar$collection$collection
  criterion[["variable"]] <- tagvar$variable
  criterion[["data_function_type"]] <- tagvar$data_function_type
  json[["criterion"]] <- criterion

  json
}


#' An S3 class representing the Tag.bio CategoricalBatch
#'
#' Represents the intersection or union of variables within one collection.
#'
#' @seealso
#' Useful links:
#' \itemize{
#' \item \url{https://code.tag.bio/docs/variable-object-reference}
#' The Categorical-Batch data-function from Tag.bio syntax documentation.
#' }
#'
#'
#' @slot collection a categorical collection
#' @slot operator "AND", "OR"
#' @slot variables list of variables to be combined
#'
#' @examples
#' cb <- CategoricalBatch("Title", "OR", c("The Matrix", "Casablanca"))
#'
#' @export
CategoricalBatch <- function(collection, operator, variables, ...) {
  .Object <- list(
    data_function_type = "categorical-batch",
    operator = operator,
    collection = collection,
    variables = variables
  )
  class(.Object) <- "CategoricalBatch"

  .Object
}

#' @export
to_json.CategoricalBatch <- function(.Object) {
  json <- list()
  json[["operator"]] <- .Object$operator
  json[["collection"]] <- .Object$collection$collection
  json[["variables"]] <- .Object$variables
  json[["data_function_type"]] <- .Object$data_function_type

  json
}


#' An S3 class representing the Tag.bio CategoricalCompound
#'
#' A categorical-compound variable object converts two or more
#' categorical variable objects representing variables into the intersection
#' or the union of those two sets.
#'
#' If the operator attribute is AND, the categorical variable objects
#' in the criteria attribute will be intersected.
#'
#' @seealso
#' Useful links:
#' \itemize{
#' \item \url{https://code.tag.bio/docs/variable-object-reference#categoricalcompound}
#' The Categorical-Compound data-function from Tag.bio syntax documentation.
#' }
#'
#'
#' @slot criteria list of categorical variable objects
#' @slot operator "AND", "OR"
#'
#' @examples
#' cc <- CategoricalCompound(c(), "AND")
#'
#' @export
CategoricalCompound <- function(criteria = list(), operator, ...) {
  .Object <- list(
    data_function_type = "categorical-compound",
    operator = operator,
    criteria = criteria
  )
  class(.Object) <- "CategoricalCompound"

  .Object
}

#' @export
to_json.CategoricalCompound <- function(.Object) {
  json <- list()
  json[["operator"]] <- .Object$operator
  json[["data_function_type"]] <- .Object$data_function_type
  crit_list <- list()
  cnt <- 1

  for (crit in .Object$criteria) {
    crit_list[[cnt]] <- to_json(crit)
    cnt <- cnt + 1
  }
  json[["criteria"]] <- crit_list

  json
}
