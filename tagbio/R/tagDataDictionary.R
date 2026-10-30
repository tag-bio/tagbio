# Compound names ({"operator": " | ", "names": [...]}) display as their parts joined by the operator.
flux_name <- function(n) {
  if (is.list(n) && !is.null(n$names)) {
    paste(vapply(n$names, flux_name, character(1)), collapse = n$operator)
  } else {
    as.character(n)
  }
}

#' Describe a data product's data model
#'
#' One row per collection: how to select it, what it arrives as in a pull, how many entities have a value,
#' its collection sets and, for small categoricals, its values. The same model the FC's MCP endpoint serves
#' as fc://data-model, as a data frame. Only metadata is fetched, never entity rows.
#'
#' @param fc a tagFC (from \code{tbl(con)} or \code{tbl(con, "fc-name")})
#' @param collections optional character vector of collection names to describe
#' @param collection_set optional collection set name to describe
#' @param variables if TRUE, return one row per variable (numeric variable or categorical value) instead; needs
#'   \code{collections} or \code{collection_set}, since some data products have millions of variables
#' @param max_levels categoricals with at most this many values list them in \code{levels}; numerics with at most
#'   this many variables list their column names in \code{column} (larger ones show the name pattern)
#' @param max_sets collection set membership is filled in when the product has at most this many sets
#' @return a tibble; the product's name, title, entity names and entity count are in \code{attr(x, "data_product")}
#' @examples
#' \dontrun{
#' con <- tagConnect()
#' dd <- tagDataDictionary(tbl(con, "fc-clinic"))
#' tagDataDictionary(tbl(con, "fc-clinic"), collections = "Blood Pressure", variables = TRUE)
#' }
#' @export
tagDataDictionary <- function(fc, collections = NULL, collection_set = NULL, variables = FALSE,
                              max_levels = 20, max_sets = 500) {
  if (isTRUE(variables) && is.null(collections) && is.null(collection_set)) {
    stop("variables = TRUE needs collections or collection_set: some data products have millions of variables.",
         call. = FALSE)
  }
  query <- function(script) {
    api_post(fc$con, "q", fc$url, "json",
             list(zip = TRUE, groups = c("developer"), script = script))$results
  }
  value_of <- function(x, name, default = NA) if (is.null(x[[name]])) default else x[[name]]
  info <- info(fc)
  product <- list(name = value_of(info, "name"), title = value_of(info, "title"),
                  entity_name_singular = value_of(info, "entity_name_singular"),
                  entity_name_plural = value_of(info, "entity_name_plural"),
                  entity_count = suppressWarnings(as.numeric(value_of(info, "entity_count"))))

  # ---- collections
  script <- list(method = "collection", limit = 1000000000)
  if (!is.null(collection_set)) {
    script$analysis_variables <- list(to_json(collection_set(collection_set)))
  }
  rows <- lapply(query(script), function(r) {
    v <- r$values
    coll <- v$collection
    tibble::tibble(collection = flux_name(coll$name),
                   data_type = value_of(v, "data_reference_type", value_of(v, "data_function_type")),
                   single_valued = isTRUE(coll$single_valued),
                   logical = isTRUE(coll$logical),
                   date_kind = value_of(coll, "date_kind", NA_character_),
                   n_values = as.numeric(value_of(v, "collection-size")),
                   n_entities = as.numeric(value_of(v, "collection-entity-count")))
  })
  dd <- if (length(rows)) dplyr::bind_rows(rows) else
    tibble::tibble(collection = character(), data_type = character(), single_valued = logical(),
                   logical = logical(), date_kind = character(), n_values = numeric(), n_entities = numeric())
  if (!is.null(collections)) {
    missing <- setdiff(collections, dd$collection)
    if (length(missing)) warning("No collection named: ", paste(missing, collapse = ", "), call. = FALSE)
    dd <- dd[dd$collection %in% collections, , drop = FALSE]
  }

  # ---- variables of small collections (levels, numeric column names), or everything in scope when variables = TRUE
  want <- if (isTRUE(variables)) rep(TRUE, nrow(dd)) else
    dd$data_type %in% c("categorical", "numeric") & dd$n_values <= max_levels
  vars <- tibble::tibble(collection = character(), variable = character(), data_type = character(),
                         n_entities = numeric(), date_kind = character())
  if (any(want) && (isTRUE(variables) || sum(dd$n_values[want]) <= 100000)) {
    fns <- lapply(which(want), function(i) list(data_function_type = dd$data_type[i], collection = dd$collection[i]))
    res <- query(list(method = "variable", variables = fns, limit = 1000000000))
    vars <- dplyr::bind_rows(lapply(res, function(r) {
      v <- r$values
      tibble::tibble(collection = flux_name(v$collection$name),
                     variable = flux_name(v$variable$name),
                     data_type = value_of(v, "data_reference_type"),
                     n_entities = as.numeric(value_of(v, "variable-size")),
                     date_kind = value_of(v$variable, "date_kind", NA_character_))
    }))
  }
  column_name <- function(collection, variable, data_type) {
    ifelse(data_type == "categorical" | variable == collection, collection, paste0(collection, " = ", variable))
  }

  if (isTRUE(variables)) {
    out <- vars %>%
      dplyr::mutate(column = column_name(collection, variable, data_type)) %>%
      dplyr::select(collection, variable, column, data_type, n_entities, date_kind) %>%
      dplyr::arrange(collection, dplyr::desc(n_entities))
    attr(out, "data_product") <- product
    return(out)
  }

  # ---- per-collection summaries from the variables
  numeric_vars <- vars[vars$data_type == "numeric", , drop = FALSE]
  columns <- tapply(column_name(numeric_vars$collection, numeric_vars$variable, numeric_vars$data_type),
                    numeric_vars$collection, function(x) paste(sort(x), collapse = "; "))
  numeric_dates <- tapply(!is.na(numeric_vars$date_kind), numeric_vars$collection, any)
  cat_vars <- vars[vars$data_type == "categorical", , drop = FALSE]
  levels <- tapply(cat_vars$variable, cat_vars$collection, function(x) paste(sort(x), collapse = "; "))

  # ---- collection set membership (one members query per set)
  set_names <- tryCatch(tagListCollectionSets(fc), error = function(e) character())
  sets <- setNames(rep(NA_character_, nrow(dd)), dd$collection)
  if (length(set_names) && length(set_names) <= max_sets) {
    sets[] <- ""
    for (s in set_names) {
      members <- intersect(tagListCollectionSetMembers(fc, s)$collection, dd$collection)
      sets[members] <- ifelse(sets[members] == "", s, paste(sets[members], s, sep = "; "))
    }
  } else if (length(set_names) > max_sets) {
    message("This data product has ", length(set_names), " collection sets (more than max_sets = ", max_sets,
            "): collection_sets left empty. Use collection_set = to describe one set.")
  }

  out <- dd %>%
    dplyr::mutate(
      column = dplyr::case_when(
        data_type != "numeric" ~ collection,
        collection %in% names(columns) ~ as.character(unname(columns[collection])),
        TRUE ~ paste0(collection, " = <variable> (", format(n_values, big.mark = ",", trim = TRUE),
                      ifelse(n_values == 1, " variable)", " variables)"))),
      kind = dplyr::case_when(logical ~ "logical",
                              !is.na(date_kind) ~ "date",
                              data_type == "categorical" ~ "text",
                              collection %in% names(numeric_dates)[numeric_dates] ~ "date",
                              TRUE ~ "number"),
      single_valued = ifelse(data_type == "categorical", single_valued, NA),
      pct_entities = round(100 * n_entities / product$entity_count, 1),
      levels = ifelse(collection %in% names(levels), unname(levels[collection]), NA_character_),
      collection_sets = unname(sets[collection])) %>%
    dplyr::select(collection, column, data_type, kind, single_valued, n_values, n_entities, pct_entities,
                  levels, collection_sets) %>%
    dplyr::arrange(collection)
  attr(out, "data_product") <- product
  out
}
