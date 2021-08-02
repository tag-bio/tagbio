
# Converts dplyr to tagbio.R object representation

# Following code comes from Wickham Advanced R book
# walk AST
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

flat_map_chr <- function(.x, .f, ...) {
  purrr::flatten_chr(purrr::map(.x, .f, ...))
}

all_names_rec <- function(x) {
  switch_expr(x,
              constant = character(),
              symbol =   as.character(x),
              call =     flat_map_chr(as.list(x[-1]), all_names)
  )
}

all_names <- function(x) {
  unique(all_names_rec(x))
}

check_variable <- function(collection_defs, collection_variable) {
  # if name split into collection and variable based on delimiter,
  # then assume it's a valid variable
  if (length(collection_variable) == 2) {
    collection_name <- collection_variable[1]
    variable_name <- collection_variable[2]
    if (collection_name %in% names(collection_defs)) {
      collection <- collection_defs[[collection_name]]
      tag_var <- switch(collection$data_function_type,
                        categorical = CategoricalVariable(collection = collection,
                                                          variable = variable_name),
                        numeric = NumericVariable(collection = collection,
                                                  variable = variable_name))
      return(tag_var)
    }
  } else {
    collection_defs[[collection_variable]]
  }
}


#----------------------------------------------------------
# NumericSlice

comparator_opposites <- list(
  ">" = "<",
  ">=" = "<=",
  "<" = ">",
  "<=" = ">=",
  "==" = "=="
)

tag_numeric_slice_op <- function(op) {

  # handle numeric and cat
  rlang::new_function(
    rlang::exprs(e1 = , e2 = ),
    rlang::expr(
      # test signature - if we don't handle the pass to parent environment
      if (is(e1, "NumericVariable")) {
        if (is(e2, "numeric")) {
          print("SLICE YES")
          NumericSlice(criterion = e1, operator = !!op, value = e2)
        } else {
          # if e2 is another type, raise error for now... more work to do...
          stop(paste("ERROR: Not supported for operator", !!op,
                     "arguments NumericVariable,", is(e2)))
        }
      } else {
        if (is(e2, "NumericVariable")) {
          if (is(e1, "numeric")) {
            # reverse the comparator
            NumericSlice(criterion = e2,
                         operator = comparator_opposites[[!!op]],
                         value = e1)
          } else {
            # if e1 is another type, raise error for now... more work to do...
            stop(paste("ERROR: Not supported for operator", !!op,
                       "arguments", is(e1) ," and NumericVariable,"))
          }
        } else {
          # use the parent evaluation
          (!!op)(e1, e2)
        }
      }
    ),
    rlang::caller_env()
  )
}

# set up environment of known functions
tag_numeric_func_list <- list(
  `>` = tag_numeric_slice_op(">"),
  `>=` = tag_numeric_slice_op(">="),
  `<` = tag_numeric_slice_op("<"),
  `<=` = tag_numeric_slice_op("<="),
  `==` = tag_numeric_slice_op("==")
)

#----------------------------------------------------------
# CategoricalBatch

tag_categorical_batch_op <- function(op, batch_op) {

  # handle categorical batch operators
  rlang::new_function(
    rlang::exprs(e1 = , e2 = ),
    rlang::expr(
      # test signature - if we don't handle the pass to parent environment
      if (is(e1, "CategoricalCollection")) {
        if (is(e2, "numeric") | is(e2, "character")) {
          #if (length(e2) == 1) {
          #  # variables should always be a list
          #  e2 <- list(e2)
          #}
          CategoricalBatch(collection = e1, operator = !!batch_op,
                           variables = list(e2))
        } else {
          # if e2 is another type, raise error for now... more work to do...
          stop(paste("ERROR: Not supported for operator", !!op,
                     "arguments CategoricalCollection,", is(e2)))
        }
      } else {
        if (is(e2, "CategoricalCollection")) {
          if (is(e1, "numeric") | is(e1, "character")) {
            #if (length(e1) == 1) {
            #  # variables should always be a list
            #  e1 <- list(e1)
            #}
            CategoricalBatch(collection = e2, operator = !!batch_op,
                             variables = list(e1))
          } else {
            # if e1 is another type, raise error for now... more work to do...
            stop(paste("ERROR: Not supported for operator", !!op,
                       "arguments ", is(e1), " CategoricalCollection,"))
          }
        } else {
          # use the parent evaluation
          (!!op)(e1, e2)
        }
      }
    ),
    rlang::caller_env()
  )
}

# set up environment of known functions
tag_categorical_func_list <- list(
  `%in%` = tag_categorical_batch_op("%in%", "OR"),
  `==` = tag_categorical_batch_op("==", "OR"),
  `%all_in%` = tag_categorical_batch_op("%all_in%", "AND")
)


# From all symbols, identify those that correspond to FC collection or variable
# if a symbol has the delimiter in it then we check that
tag_env <- function(fc, expr) {
  names <- all_names(expr)
  delim <- fc$qdelim

  # functions...
  fn_env <- rlang::as_environment(tag_numeric_func_list, rlang::caller_env())
  fc_env <- rlang::as_environment(tag_categorical_func_list, fn_env)

  # check names for variables and add to environment
  tag_vars <- lapply(str_split(names, delim),
                     function(x) { check_variable(get_collection_defs(fc), x) })
  print(tag_vars)
  tag_vars <- purrr::set_names(tag_vars, names)
  #tag_vars <- tag_vars[lengths(tag_vars) != 0] # drops empty entries
  tag_vars <- rlang::as_environment(tag_vars, parent = fc_env)

  #tag_collections <- as_environment(fc@collection_defs)
  #env_clone(tag_vars, parent = fc_env)
  rlang::env_clone(tag_vars)

}

# method takes R expressions and converts to tag objects ready for filter queries

#' @export
to_tag <- function(fc, x) {
  expr <- enexpr(x)
  out <- rlang::eval_bare(expr, tag_env(fc = fc, expr))
  return(out)
}

#-----------------------------------------------------------------
# select mini-language

# Get tag variable names present in select statement

tag_select_names <- function(x) {
  if (is.atomic(x)) {
    character()
  } else if (is.name(x)) {
    as.character(x)
  } else if (is.call(x) || is.pairlist(x)) {
    if (x[1] == 'all_of()') {
      eval(x[[2]])
    } else {
      children <- lapply(x[-1], all_names)
      unique(unlist(children))
    }
  } else {
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}

#' @export
tag_select_eval <- function(fc, ...) {

  names <- unlist(map(rlang::exprs(...), tag_select_names))
  delim <- fc$qdelim

  # check names for variables and add to environment
  tag_vars <- lapply(str_split(names, delim),
                     function(x) { check_variable(get_collection_defs(fc), x) })

  tag_vars <- purrr::set_names(tag_vars, names)
  tag_vars <- tag_vars[lengths(tag_vars) != 0] # drops empty entries
  sim_data <- tibble::as_tibble(rlang::rep_named(names(tag_vars), list(logical())))

  loc <- tidyselect::eval_select(rlang::expr(c(...)), sim_data)
  return(tag_vars[loc])
}



