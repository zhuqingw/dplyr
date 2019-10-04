#' Select and rename a selection of variables
#'
#' @description
#'
#' These [scoped] variants of [select()] and [rename()] operate on a
#' selection of variables. The semantics of these verbs have subtle
#' but important differences:
#'
#' * Selection drops variables that are not in the selection while
#'   renaming retains them.
#'
#' * The renaming function is optional for selection but not for
#'   renaming.
#'
#' The `_if` and `_at` variants always retain grouping variables for grouped
#' data frames.
#'
#' @inheritParams scoped
#' @param .funs A function `fun`, a purrr style lambda `~ fun(.)` or a list of either form.
#'
#' @section Grouping variables:
#'
#' Existing grouping variables are always kept in the data frame, even
#' if not included in the selection.
#'
#' @examples
#'
#' # Supply a renaming function:
#' select_all(mtcars, toupper)
#' select_all(mtcars, "toupper")
#' select_all(mtcars, list(~toupper(.)))
#'
#' # Selection drops unselected variables:
#' is_whole <- function(x) all(floor(x) == x)
#' select_if(mtcars, is_whole, toupper)
#' select_at(mtcars, vars(-contains("ar"), starts_with("c")), toupper)
#'
#' # But renaming retains them:
#' rename_if(mtcars, is_whole, toupper)
#' rename_at(mtcars, vars(-(1:3)), toupper)
#' rename_all(mtcars, toupper)
#'
#' # The renaming function is optional for selection:
#' select_if(mtcars, is_whole)
#' select_at(mtcars, vars(-everything()))
#' select_all(mtcars)
#' @export
select_all <- function(.tbl, .funs = list(), ...) {
  funs <- as_fun_list(.funs, caller_env(), ...)
  vars <- tbl_vars(.tbl)
  vars_inds <- seq_along(vars)
  inds <- vars_select_inds(vars, funs, .tbl, inds = vars_inds)
  select(.tbl, !!inds)
}
#' @rdname select_all
#' @export
rename_all <- function(.tbl, .funs = list(), ...) {
  funs <- as_fun_list(.funs, caller_env(), ...)
  vars <- tbl_vars(.tbl)
  vars_inds <- seq_along(vars)
  inds <- vars_select_inds(vars, funs, .tbl, strict = TRUE, inds = vars_inds)
  rename(.tbl, !!!inds) # FIXME: Use `!!` with tidyselect 0.3.0
}

#' @rdname select_all
#' @export
select_if <- function(.tbl, .predicate, .funs = list(), ...) {
  funs <- as_fun_list(.funs, caller_env(), ...)
  if (!is_logical(.predicate)) {
    .predicate <- as_fun_list(.predicate, caller_env())
  }
  vars <- tbl_if_vars(.tbl, .predicate, caller_env(), .include_group_vars = TRUE)
  inds <- vars_select_inds(vars, funs, .tbl)
  select(.tbl, !!inds)
}
#' @rdname select_all
#' @export
rename_if <- function(.tbl, .predicate, .funs = list(), ...) {
  funs <- as_fun_list(.funs, caller_env(), ...)
  if (!is_logical(.predicate)) {
    .predicate <- as_fun_list(.predicate, caller_env())
  }
  vars <- tbl_if_vars(.tbl, .predicate, caller_env(), .include_group_vars = TRUE)
  inds <- vars_select_inds(vars, funs, .tbl, strict = TRUE)
  rename(.tbl, !!!inds) # FIXME: Use `!!` with tidyselect 0.3.0
}

#' @rdname select_all
#' @export
select_at <- function(.tbl, .vars, .funs = list(), ...) {
  vars <- tbl_at_vars(.tbl, .vars, .include_group_vars = TRUE)
  funs <- as_fun_list(.funs, caller_env(), ...)
  inds <- vars_select_inds(vars, funs, .tbl)
  select(.tbl, !!inds)
}
#' @rdname select_all
#' @export
rename_at <- function(.tbl, .vars, .funs = list(), ...) {
  vars <- tbl_at_vars(.tbl, .vars, .include_group_vars = TRUE)
  funs <- as_fun_list(.funs, caller_env(), ...)
  inds <- vars_select_inds(vars, funs, .tbl, strict = TRUE)
  rename(.tbl, !!!inds) # FIXME: Use `!!` with tidyselect 0.3.0
}

vars_select_inds <- function(vars, funs, tbl, strict = FALSE, inds = NULL) {
  if (length(funs) > 1) {
    bad_args(".funs", "must contain one renaming function, not {length(funs)}")
  }
  if (length(funs) == 0 && strict) {
    bad_args(".funs", "must specify a renaming function")
  }

  tbl_vars <- tbl_vars(tbl)
  if (is.null(inds)) {
    # This `match()` call doesn't handle duplicates in `vars` properly
    inds <- match(vars, tbl_vars)
  }

  if (length(funs) == 1) {
    fun <- funs[[1]]
    if (is_quosure(fun)) {
      fun <- quo_as_function(fun)
    }
    if (length(vars)) {
      inds <- set_names(inds, fun(as.character(vars)))
    }
  }

  group_vars <- group_vars(tbl)
  group_inds <- match(group_vars, tbl_vars)

  has_group <- group_inds %in% inds
  new_group_inds <- set_names(group_inds[!has_group], group_vars[!has_group])

  c(new_group_inds, inds)
}
