library("tidyr")
library("dplyr")

#' Create a tibble from all combinations of inputs
#'
#' @description
#' Currently `grid` is simply an alias to `tidyr::expand_grid`.
#' It is designed to work with `fork::fork` to produce a multiverse specification.
#'
#' @param ... Name-value pairs. The name will become the column name in the output.
#'
#' @return A tibble
#' @export
grid <- expand_grid

# useful for partial grid expansion, only when certain conditions are met, e.g.
#
#   tibble(major=c(1,2,3)) %>% fork(major=2, minor=c('2.1', '2.2'))

#' Create a tibble from partial combinations of inputs
#'
#' @param .left A tibble as created with `grid`, `tibble`, `tribble` or a previous call to `fork`
#' @param ... Name-value pairs to produce a righthand tibble that will be crossed with the lefthand one.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' tibble(analysis = c("lm", "glm")) %>% fork(analysis = "glm", family = c("logit", "probit"))
fork <- function(.left, ...) {
  .right <- grid(...)
  if (length(intersect(colnames(.left), colnames(.right)))) {
    left_join(.left, .right, relationship = "many-to-many")
  } else {
    cross_join(.left, .right)
  }
}

na_replacements <- list(
  character = "",
  numeric = 0,
  logical = FALSE
)

#' Replace NAs with falsey values, depending on the type of vector
#'
#' @param data A vector.
#'
#' @description
#' To state the obvious, it is not in general a good idea to replace NA values
#' with FALSE or its equivalents, but in the context of a multiverse scenario,
#' NA is meant to convey that the parameter does not apply and so its value is
#' irrelevant.
#'
#' Replacing NA with falsey values in scenarios allows for much easier filtering:
#'
#'   * `is.na(x) | !x)` can be replaced with `!x`
#'   * `!is.na(x) & x == 'abc'` can be replaced with `x == 'abc'`
#'
#' `replace_na_with_false` must be called explicitly, it is not a part of the
#' behavior of `fork`
#'
#' @export
replace_na_with_false <- function(data) {
  structure <- map_chr(data, \(x) class(x)[1])
  replacements <- na_replacements[structure]
  names(replacements) <- colnames(data)
  replace_na(data, replacements)
}


# figure out to what extent intermediate results from the previous scenario
# can be reused (by grabbing them from a cache); note that planning must
# occur on the final list of scenarios, *after* any filtering, because
# it requires intact (prev, cur) scenario pairs, but *before* any
# column packing; this also implies that if we do any row-level caching
# and then resume from somewhere, we need to make a new plan before
# resuming

#' Generate caching hints by analyzing when subsequent scenarios diverge.
#'
#' @param scenarios A tibble with scenarios, usually generated with `grid` and/or `fork`
#'
#' @return An integer vector with cache invalidation positions.
#' @export
plan_cache <- function(scenarios) {
  scenarios_chr <- scenarios |>
    mutate(across(everything(), as.character)) |>
    mutate(across(everything(), ~ replace_na(.x, "<NA>")))

  start_at_ix <- map_int(2:nrow(scenarios_chr), function(i) {
    which.min(scenarios_chr[i, ] == scenarios_chr[i - 1, ])
  })

  # adjust start_at indices to account for groups (i.e. df-columns)
  steps <- dotnames(scenarios)
  groups <- dotprefix(scenarios)
  start_at_group <- groups[start_at_ix]
  start_at_step_ix <- map_int(start_at_group, function(group) {
    which.max(group == steps)
  })

  c(1, start_at_step_ix)
}

#' Roughly sort scenarios from preferred to disliked
#'
#' @description
#' A parameter value is taken to be better if it occurs earlier.
#' Thus, preferences can be changed by changing the order of
#' parameter values in `grid` and `fork` calls.
#'
#' @param scenarios A tibble with scenarios, usually generated with `grid` and/or `fork`
#' @param exclude Column names that should not be taken into consideration when rating scenarios.
#'
#' @return A numeric vector of penalties.
#' @export
penalize_scenarios <- function(scenarios, exclude = NULL) {
  if (is.null(exclude)) exclude <- c()

  scenarios |>
    select(!any_of(unordered)) |>
    mutate(across(everything(), ~ as.numeric(as.ordered(as_factor(as.character(.x))))) - 1) |>
    mutate(across(everything(), ~ replace_na(.x, 0))) |>
    mutate(across(everything(), ~ .x / max(.x))) |>
    rowSums()
}

#' Annotate scenarios with caching hints and a preference ordering, then pack columns into a more compact format.
#'
#' @param scenarios A tibble with scenarios, usually generated with `grid` and/or `fork`
#' @param do_not_penalize
#'
#' @return A tibble with scenarios packed into df-columns and annotated with additional columns.
#' @export
plan_scenarios <- function(scenarios, do_not_penalize = NULL) {
  scenarios$penalty <- penalize_scenarios(scenarios, exclude = do_not_penalize)
  scenarios$start_at <- plan_cache(scenarios)

  # changes variable ordering but does not matter at this stage
  pack_by_sep(scenarios, sep = ".")
}

exists <- function(x) {
  !any(is.na(x))
}

# we assume that a step that has multiple parameters, contained within a df-column,
# will describe in its first sub-column whether the analysis is needed;
# this is naturally what `pack_by_sep` will do if a <step> variable is defined before
# any <step>_<parameter> variables, but still good to keep in mind;
# this function also works with regular columns
is_needed <- function(x) {
  !(is.na(x[[1]]) | x[[1]] == FALSE)
}

# TODO: we'll probably want to add some memoization (with on-disk persistence) so that
# we don't have to start all over again if something goes wrong halfway through;
# something like Redis would do the job nicely (it's simple keys and values, a single
# number per analysis), but let's see how fast or slow this is anyway



# NOTE: I think this can be replaced with `safely` from the `purrr`
# library, although it will nest the output in a `list(result, error)`
# so it requires some additional unnesting.
silence <- function(fn, error_handler) {
  function(...) {
    # without `as.function`, any error handler we pass will be interpreted as an
    # expression and will not get executed
    tryCatch(fn(...), error = as.function(error_handler))
  }
}
