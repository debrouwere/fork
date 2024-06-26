library("rlang")
library("purrr")
library("stringr")
library("cli")

#' Verify whether each fork in a multiverse specification has a corresponding step function.
#'
#' @param steps a named list of functions
#' @param scenarios a data frame with a multiverse specification, such as produced by `fork::fork`
#'
#' @export
verify_steps <- function(steps, scenarios) {
  expected <- dotattr(scenarios, "forks")
  actual <- names(steps)
  missing <- setdiff(expected, actual)
  if (length(missing)) {
    abort_cli(str_c(
      "Please add missing steps to analysis pipeline: ",
      str_flatten_comma(missing),
      ". If nothing should happen, use the fork::noop function."
    ))
  }
}

# TODO: include some information about caching potential:
# a step may be needed {n} times, but only {m} times uncached,
# and then a total at the bottom (n scenarios, x% of steps cached)
describe_steps <- function(steps, scenarios) {
  total <- nrow(scenarios)
  for (step in names(steps)) {
    if (step %in% colnames(scenarios)) {
      n <- sum(!pull(scenarios, step))
      print(str_glue("* {step} (linked to `scenarios${step}`, {n} analyses (out of {total})"))
    } else {
      print(str_glue("* {step}"))
    }
  }
}

enhance_steps <- function(steps, cache = NULL) {
  steps <- imap(steps, function(fn, name) {
    rethrow_with_args(fn, name = name)
  })

  #steps <- imap(steps, function(fn, key) {
  #  do_if(fn, key = key, otherwise = list())
  #})

  if (!is.null(cache)) {
    steps <- imap(steps, function(fn, key) {
      cached(fn, cache = cache, key = key)
    })
  }

  steps <- map(steps, function(fn) {
    modifier(unsplat(fn))
  })

  steps
}

#' Convert a named list of step functions into a composition
#'
#' @description Turns a list of functions into a pipeline that accepts a list of
#'   arguments (e.g. a scenario from a multiverse specification produced with
#'   `fork::fork`, or more generally a row from a data frame) and produces
#'   the result of running each function on the input to the pipeline, merged
#'   with the output of the previous function.
#'
#'   In technical terms, in addition to creating the composition, this function
#'   also enhances the original step functions so that they report improved
#'   error messages (with `fork::rethrow_with_args`), accept individual function
#'   arguments instead of a list of arguments (with `fork::unsplat`), modify the
#'   input with the output (with `fork::modifier`) and cache the output (with
#'   `fork::cached`) if desired. All of this considerably easier to understand
#'   in the context of an example.
#'
#'   Because the pipeline unsplats function arguments, it becomes easy to test
#'   each step individually, with natural function signatures, even though behind
#'   the scenes each function accepts a list and returns a list.
#'
#' @param steps A named list of functions.
#' @param cache A step cache as generated by `make_cache`.
#' @param after A function to run after the steps of the pipeline. The default
#'   `last` should usually be left as-is.
#'
#' @return A function with signature `analysis(list_of_args)`
#' @export
#'
#' @examples
#' scenarios <- expand_grid(
#'   add = 1:3,
#'   multiply = 1:3
#' )
#' steps <- list(
#'   add = function(add, ...) {
#'     list(
#'       sum = 10 + add
#'     )
#'   },
#'   multiply = function(sum, multiply, ...) {
#'     list(result = tibble(
#'       sumprod = sum * multiply
#'     ))
#'   }
#' )
#'
#' analyze <- compose_steps(steps, cache = NULL)
#'
#' # we'll create a cache but refrain from using it for now
#' cache <- make_cache(steps)
#' analyze_and_cache <- compose_steps(steps, cache = cache)
#'
#' result <- analyze(add = 1, multiply = 2)
#' expect_equal(result, 22)
#'
#' results <- scenarios |> row_modify(analyze)
#' expect_equal(results$sumprod, c(11, 22, 33, 12, 24, 36, 13, 26, 39))
compose_steps <- function(steps, cache = NULL, select = last) {
  if (!is.null(cache)) {
    invalidate <- function(input) {
      invalidate_cache(cache, input$start_at_ix)
      input
    }
  } else {
    invalidate <- identity
  }
  steps <- enhance_steps(steps, cache = cache)
  compose(splat, invalidate, !!!steps, select, .dir = "forward")
}
