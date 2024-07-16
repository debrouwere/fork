library("rlang")
library("purrr")
library("dplyr")
library("stringr")
library("cli")


#' Wrap a function to run it and pass through its arguments
#'
#' @description This is a useful wrapper for functions in compositions (see
#'   `purrr:compose`) that is run merely for its side effects.
#'
#' @param f The function to modify.
#'
#' @export
invisibly <- function(f) {
  function(x) {
    f()
    x
  }
}

#' Wrap a function that takes multiple arguments to instead take a list of arguments
#'
#' @param fn A function to modify.
#'
#' @export
unsplatted <- function(fn) {
  function(args) {
    do.call(fn, args)
  }
}

is_falsey <- function(x) {
  is.null(x) || is.na(x) || (is.numeric(x) && x == 0) || (is.logical(x) && !x) || (is.character(x) && str_length(x) == 0)
}

#' Wrap a function that accepts a list and produces a list, so that it merges input and output
#'
#' @param fn A function to modify.
#'
#' @export
modifier <- function(fn) {
  function(input) {
    output <- fn(input)
    list_assign(input, !!!output)
  }
}

#' Only execute a function if a particular argument is truthy
#'
#' @param fn A function to modify
#' @param path The name of an argument (or component thereof)
#' @param otherwise The return value if the function is not executed

#' @export
skippable <- function(fn, path, otherwise) {
  if (is.null(path)) {
    fn
  } else {
    function(...) {
      arguments <- list2(...)
      value <- pluck(arguments, !!!path)
      if (is_falsey(value)) {
        otherwise
      } else {
        fn(...)
      }
    }
  }
}


# like `identity` but with a splat

#' Return the function arguments as a list.
#'
#' @description `splat` works similarly to `identity` but returns all of its arguments as a list, not just the first one.
#'
#' @param ... function arguments
#'
#' @return a list of arguments
#' @export
splat <- function(...) {
  list(...)
}

#' Modify the rows of a data frame, one at a time, and bind the results with the
#' original rows
#'
#' @description `row_modify(...)` is very similar to `dplyr::rowwise() |>
#' dplyr::mutate(...)` and if possible you should prefer to use that workflow,
#' however `fork::row_modify` works on the actual row instead of an indirect
#' representation of the row (`.data`), which leads to fewer surprises when all
#' columns of the row are relevant to the modification.
#'
#' @param data a data frame
#' @param fn a function that takes a one-row data frame and returns a one-row data frame
#' @export
row_modify <- function(data, fn) {
  results <- data |>
    group_by(.ix = row_number()) |>
    group_modify(\(x, g) fn(x)) |>
    ungroup() |>
    select(-.ix)
  bind_cols(data, results)
}

obj_to_chr <- function(x) {
  case_match(class(x)[1],
    "character" ~ str_trunc(str_flatten_comma(x), width = 50),
    "numeric" ~ str_trunc(str_flatten_comma(format(x)), width = 50),
    "integer" ~ str_trunc(str_flatten_comma(as.character(x)), width = 50),
    "logical" ~ str_trunc(str_flatten_comma(as.character(x)), width = 50),
    "list" ~ str_glue("list({keys})", keys = str_trunc(str_flatten_comma(names(x)), width = 44)),
    .default = str_glue("{cls}(...)", cls = class(x)[1]),
    .ptype = character(0)
  )
}

#' Modify a function to rethrow error conditions with function name and arguments
#'
#' @param fn A function to be modified.
#' @param name Name of the function.
#'
#' @export
rethrow_with_arguments <- function(fn, name = '(unknown)') {
  function(...) {
    args <- rlang::dots_list(..., .named = TRUE)
    rlang::try_fetch(
      fn(...),
      error = function(condition) {
        locals <- args |>
          imap(\(v, k) str_c(k, ": ", obj_to_chr(v))) |>
          as.character() |>
          set_names("i")
        cli::cli_abort(
          message = "Function `{.strong {name}}` raised an error.",
          body = locals,
          parent = condition
        )
      }
    )
  }
}
