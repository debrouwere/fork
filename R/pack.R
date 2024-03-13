# These functions are meant to be used with multiverse specifications
# such as those produced by `multiverse.R`
#
# Packing and unpacking refers to `tidyr::pack` and `tidyr::unpack`, which
# produces tibble df-columns: a column that can contain other columns
# (technically, a column that contains a tibble).
#
# Each step of a multiverse analysis may require multiple parameters, e.g.
# imputation of missing values can differ in the amount of imputations, the
# amount of iterations, the variables to impute, and so on, and it is nice to be
# able to refer to these as `scenario$impute$iter` and so on so we provide a
# `pack_by_sep` function to convert columns in the format `step.parameter` to
# df-column `step$parameter`. After we have built the multiverse, however,
# browsing it is easier without packed columns, and so `unpack_by_sep` converts
# back to the original format.

library("tibble")

#' Extract the column name prefixes from a dataset
#'
#' @description Extract the column name prefixes from a dataset with a mix of prefix and prefix.suffix column names.
#'
#' @param data A data frame or tibble.
#' @param sep The separator between prefix and suffix.
#' @param subset Exclude column names that do not have both prefix and suffix.
#'
#' @return A character vector of column name prefixes.
#' @export
dotprefix <- function(data, sep = ".", subset = FALSE) {
  if (subset) {
    names <- str_subset(colnames(data), coll(sep))
  } else {
    names <- colnames(data)
  }
  str_split_i(names, coll(sep), 1)
}

#' Extract the unique column name prefixes from a dataset
#'
#' @param data A data frame or tibble.
#' @param sep The separator between prefix and suffix.
#' @param subset Exclude column names that do not have both prefix and suffix.
#'
#' @return A character vector of column name prefixes.
#' @export
dotnames <- function(data, sep = ".", subset = FALSE) {
  unique(dotprefix(data, sep = sep, subset = subset))
}

#' Pack columns that have a prefix.suffix format into df-columns.
#'
#' @param data A data frame or tibble.
#' @param sep The separator between prefix and suffix.
#'
#' @return A tibble with df-columns.
#' @export
pack_by_sep <- function(data, sep = ".") {
  for (dotname in dotnames(data, subset = TRUE)) {
    data <- data |> pack({{ dotname }} := starts_with(dotname), .names_sep = sep)
  }
  data
}

is_packed <- is_tibble

#' Extract the column names of df-columns from a tibble.
#'
#' @param data A tibble.
#'
#' @return A character vector of column names.
#' @export
packnames <- function(data) {
  names <- colnames(data)
  pack_ixs <- map_lgl(names, \(name) is_packed(data[[name]]))
  names[pack_ixs]
}

#' Unpack df-columns into regular columns with a prefix.suffix naming scheme
#'
#' @param data A tibble.
#' @param sep The separator between prefix and suffix.
#'
#' @return A tibble without df-columns.
#' @export
unpack_by_sep <- function(data, sep = ".") {
  packs <- packnames(data)
  data <- data |> unpack(all_of(packs), names_sep = sep)
  redundancies <- intersect(colnames(data), str_glue("{packs}.{packs}"))
  data |> rename_with(~ str_split_i(.x, coll(sep), 1), all_of(redundancies))
}
