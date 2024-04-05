kvs <- function(keys, values) {
  if (length(values) == 1 & length(values) < length(keys)) {
    values <- rep(values, length(keys))
  }

  l <- as.list(values)
  names(l) <- keys
  l
}

#' Initialize a simple step cache
#'
#' @description We cache the steps of a process, and then for any subsequent
#' similar process we use the cache of the last step before the process
#' diverges. Only one process is cached at any time, so this cache relies on a
#' good ordering of the processes (last step changest fastest) and user-managed
#' cache invalidation to function effectively.
#'
#' @param steps a named list, whose names describe the steps to cache
#'
#' @return A cache environment.
#' @export
make_cache <- function(steps) {
  steps <- names(steps)
  cache <- kvs(steps, NA)
  slots <- list2env(cache)
  attr(slots, "steps") <- steps
  attr(slots, "hits") <- kvs(steps, 0)
  attr(slots, "misses") <- kvs(steps, 0)
  slots
}

increment <- function(obj, which, key) {
  attr(obj, which)[[key]] <- attr(obj, which)[[key]] + 1
}

#' Wrap a function to cache its outcome
#'
#' @description See `fork::make_cache` for details. In particular, note that
#'   this is not a generic memoization cache, it has no invalidation built in,
#'   does not vary the cache by the arguments with which the function is
#'   called... in short, you are responsible for calling
#'   `fork::invalidate_cache` whenever necessary.
#'
#' @param fn A function to modify.
#' @param cache A simple cache, created through `fork::make_cache`
#' @param key The key in which to store the cached outcome.
#'
#' @export
cached <- function(fn, cache, key) {
  function(...) {
    if (all(is.na(cache[[key]]))) {
      increment(cache, 'misses', key)
      cache[[key]] <- fn(...)
    } else {
      increment(cache, 'hits', key)
    }
    cache[[key]]
  }
}

#' Invalidate the step cache from a particular step onwards
#'
#' @description The step cache is invalidated in-place, not in a copy.
#'
#' @param cache A simple step cache as made by `make_cache`
#' @param start_at_ix The step index from which to invalidate
#'
#'
#' @return The partially invalidated cache.
#' @export
invalidate_cache <- function(cache, start_at_ix = 1) {
  steps <- attr(cache, "steps", exact = TRUE)
  n <- length(steps)
  for (i in start_at_ix:n) {
    name <- steps[i]
    cache[[name]] <- NA
  }

  cache
}
