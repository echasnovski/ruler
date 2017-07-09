# Keyed object ------------------------------------------------------------
#' Keyed object
#'
#' Utility functions for keyed objects which are implemented with class
#' `keyed_df`. Keyed object should inherit from `keyed_df`, have rows and
#' contain a data.frame of [keys][keys-set] in attribute 'keys'.
#'
#' @param .tbl Object to check.
#' @param x Object to print or extract elements.
#' @param ... Further arguments passed to or from other methods.
#' @param i,j Arguments for \code{\link{[}}.
#'
#' @examples is_keyed_df(mtcars)
#' mtcars %>% key_by(vs) %>% is_keyed_df
#'
#' # Not valid keyed_df
#' df <- mtcars
#' class(df) <- c("keyed_df", "data.frame")
#' is_keyed_df(df)
#'
#' @name keyed-df
NULL

#' @rdname keyed-df
#' @export
is_keyed_df <- function(.tbl) {
  keys_attr <- attr(.tbl, "keys")

  inherits(.tbl, "keyed_df") &&
    inherits(keys_attr, "data.frame") &&
    isTRUE(nrow(keys_attr) == nrow(.tbl))
}

#' @rdname keyed-df
#' @export
is.keyed_df <- is_keyed_df

#' @rdname keyed-df
#' @export
print.keyed_df <- function(x, ...) {
  cat("# A keyed object. Keys: ")
  x_keys <- keys(x)

  if (ncol(x_keys) == 0) {
    cat("there are no keys.\n")
  } else {
    cat(paste0(names(x_keys), collapse = ", "), "\n")
  }

  NextMethod()
}

#' @rdname keyed-df
#' @export
`[.keyed_df` <- function(x, i, j, ...) {
  y <- NextMethod()

  if (!missing(i)) {
    keys(y) <- keys(x)[i, , drop = FALSE]
  } else {
    keys(y) <- keys(x)
  }

  class(y) <- class(x)

  y
}


# Verbs from dplyr for keyed_df -------------------------------------------
#' Verbs from dplyr for keyed_df
#'
#' Defined methods for [dplyr] generic single table functions. They preserve
#' 'keyed_df' class and 'keys' attribute (excluding `summarise` and its scoped
#' variants which remove them). Also these methods modify rows in keys according
#' to the rows modification in reference data.frame (if any).
#'
#' @param .tbl,.data A keyed object.
#' @param ... Appropriate arguments for functions.
#' @param add Parameter for [dplyr::group_by].
#' @param .by_group Parameter for [dplyr::arrange].
#'
#' @details [dplyr::transmute()] is supported implicitly with [dplyr::mutate()]
#' support.
#'
#' [dplyr::rowwise()] as for `dplyr` version 0.7.1 is not generic. Use
#' `rowwise.keyed_df` directly.
#'
#' All [scoped][dplyr::scoped] variants of present functions are also supported.
#'
#' @examples mtcars %>% key_by(vs, am) %>% dplyr::mutate(gear = 1)
#'
#' @name keyed-df-dplyr
NULL

#' @rdname keyed-df-dplyr
#' @export
select.keyed_df <- function(.tbl, ...) {
  next_method_keys(.tbl, select, ...)
}

#' @rdname keyed-df-dplyr
#' @export
rename.keyed_df <- function(.tbl, ...) {
  next_method_keys(.tbl, rename, ...)
}

#' @rdname keyed-df-dplyr
#' @export
mutate.keyed_df <- function(.tbl, ...) {
  next_method_keys(.tbl, mutate, ...)
}

#' @rdname keyed-df-dplyr
#' @export
summarise.keyed_df <- function(.tbl, ...) {
  unkey(NextMethod())
}

#' @rdname keyed-df-dplyr
#' @export
group_by.keyed_df <- function(.tbl, ..., add = FALSE) {
  next_method_keys(.tbl, group_by, ..., add = add)
}

#' @rdname keyed-df-dplyr
#' @export
ungroup.keyed_df <- function(.tbl, ...) {
  next_method_keys(.tbl, ungroup, ...)
}

# rowwise is not generic in dplyr 0.7.1 so use this function directly.
#' @rdname keyed-df-dplyr
#' @export
rowwise.keyed_df <- function(.tbl) {
  next_method_keys(.tbl, rowwise)
}

#' @rdname keyed-df-dplyr
#' @export
arrange.keyed_df <- function(.tbl, ..., .by_group = FALSE) {
  if (is_grouped_df(.tbl)) {
    next_method_keys_track(.tbl, arrange, ..., .by_group = .by_group)
  } else {
    next_method_keys_track(.tbl, arrange, ...)
  }
}

# To ensure `filter` from `dplyr` (and not from `stats`)
#' @export
dplyr::filter

#' @rdname keyed-df-dplyr
#' @export
filter.keyed_df <- function(.data, ...) {
  next_method_keys_track(.data, filter, ...)
}

#' @rdname keyed-df-dplyr
#' @export
slice.keyed_df <- function(.tbl, ...) {
  next_method_keys_track(.tbl, slice, ...)
}

next_method_keys <- function(.tbl, .f, ...) {
  # If attr(.tbl, "keys") is NULL it is replaced with 0-column tibble
  .f(unkey(.tbl), ...) %>% assign_keys(keys(.tbl))
}

next_method_keys_track <- function(.tbl, .f, ...) {
  dots_names <- names(quos(...))
  id_name <- compute_id_name(c(names(.tbl), dots_names))

  y <- unkey(.tbl)
  y[[id_name]] <- 1:nrow(y)
  res <- .f(y, ...)

  keys(res) <- keys(.tbl)[res[[id_name]], ]
  res[[id_name]] <- NULL

  res
}
