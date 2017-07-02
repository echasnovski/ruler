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
#' class and 'keys' attribute (excluding `summarise` and its scoped variants).
#' Also they modify keys in case rows of reference data.frame have somehow
#' changed.
#'
#' @param .tbl A keyed object.
#' @param ... Appropriate arguments for functions.
#' @param add Parameter for [dplyr::group_by].
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
  keys_attr <- attr(.tbl, "keys")
  y <- NextMethod()
  attr(y, "keys") <- keys_attr

  add_class_cond(y, "keyed_df")
}

#' @rdname keyed-df-dplyr
#' @export
rename.keyed_df <- function(.tbl, ...) {
  keys_attr <- attr(.tbl, "keys")
  y <- NextMethod()
  attr(y, "keys") <- keys_attr

  add_class_cond(y, "keyed_df")
}

#' @rdname keyed-df-dplyr
#' @export
mutate.keyed_df <- function(.tbl, ...) {
  keys_attr <- attr(.tbl, "keys")
  y <- NextMethod()
  attr(y, "keys") <- keys_attr

  add_class_cond(y, "keyed_df")
}

#' @rdname keyed-df-dplyr
#' @export
summarise.keyed_df <- function(.tbl, ...) {
  y <- NextMethod()

  unkey(y)
}

#' @rdname keyed-df-dplyr
#' @export
group_by.keyed_df <- function(.tbl, ..., add = FALSE) {
  keys_attr <- attr(.tbl, "keys")
  attr(.tbl, "keys") <- NULL
  y <- NextMethod()
  attr(y, "keys") <- keys_attr

  add_class_cond(y, "keyed_df")
}

#' @rdname keyed-df-dplyr
#' @export
ungroup.keyed_df <- function(.tbl, ...) {
  keys_attr <- attr(.tbl, "keys")
  y <- NextMethod()
  attr(y, "keys") <- keys_attr

  add_class_cond(y, "keyed_df")
}

# rowwise is not generic in dplyr 0.7.1 so this function will not preserve
# "keyed_df" class.
#' @rdname keyed-df-dplyr
#' @export
rowwise.keyed_df <- function(.tbl) {
  keys_attr <- attr(.tbl, "keys")
  # y <- NextMethod()
  y <- rowwise(unkey(.tbl))
  attr(y, "keys") <- keys_attr

  add_class_cond(y, "keyed_df")
}

# # Implement `guided_*` functions for verbs inferencing rows and use them.
# arrange.keyed_df <- function(.tbl, ...) {
#   res <- NextMethod()
#   class(res) <- class(.tbl)
#
#   res
# }
#
# filter.keyed_df <- function(.tbl, ...) {
#   res <- NextMethod()
#   class(res) <- class(.tbl)
#
#   res
# }

