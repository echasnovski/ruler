# Set keys -------------------------------------------------------------
#' Set keys
#'
#' Key is a vector which goal is to provide information about rows in reference
#' data.frame. Its length should always be equal to number of rows in
#' data.frame. Keys are stored as [tibble][tibble::lst] in attribute `"keys"`
#' and so one data.frame can have multiple keys. Data.frame with keys is
#' implemented as class [keyed_df][keyed-df].
#'
#' @param .tbl Reference data.frame .
#' @param value Values of keys (converted to [tibble][tibble::as_tibble]).
#' @param ... Variables to be used as keys defined in similar fashion as in
#'   [dplyr::select()].
#' @param .add Whether to add keys to (possibly) existing ones. If `FALSE` keys
#'   will be overridden.
#' @param .exclude Whether to exclude key variables from `.tbl`.
#'
#' @details `key_by` ignores grouping when creating keys. Also if `.add == TRUE`
#' and names of some added keys match the names of existing keys the new ones
#' will override the old ones.
#'
#' Value for `keys<-` should not be `NULL` because it is converted to tibble
#' with zero rows. To remove keys use `unkey()`, [remove_keys()] or
#' [restore_keys()]. `assign_keys` is a more suitable for piping wrapper for
#' `keys<-`.
#'
#' @examples df <- mtcars
#'
#' # Value is converted to tibble
#' keys(df) <- 1:nrow(df)
#'
#' # This will throw an error
#' \dontrun{
#' keys(df) <- 1:10
#' }
#'
#' # Use 'vs' and 'am' as keys
#' df %>% key_by(vs, am)
#' df %>% key_by(vs, am, .exclude = TRUE)
#' df %>% key_by(vs) %>% key_by(am, .add = TRUE, .exclude = TRUE)
#'
#' # Override keys
#' df %>% key_by(vs, am) %>% dplyr::mutate(vs = 1) %>%
#'   key_by(gear, vs, .add = TRUE)
#'
#' # Use select helpers
#' df %>% key_by(dplyr::one_of(c("vs", "am")))
#' df %>% key_by(dplyr::everything())
#'
#' @seealso [Get keys][keys-get], [Manipulate keys][keys-manipulate]
#'
#' @name keys-set
NULL

#' @rdname keys-set
#' @export
`keys<-` <- function(.tbl, value) {
  value <- as_tibble(value)

  if (!isTRUE(nrow(value) == nrow(.tbl))) {
    stop("Keys object should have the same number of rows as data.")
  }

  attr(.tbl, "keys") <- value

  add_class_cond(.tbl, "keyed_df")
}

#' @rdname keys-set
#' @export
assign_keys <- function(.tbl, value) {
  keys(.tbl) <- value

  .tbl
}

#' @rdname keys-set
#' @export
key_by <- function(.tbl, ..., .add = FALSE, .exclude = FALSE) {
  UseMethod("key_by")
}

#' @export
key_by.default <- function(.tbl, ..., .add = FALSE, .exclude = FALSE) {
  if (dots_n(...) == 0) {
    return(.tbl)
  }

  tbl_keys <- keys(.tbl)
  cur_keys <- .tbl %>%
    # Keys should not have keys
    unkey() %>%
    # Keys should not be grouped
    ungroup() %>%
    select(...) %>%
    as_tibble()

  if (.add) {
    keys(.tbl) <- assign_tbl(tbl_keys, cur_keys)
  } else {
    keys(.tbl) <- cur_keys
  }

  if (.exclude) {
    .tbl <- diff_tbl(.tbl, cur_keys)
  }

  .tbl
}

#' @rdname keys-set
#' @export
unkey <- function(.tbl) {
  UseMethod("unkey")
}

#' @export
unkey.default <- function(.tbl) {
  attr(.tbl, "keys") <- NULL

  .tbl
}

#' @export
unkey.keyed_df <- function(.tbl) {
  .tbl <- remove_class(.tbl)

  NextMethod()
}


# Get keys ----------------------------------------------------------------
#' Get keys
#'
#' Functions for getting information about keys.
#'
#' @param .tbl Reference data.frame .
#'
#' @return `keys()` always returns a [tibble][tibble::lst] of keys. In case of
#'   no keys it returns a tibble with number of rows as in `.tbl` and zero
#'   columns. `raw_keys()` is just a wrapper for `attr(.tbl, "keys")`.
#'   To know whether `.tbl` has keys use `has_keys()`.
#'
#' @examples keys(mtcars)
#' raw_keys(mtcars)
#' has_keys(mtcars)
#'
#' df <- key_by(mtcars, vs, am)
#' keys(df)
#' has_keys(df)
#'
#' @seealso [Set keys][keys-set], [Manipulate keys][keys-manipulate]
#'
#' @name keys-get
NULL

#' @rdname keys-get
#' @export
keys <- function(.tbl) {
  keys_attr <- attr(.tbl, "keys")

  if (is.null(keys_attr)) {
    tibble(logical(nrow(.tbl)))[-1]
  } else {
    as_tibble(keys_attr)
  }
}

#' @rdname keys-get
#' @export
raw_keys <- function(.tbl) {
  attr(.tbl, "keys")
}

#' @rdname keys-get
#' @export
has_keys <- function(.tbl) {
  !is.null(attr(.tbl, "keys"))
}


# Manipulate keys ---------------------------------------------------------
#' Manipulate keys
#'
#' Functions to manipulate [keys][keys-set].
#'
#' @param .tbl Reference data.frame .
#' @param ... Variables to be used for operations defined in similar fashion as
#'   in [dplyr::select()].
#' @param .unkey Whether to [unkey()] `.tbl` in case there are no keys left.
#' @param .remove Whether to remove keys after restoring.
#'
#' @details `remove_keys()` removes keys defined with `...`.
#'
#' `restore_keys()` transfers keys defined with `...` into `.tbl` and removes
#' them from `keys` if `.remove == TRUE`. If `.tbl` is grouped the following
#' happens:
#' - If restored keys don't contain grouping variables then groups don't change;
#' - If restored keys contain grouping variables then result will be regrouped
#' based on restored values. In other words restoring keys beats 'not-modifying'
#' grouping variables rule. It is made according to the ideology of keys: they
#' contain information about rows and by restoring you want it to be
#' available.
#'
#' `rename_keys()` renames columns in keys using [dplyr::rename()].
#'
#' @examples df <- mtcars %>% key_by(vs, am, .exclude = TRUE)
#' df %>% remove_keys(vs)
#' df %>% remove_keys(dplyr::everything())
#' df %>% remove_keys(dplyr::everything(), .unkey = TRUE)
#'
#' df %>% restore_keys(vs)
#' df %>% restore_keys(vs, .remove = TRUE)
#'
#' df %>% restore_keys(dplyr::everything(), .remove = TRUE)
#' df %>% restore_keys(dplyr::everything(), .remove = TRUE, .unkey = TRUE)
#'
#' # Restoring on grouped data frame
#' df_grouped <- df %>% dplyr::mutate(vs = 1) %>% dplyr::group_by(vs)
#' df_grouped %>% restore_keys(dplyr::everything())
#'
#' # Renaming
#' df %>% rename_keys(Vs = vs)
#'
#' @seealso [Get keys][keys-get], [Set keys][keys-set]
#'
#' @name keys-manipulate

#' @rdname keys-manipulate
#' @export
remove_keys <- function(.tbl, ..., .unkey = FALSE) {
  UseMethod("remove_keys")
}

#' @export
remove_keys.default <- function(.tbl, ..., .unkey = FALSE) {
  tbl_keys <- keys(.tbl)
  left_keys <- diff_tbl(tbl_keys, select(tbl_keys, ...))

  set_key_cond(.tbl, left_keys, .unkey)
}

#' @rdname keys-manipulate
#' @export
restore_keys <- function(.tbl, ..., .remove = FALSE, .unkey = FALSE) {
  UseMethod("restore_keys")
}

#' @export
restore_keys.default <- function(.tbl, ..., .remove = FALSE, .unkey = FALSE) {
  tbl_keys <- keys(.tbl)
  tbl_class <- class(.tbl)

  if (ncol(tbl_keys) == 0) {
    return(.tbl)
  }

  restored_keys <- select(tbl_keys, ...)
  if (.remove) {
    left_keys <- diff_tbl(tbl_keys, restored_keys)
  } else {
    left_keys <- tbl_keys
  }

  # Restoring keys beats 'not-modifying' grouping variables.
  tbl_groups <- groups(.tbl)

  .tbl %>%
    ungroup() %>%
    assign_tbl(restored_keys) %>%
    group_by(UQS(tbl_groups)) %>%
    `class<-`(tbl_class) %>%
    set_key_cond(left_keys, .unkey)
}

#' @rdname keys-manipulate
#' @export
rename_keys <- function(.tbl, ...) {
  UseMethod("rename_keys")
}

#' @export
rename_keys.default <- function(.tbl, ...) {
  if (has_keys(.tbl)) {
    keys(.tbl) <- rename(keys(.tbl), ...)
  }

  .tbl
}

set_key_cond <- function(.tbl, .key, .unkey) {
  if (.unkey && (ncol(.key) == 0)) {
    .tbl <- unkey(.tbl)
  } else {
    keys(.tbl) <- .key
  }

  .tbl
}
