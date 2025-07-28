# General -----------------------------------------------------------------
#' Inside punctuation regular expression
#'
#' Function to construct regular expression of form: 'non alpha-numeric
#' characters' + 'some characters' + 'non alpha-numeric characters'.
#'
#' @param .x Middle characters to be put between non alpha-numeric characters.
#'
#' @examples
#' inside_punct()
#'
#' inside_punct("abc")
#' @export
inside_punct <- function(.x = "\\._\\.") {
  paste0("[^[:alnum:]]*", .x, "[^[:alnum:]]*")
}

negate_select_cols <- function(.tbl, ...) {
  selected_tbl <- select(.tbl, ...)

  setdiff(colnames(.tbl), colnames(selected_tbl))
}

# Replicate deprecated `rlang::squash()`
squash <- function(x) {
  out <- purrr::list_flatten(x)
  if (identical(out, x)) {
    return(out)
  }
  squash(out)
}

# General assertions ------------------------------------------------------
assert_positive_length <- function(.x, .name) {
  if (length(.x) == 0) {
    stop(.name, " should have positive length.", call. = FALSE)
  }

  invisible(.x)
}

assert_length <- function(.x, .length, .name) {
  if (length(.x) != .length) {
    stop(.name, " should have length ", .length, ".", call. = FALSE)
  }

  invisible(.x)
}

assert_character <- function(.x, .name) {
  if (!is.character(.x)) {
    stop(.name, " should be a character vector.", call. = FALSE)
  }

  invisible(.x)
}


# Class utilities ---------------------------------------------------------
add_class <- function(.x, .class) {
  class(.x) <- c(.class, class(.x))

  .x
}

add_class_cond <- function(.x, .class) {
  if (class(.x)[1] != .class) {
    class(.x) <- c(.class, class(.x))
  }

  .x
}

remove_class_cond <- function(.x, .class) {
  if (class(.x)[1] == .class) {
    class(.x) <- class(.x)[-1]
  }

  .x
}


# Naming ------------------------------------------------------------------
compute_def_names <- function(.n = 1, .root = "", .start_ind = 1) {
  if (.n < 1) {
    return(character(0))
  } else {
    paste0(.root, "__", seq_len(.n) + .start_ind - 1)
  }
}

enhance_names <- function(
  .name,
  .prefix = "",
  .root = "",
  .suffix = "",
  .start_ind = 1
) {
  if (length(.name) == 0) {
    return(.name)
  }

  def_name <- compute_def_names(length(.name), .root, .start_ind)

  is_empty_name <- .name == ""
  .name[is_empty_name] <- def_name[is_empty_name]

  paste0(.prefix, .name, .suffix)
}
