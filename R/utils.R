#' @export
dplyr::`%>%`


# Class utilities ---------------------------------------------------------
add_class <- function(x, class) {
  class(x) <- c(class, class(x))

  x
}

add_class_cond <- function(x, class) {
  if (class(x)[1] != class) {
    class(x) <- c(class, class(x))
  }

  x
}

remove_class <- function(x) {
  class(x) <- class(x)[-1]

  x
}

remove_class_cond <- function(x, class) {
  if (class(x)[1] == class) {
    class(x) <- class(x)[-1]
  }

  x
}


# Two tables utilities ----------------------------------------------------
diff_tbl <- function(.tbl1, .tbl2) {
  not_in_tbl2_idx <- which(!(colnames(.tbl1) %in% colnames(.tbl2)))

  select(.tbl1, not_in_tbl2_idx)
}

assign_tbl <- function(.tbl1, .tbl2) {
  .tbl1[, colnames(.tbl2)] <- .tbl2[, ]

  .tbl1
}
