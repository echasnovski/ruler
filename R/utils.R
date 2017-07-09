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
