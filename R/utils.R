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


# Naming ------------------------------------------------------------------
compute_def_names <- function(n = 1, root = "", start_ind = 1) {
  if (n < 1) {
    return(character(0))
  } else {
    paste0(root, "..", seq_len(n) + start_ind - 1)
  }
}

enhance_names <- function(name, prefix = "", root = "", suffix = "",
                          start_ind = 1) {
  if (length(name) == 0) {
    return(name)
  }

  def_name <- compute_def_names(n = length(name), root = root,
                                start_ind = start_ind)

  is_empty_name <- name == ""
  name[is_empty_name] <- def_name[is_empty_name]

  paste0(prefix, name, suffix)
}
