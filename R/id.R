add_id <- function(.data, key = TRUE) {
  id_name <- compute_id_name(names(.data))

  .data %>%
    tibble::rowid_to_column(var = id_name)
  # %>% key_by(!!quo(id_name))
}

compute_id_name <- function(x) {
  is_id <- grepl(pattern = "^\\.id[1]*$", x = as.character(x))

  if (sum(is_id) == 0) {
    return(".id")
  } else {
    n_ones <- nchar(x[is_id]) - 3
    marker <- c(rep(TRUE, (max(n_ones) + 1)), TRUE)
    marker[n_ones + 1] <- FALSE
    id_extra <- paste0(rep("1", which(marker)[1] - 1), collapse = "")

    return(paste0(".id", id_extra))
  }
}
