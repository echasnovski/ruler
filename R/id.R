add_id <- function(.tbl) {
  id_sym <- sym(compute_id_name(names(.tbl)))

  .tbl %>%
    # Have to use `mutate` because tibble::rowid_to_column is not generic
    # and removes attr(., "keys").
    mutate(UQ(id_sym) := seq_len(nrow(.tbl))) %>%
    select(UQ(id_sym), everything())
}

add_id_key <- function(.tbl, .add = FALSE, .exclude = FALSE) {
  if (.add) {
    id_name <- compute_id_name(c(colnames(.tbl), colnames(keys(.tbl))))
  } else {
    id_name <- compute_id_name(colnames(.tbl))
  }

  tbl_names <- colnames(.tbl)
  .tbl[[id_name]] <- seq_len(nrow(.tbl))
  .tbl <- .tbl[, c(id_name, tbl_names)]

  .tbl %>%
    key_by(UQ(sym(id_name)), .add = .add, .exclude = .exclude)
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
