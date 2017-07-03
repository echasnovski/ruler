add_id <- function(.tbl) {
  id_name <- compute_id_name(names(.tbl))

  .tbl[[id_name]] <- seq_len(nrow(.tbl))
  .tbl <- select(.tbl, UQ(sym(id_name)), everything())

  .tbl
}

add_id_key <- function(.tbl, .add = FALSE, .exclude = FALSE) {
  if (.add) {
    id_name <- compute_id_name(c(colnames(.tbl), colnames(keys(.tbl))))
  } else {
    id_name <- compute_id_name(colnames(.tbl))
  }

  .tbl[[id_name]] <- seq_len(nrow(.tbl))
  .tbl <- select(.tbl, UQ(sym(id_name)), everything())

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
