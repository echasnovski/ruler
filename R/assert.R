assert_exposure <- function(.tbl, .type = "error",
                            .trigger = ~ !all(.[["report"]][["value"]]
                                              %in% TRUE),
                            .informer = ~ .) {
  tbl_exposure <- get_exposure(.tbl)
  trigger <- rlang::as_function(.trigger, env = rlang::caller_env(n = 2))
  informer <- rlang::as_function(.informer, env = rlang::caller_env(n = 2))

  if (identical(tbl_exposure, NULL)) {
    stop("Supplied object does not have exposure.")
  }

  if (!is_exposure(tbl_exposure)) {
    stop("Extracted 'exposure' object is not exposure.")
  }

  if (trigger(tbl_exposure)) {
    print(informer(tbl_exposure))
    switch(.type,
           error = stop("assert_exposure is triggered."),
           warning = warning("assert_exposure is triggered."),
           message = message("assert_exposure is triggered.")
    )
  }

  .tbl
}
