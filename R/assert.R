# Assertions --------------------------------------------------------------
assert_any_breaker <- function(.tbl, .type = "error", ...) {
  informer_fun <- switch(
    .type,
    message = message,
    warning = function(...) {warning(..., call. = FALSE)},
    function(...) {stop(..., call. = FALSE)}
  )
  breakers_informer <- generate_breakers_informer(
    informer_fun,
    "assert_any_breaker: Some breakers found in exposure.",
    ...
  )

  act_on_exposure(.tbl, any_breaker, breakers_informer)
}


# General act -------------------------------------------------------------
act_on_exposure <- function(.tbl, .trigger, .actor) {
  tbl_exposure <- get_exposure(.tbl)
  trigger <- rlang::as_function(.trigger, env = rlang::caller_env(n = 2))
  actor <- rlang::as_function(.actor, env = rlang::caller_env(n = 2))

  if (identical(tbl_exposure, NULL)) {
    stop("act_on_exposure: Supplied object does not have exposure.")
  }

  if (!is_exposure(tbl_exposure)) {
    stop("act_on_exposure: Extracted 'exposure' object isn't proper exposure.")
  }

  if (isTRUE(trigger(tbl_exposure))) {
    res <- actor(.tbl, tbl_exposure)
  } else {
    res <- .tbl
  }

  res
}


# Triggers ----------------------------------------------------------------
any_breaker <- function(.exposure) {
  report <- get_report(.exposure)

  !all(is_obeyer(report[["value"]]))
}


# Actors ---------------------------------------------------------------
generate_breakers_informer <-
  function(.fun = stop, .message = "Some breakers found in exposure.",
           ...) {
    force(.fun)
    force(.message)

    function(.tbl, .exposure) {
      report_breakers <- get_report(.exposure) %>% remove_obeyers(TRUE)

      print(report_breakers, ...)
      .fun(.message)

      .tbl
    }
  }
