context("actions")


# Input data --------------------------------------------------------------
mtcars_exposed <- mtcars %>% set_exposure(input_exposure_ref)
rule_breakers <- input_exposure_ref %>%
  get_report() %>%
  filter(!(value %in% TRUE))

trigger_nrow_30 <- function(.tbl) {
  nrow(get_report(.tbl)) > 40
}
trigger_nrow_10 <- function(.tbl) {
  nrow(get_report(.tbl)) > 10
}
actor_print <- function(.tbl) {
  print(get_exposure(.tbl))

  .tbl
}

assert_text <- "assert_any_breaker: Some breakers found in exposure."

exposure_no_breakers <- input_exposure_ref
exposure_no_breakers$packs_info <- exposure_no_breakers$packs_info %>%
  slice(1) %>%
  as_packs_info()
exposure_no_breakers$report <- exposure_no_breakers$report %>%
  slice(c(1, 2, 5)) %>%
  as_report()

mtcars_exposed_no_breakers <- set_exposure(mtcars, exposure_no_breakers)


# Custom expectations -----------------------------------------------------
expect_asserts <- function(input, type, silent = FALSE, result = input,
                           output_name = "Breakers report\n",
                           output_report,
                           warnings = character(0),
                           messages = character(0),
                           ...) {
  assert_evalutation <- evaluate_promise(
    assert_any_breaker(input, type, silent, ...)
  )

  expect_identical(assert_evalutation$result, result)
  expect_match(assert_evalutation$output, output_name)
  expect_match(assert_evalutation$output, output_report)
  expect_identical(assert_evalutation$warnings, warnings)
  expect_identical(assert_evalutation$messages, messages)
}


# act_after_exposure ------------------------------------------------------
test_that("act_after_exposure works", {
  expect_error(
    act_after_exposure(mtcars, trigger_nrow_30, actor_print),
    "act_after_exposure:.*not.*have"
  )

  input_bad <- mtcars
  attr(input_bad, "exposure") <- "a"

  expect_error(
    act_after_exposure(input_bad, trigger_nrow_30, actor_print),
    "act_after_exposure:.*not.*proper.*exposure"
  )

  expect_silent(
    output_1 <- act_after_exposure(
      mtcars_exposed, trigger_nrow_30,
      actor_print
    )
  )
  expect_identical(output_1, mtcars_exposed)

  output_ref <- capture_output(print(input_exposure_ref))

  expect_output(
    output_2 <- act_after_exposure(
      mtcars_exposed, trigger_nrow_10,
      actor_print
    ),
    output_ref,
    fixed = TRUE
  )
  expect_identical(output_2, mtcars_exposed)
})


# assert_any_breaker ------------------------------------------------------
test_that("assert_any_breaker works", {
  output_ref <- capture_output(print(rule_breakers))

  # Error assertions
  expect_error(
    expect_output(assert_any_breaker(mtcars_exposed), output_ref),
    assert_text
  )
  expect_error(
    expect_output(assert_any_breaker(mtcars_exposed, "error"), output_ref),
    assert_text
  )
  expect_error(
    expect_output(assert_any_breaker(mtcars_exposed, "error", TRUE), ""),
    assert_text
  )

  # Warning and message assertions
  expect_asserts(
    mtcars_exposed,
    "warning",
    output_report = output_ref,
    warnings = assert_text
  )
  expect_asserts(
    mtcars_exposed,
    "message",
    output_report = output_ref,
    messages = paste0(assert_text, "\n")
  )

  # Absence of printing
  expect_asserts(
    mtcars_exposed,
    "warning",
    silent = TRUE,
    output_name = "",
    output_report = "",
    warnings = assert_text
  )
  expect_asserts(
    mtcars_exposed,
    "message",
    silent = TRUE,
    output_name = "",
    output_report = "",
    messages = paste0(assert_text, "\n")
  )

  # Absence of assertions
  expect_asserts(
    mtcars_exposed_no_breakers,
    "error",
    output_name = "",
    output_report = ""
  )
  expect_asserts(
    mtcars_exposed_no_breakers,
    "warning",
    output_name = "",
    output_report = ""
  )
  expect_asserts(
    mtcars_exposed_no_breakers,
    "message",
    output_name = "",
    output_report = ""
  )
})

test_that("assert_any_breaker accounts for printing options", {
  output_ref <- capture_output(print(rule_breakers, n = 3))

  expect_error(
    expect_output(
      assert_any_breaker(mtcars_exposed, "error", n = 3),
      output_ref
    ),
    assert_text
  )
  expect_asserts(
    mtcars_exposed,
    "warning",
    output_report = output_ref,
    warnings = assert_text,
    n = 3
  )
  expect_asserts(
    mtcars_exposed,
    "message",
    output_report = output_ref,
    messages = paste0(assert_text, "\n"),
    n = 3
  )
})


# any_breaker -------------------------------------------------------------
test_that("any_breaker works", {
  expect_error(any_breaker("a"), "any_breaker:.*not.*proper.*exposure")
  expect_true(any_breaker(input_exposure_ref))
  expect_false(any_breaker(exposure_no_breakers))
})


# generate_breakers_informer ----------------------------------------------
test_that("generate_breakers_informer works", {
  custom_assert_text <- "Custom"
  informer <- generate_breakers_informer(
    .fun = warning,
    .message = custom_assert_text,
    .silent = FALSE
  )

  expect_is(informer, "function")

  output <- evaluate_promise(informer(.tbl = mtcars_exposed))

  expect_identical(output$result, mtcars_exposed)
  expect_match(output$output, capture_output(print(rule_breakers)))
  expect_identical(output$warnings, custom_assert_text)
  expect_identical(output$messages, character(0))
})
