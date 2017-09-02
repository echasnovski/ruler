context("spread-groups")


# Input data --------------------------------------------------------------
input_grouped_summary <- mtcars %>%
  group_by(vs, am) %>%
  summarise(n_low = n() > 6, n_high = n() < 10)


# spread_groups -----------------------------------------------------------
test_that("spread_groups works", {
  output_ref_1 <- tibble(
    '0_0._.n_low' = TRUE, '0_1._.n_low' = FALSE,
    '1_0._.n_low' = TRUE, '1_1._.n_low' = TRUE,
    '0_0._.n_high' = FALSE, '0_1._.n_high' = TRUE,
    '1_0._.n_high' = TRUE, '1_1._.n_high' = TRUE
  )

  expect_identical(spread_groups(input_grouped_summary, vs, am),
                   output_ref_1)

  output_ref_2 <- output_ref_1
  colnames(output_ref_2) <- gsub("^(.)_", "\\1__", colnames(output_ref_2))

  expect_identical(spread_groups(input_grouped_summary, vs, am,
                                 .group_sep = "__"),
                   output_ref_2)

  output_ref_3 <- output_ref_1
  colnames(output_ref_3) <- gsub("\\._\\.", "___", colnames(output_ref_3))

  expect_identical(spread_groups(input_grouped_summary, vs, am,
                                 .col_sep = "___"),
                   output_ref_3)
})

test_that("spread_groups throws errors", {
  expect_error(spread_groups(input_grouped_summary),
               "spread_groups: No group.*column")
  expect_error(spread_groups(input_grouped_summary, ends_with("Absent")),
               "spread_groups: No group.*column")
  expect_error(spread_groups(input_grouped_summary, vs),
               "spread_groups:.*non-unique")
  expect_error(spread_groups(input_grouped_summary, everything()),
               "spread_groups: No rule.*column")
  expect_error(input_grouped_summary %>%
                 ungroup() %>%
                 mutate(vs = 1:4) %>%
                 spread_groups(vs),
               "spread_groups:.*logical")
})
