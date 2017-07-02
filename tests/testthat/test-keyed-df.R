context("keyed-df")


# Input data --------------------------------------------------------------
df <- mtcars
df_keyed <- df %>% key_by(vs, am)
keys_df <- keys(df_keyed)

# is_keyed_df -------------------------------------------------------------
test_that("is_keyed_df works", {
  expect_true(is_keyed_df(df_keyed))
  expect_false(is_keyed_df(df))

  class(df) <- c("keyed_df", "data.frame")

  expect_false(is_keyed_df(df))

  attr(df, "keys") <- seq_len(nrow(df) - 1)

  expect_false(is_keyed_df(df))

  attr(df, "keys") <- matrix(seq_len(nrow(df) - 1), ncol = 1)

  expect_false(is_keyed_df(df))

  attr(df, "keys") <- data.frame(x = seq_len(nrow(df) - 1))

  expect_false(is_keyed_df(df))

  df_bad_keyed <- add_class(df_keyed[[1]], "keyed_df")
  attr(df_bad_keyed, "keys") <- keys_df

  expect_false(is_keyed_df(df_bad_keyed))
})


# is.keyed_df -------------------------------------------------------------
test_that("is.keyed_df works", {
  expect_identical(is.keyed_df, is_keyed_df)
})


# print -------------------------------------------------------------------
test_that("print.keyed_df works", {
  expect_output(print(df_keyed), "keyed object.*vs.*am")

  expect_output(
    df_keyed %>% remove_keys(everything(), .unkey = FALSE) %>% print(),
    "keyed object.*no.*key"
  )
})


# [ -----------------------------------------------------------------------
test_that("`[.keyed_df` works", {
  i_idx <- 1:10
  j_idx <- 1:3
  output_1 <- df_keyed[i_idx, j_idx]
  output_ref_1 <- df[i_idx, j_idx] %>%
    assign_keys(keys_df[i_idx, ])

  expect_identical(output_1, output_ref_1)

  output_2 <- df_keyed[, j_idx]
  output_ref_2 <- df[, j_idx] %>%
    assign_keys(keys_df)

  expect_identical(output_2, output_ref_2)

  output_3 <- df_keyed[i_idx, logical(0)]
  output_ref_3 <- df[i_idx, logical(0)] %>%
    assign_keys(keys_df[i_idx, ])

  expect_identical(output_3, output_ref_3)
})


# select ------------------------------------------------------------------
test_that("select.keyed_df works", {
  output_1 <- select(df_keyed, mpg, vs)
  output_ref_1 <- select(df, mpg, vs) %>% assign_keys(keys_df)

  expect_identical(output_1, output_ref_1)

  output_2 <- select(df_keyed, starts_with("c"))
  output_ref_2 <- select(df, starts_with("c")) %>% assign_keys(keys_df)

  expect_identical(output_2, output_ref_2)
})

test_that("select_all works", {
  output <- select_all(df_keyed, toupper)
  output_ref <- select_all(df, toupper) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("select_if works", {
  is_whole <- function(x) all(floor(x) == x)
  output <- select_if(df_keyed, is_whole, toupper)
  output_ref <- select_if(df, is_whole, toupper) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("select_at works", {
  output <- select_at(df_keyed, vars(gear, vs))
  output_ref <- select_at(df, vars(gear, vs)) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

# rename ------------------------------------------------------------------
test_that("rename.keyed_df works", {
  output <- rename(df_keyed, newMPG = mpg)
  output_ref <- rename(df, newMPG = mpg) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("rename_all works", {
  output <- rename_all(df_keyed, toupper)
  output_ref <- rename_all(df, toupper) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("rename_if works", {
  is_whole <- function(x) all(floor(x) == x)
  output <- rename_if(df_keyed, is_whole, toupper)
  output_ref <- rename_if(df, is_whole, toupper) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("rename_at works", {
  output <- rename_at(df_keyed, vars(gear, vs), toupper)
  output_ref <- rename_at(df, vars(gear, vs), toupper) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

# mutate ------------------------------------------------------------------
test_that("mutate.keyed_df works", {
  output <- mutate(df_keyed, mpg = 0, vs = 1)
  output_ref <- mutate(df, mpg = 0, vs = 1) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("mutate_all works", {
  output <- mutate_all(df_keyed, as.character)
  output_ref <- mutate_all(df, as.character) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("mutate_if works", {
  output <- mutate_if(df_keyed, is.double, as.integer)
  output_ref <- mutate_if(df, is.double, as.integer) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("mutate_at works", {
  output <- mutate_at(df_keyed, vars(gear, vs), as.integer)
  output_ref <- mutate_at(df, vars(gear, vs), as.integer) %>%
    assign_keys(keys_df)

  expect_identical(output, output_ref)
})


# transmute ---------------------------------------------------------------
test_that("transmute.keyed_df works", {
  output <- transmute(df_keyed, mpg = 1, vs = 10)
  output_ref <- transmute(df, mpg = 1, vs = 10) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("transmute_all works", {
  output <- transmute_all(df_keyed, as.character)
  output_ref <- transmute_all(df, as.character) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("transmute_if works", {
  output <- transmute_if(df_keyed, is.double, as.integer)
  output_ref <- transmute_if(df, is.double, as.integer) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("transmute_at works", {
  output <- transmute_at(df_keyed, vars(gear, vs), as.integer)
  output_ref <- transmute_at(df, vars(gear, vs), as.integer) %>%
    assign_keys(keys_df)

  expect_identical(output, output_ref)
})


# summarise ---------------------------------------------------------------
test_that("summarise.keyed_df works", {
  expect_false(is_keyed_df(summarise(df_keyed, meanGear = mean(gear))))
})

test_that("summarise_all works", {
  output <- summarise_all(df_keyed, funs(n()))
  expect_false(is_keyed_df(output))
})

test_that("summarise_if works", {
  output <- summarise_if(df_keyed, is.double, mean)
  expect_false(is_keyed_df(output))
})

test_that("summarise_at works", {
  output <- summarise_at(df_keyed, vars(gear, vs), mean)
  expect_false(is_keyed_df(output))
})


# group_by ----------------------------------------------------------------
test_that("group_by.keyed_df works", {
  output <- group_by(df_keyed, gear, vs)
  output_ref <- group_by(df, gear, vs) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("group_by_all works", {
  output <- group_by_all(df_keyed, as.character)
  output_ref <- group_by_all(df, as.character) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("group_by_if works", {
  output <- group_by_if(df_keyed, is.double, as.integer)
  output_ref <- group_by_if(df, is.double, as.integer) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})

test_that("group_by_at works", {
  output <- group_by_at(df_keyed, vars(gear, vs), as.integer)
  output_ref <- group_by_at(df, vars(gear, vs), as.integer) %>%
    assign_keys(keys_df)

  expect_identical(output, output_ref)
})

# ungroup -----------------------------------------------------------------
test_that("ungroup.keyed_df works", {
  output <- group_by(df_keyed, gear, vs) %>% ungroup()
  output_ref <- as_tibble(df) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})


# rowwise.keyed_df --------------------------------------------------------
test_that("rowwise.keyed_df works", {
  output <- `rowwise.keyed_df`(df_keyed)
  output_ref <- rowwise(df) %>% assign_keys(keys_df)

  expect_identical(output, output_ref)
})
