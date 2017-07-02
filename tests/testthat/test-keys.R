context("keys")


# Input data --------------------------------------------------------------
df <- mtcars

# `keys<-` ----------------------------------------------------------------
test_that("`keys<-` works", {
  input_keys <- tibble(x = 1:nrow(df))
  keys(df) <- input_keys

  expect_identical(attr(df, "keys"), input_keys)
  expect_is(df, "keyed_df")
})

test_that("`keys<-` converts value to tibble", {
  input_keys <- 1:nrow(df)
  keys(df) <- input_keys

  expect_identical(attr(df, "keys"), tibble(value = input_keys))

  df_1 <- df
  input_keys <- data.frame(x = 1:nrow(df_1))
  keys(df_1) <- input_keys

  expect_identical(attr(df_1, "keys"), as_tibble(input_keys))
})

test_that("`keys<-` throws an error", {
  input_keys <- 1:(nrow(df) - 1)

  expect_error(keys(df) <- input_keys, "row")
})


# assign_keys -------------------------------------------------------------
test_that("assign_keys works", {
  input_keys <- tibble(x = 1:nrow(df))
  df <- assign_keys(df, input_keys)

  expect_identical(attr(df, "keys"), input_keys)
  expect_is(df, "keyed_df")
})


# key_by ------------------------------------------------------------------
test_that("key_by.default works", {
  output_1 <- key_by(df, vs)

  expect_identical(attr(output_1, "keys"), as_tibble(df[, "vs", drop = FALSE]))

  output_2 <- key_by(df)

  expect_identical(output_2, df)
})

test_that("key_by.default handles .add and .exclude arguments", {
  df_1 <- key_by(df, vs)
  output_1 <- key_by(df_1, am, .add = FALSE, .exclude = FALSE)

  expect_equal(colnames(attr(output_1, "keys")), "am")
  expect_true("am" %in% colnames(output_1))

  output_2 <- key_by(df_1, am, .add = TRUE, .exclude = FALSE)

  expect_equal(colnames(attr(output_2, "keys")), c("vs", "am"))
  expect_true("am" %in% colnames(output_2))

  output_3 <- key_by(df_1, am, .add = FALSE, .exclude = TRUE)

  expect_equal(colnames(attr(output_3, "keys")), "am")
  expect_false("am" %in% colnames(output_3))

  output_4 <- key_by(df_1, am, .add = TRUE, .exclude = TRUE)

  expect_equal(colnames(attr(output_4, "keys")), c("vs", "am"))
  expect_false("am" %in% colnames(output_4))
})

test_that("key_by.default overrides keys in case .add is TRUE", {
  df_1 <- key_by(df, vs, am)
  df_1$vs <- 1
  output <- key_by(df_1, gear, vs, .add = TRUE)
  output_keys_ref <- keys(df_1)
  output_keys_ref$vs <- 1
  output_keys_ref$gear <- df$gear

  expect_identical(keys(output), output_keys_ref)
})

test_that("key_by.default removes keys in case .add is TRUE", {
  df_1 <- df %>% as_tibble() %>% key_by(vs)
  output <- key_by(df_1, am, .add = FALSE)

  expect_false(inherits(keys(output), "keyed_df"))
  expect_null(attr(keys(output), "keys"))
})

test_that("key_by.default handles grouped_df", {
  output <- df %>% group_by(gear, vs) %>% key_by(vs, am)

  expect_equal(class(output)[1:2], c("keyed_df", "grouped_df"))
  expect_equal(group_vars(output), c("gear", "vs"))
  expect_false(is_grouped_df(attr(output, "keys")))
})

# unkey -------------------------------------------------------------------
test_that("unkey.default works", {
  attr(df, "keys") <- 1:3
  output <- unkey(df)

  expect_null(attr(output, "keys"))
})

test_that("unkey.keyed_df works", {
  df_keyed <- key_by(df, vs, am)
  output <- unkey(df_keyed)

  expect_false(inherits(output, "keyed_df"))
  expect_null(attr(output, "keys"))

  class(df_keyed) <- c("keyed_df", "keyed_df", "data.frame")
  expect_equal(class(unkey(df_keyed)), "data.frame")
})

# keys --------------------------------------------------------------------
test_that("keys works", {
  output_not_keyed <- keys(df)

  expect_is(output_not_keyed, "tbl_df")
  expect_equal(dim(output_not_keyed), c(nrow(df), 0))

  df_keyed <- key_by(df, vs, am)
  output_keyed <- keys(df_keyed)

  expect_is(output_keyed, "tbl_df")
  expect_equal(colnames(output_keyed), c("vs", "am"))
  expect_equal(nrow(output_keyed), nrow(df))
})


# has_keys ----------------------------------------------------------------
test_that("has_keys works", {
  expect_false(has_keys(df))

  keys(df) <- 1:nrow(df)
  expect_true(has_keys(df))
})


# remove_keys -------------------------------------------------------------
test_that("remove_keys.default works", {
  df_keyed <- key_by(df, vs, am)
  output <- remove_keys(df_keyed, vs)

  expect_equal(colnames(attr(output, "keys")), "am")
  expect_is(output, "keyed_df")
  expect_equal(colnames(df_keyed), colnames(output))
})

test_that("remove.keys.default correctly unkeys", {
  df_keyed <- key_by(df, vs, am)
  output_1 <- remove_keys(df_keyed, everything(), .unkey = FALSE)

  expect_equal(ncol(attr(output_1, "keys")), 0)
  expect_is(output_1, "keyed_df")

  output_2 <- remove_keys(df_keyed, everything(), .unkey = TRUE)

  expect_equal(attr(output_2, "keys"), NULL)
  expect_false(inherits(output_2, "keyed_df"))
})


# restore_keys ------------------------------------------------------------
test_that("restore_keys.default works", {
  expect_identical(restore_keys(df), df)

  df_keyed_1 <- key_by(df, vs, am)
  df_keyed_1$vs <- 1
  df_keyed_1$am <- 0
  output_1 <- restore_keys(df_keyed_1, vs)
  output_ref_1 <- df_keyed_1
  output_ref_1$vs <- df$vs
  output_ref_1$am <- 0

  expect_identical(output_1, output_ref_1)

  df_keyed_2 <- key_by(df, vs, am)
  df_keyed_2$vs <- 1
  df_keyed_2$am <- NULL
  output_2 <- restore_keys(df_keyed_2, vs, am)
  output_ref_2 <- df_keyed_2
  output_ref_2$vs <- df$vs
  output_ref_2$am <- df$am

  expect_identical(output_2, output_ref_2)
})

test_that("restore_keys.default handles .remove and .unkey arguments", {
  df_keyed <- key_by(df, vs, am)
  df_keyed$vs <- 1
  df_keyed$am <- 0
  output_1 <- restore_keys(df_keyed, vs, .remove = FALSE, .unkey = FALSE)
  output_ref_1 <- df_keyed
  output_ref_1$vs <- df$vs

  expect_identical(output_1, output_ref_1)

  output_2 <- restore_keys(df_keyed, vs, .remove = TRUE, .unkey = FALSE)
  output_ref_2 <- df_keyed
  output_ref_2$vs <- df$vs
  keys(output_ref_2) <- select(keys(output_ref_2), -vs)

  expect_identical(output_2, output_ref_2)

  output_3 <- restore_keys(df_keyed, everything(),
                           .remove = FALSE, .unkey = TRUE)
  output_ref_3 <- df_keyed
  output_ref_3$vs <- df$vs
  output_ref_3$am <- df$am

  expect_identical(output_3, output_ref_3)

  output_4 <- restore_keys(df_keyed, everything(),
                           .remove = TRUE, .unkey = TRUE)

  expect_identical(output_4, df)
})

test_that("restore_keys.default preserves class", {
  df_keyed_1 <- df %>% key_by(vs, am)
  df_keyed_1$vs <- 1
  output_1 <- restore_keys(df_keyed_1, vs)

  expect_equal(class(output_1), c("keyed_df", "data.frame"))

  output_2 <- restore_keys(df_keyed_1, everything(),
                           .remove = TRUE, .unkey = TRUE)

  expect_equal(class(output_2), "data.frame")

  df_keyed_2 <- df %>% key_by(vs, am) %>% as_tibble()
  df_keyed_2$vs <- 1
  output_3 <- restore_keys(df_keyed_2, vs)

  expect_equal(class(output_3), c("keyed_df", "tbl_df", "tbl", "data.frame"))

  output_4 <- restore_keys(df_keyed_2, everything(),
                           .remove = TRUE, .unkey = TRUE)

  expect_equal(class(output_4), c("tbl_df", "tbl", "data.frame"))
})

test_that("restore_keys.default handles grouping", {
  df_keyed <- df %>% key_by(vs, am)
  df_keyed$vs <- 1
  df_keyed_1 <- df_keyed %>% group_by(vs)

  expect_silent(output_1 <- df_keyed_1 %>% restore_keys(everything()))
  expect_equal(group_vars(output_1), "vs")
  expect_equal(nrow(count(output_1)), 2)

  df_keyed_2 <- df_keyed %>% group_by(gear)

  expect_silent(output_2 <- df_keyed_2 %>% restore_keys(everything()))
  expect_equal(group_vars(output_2), "gear")
  expect_equal(nrow(count(output_2)), 3)
})


# set_key_cond ------------------------------------------------------------
test_that("set_key_cond works", {
  key_tbl_1 <- tibble(x = 1:nrow(df))

  output_1 <- set_key_cond(df, key_tbl_1, FALSE)
  expect_identical(keys(output_1), key_tbl_1)

  output_2 <- set_key_cond(df, key_tbl_1, TRUE)
  expect_identical(keys(output_1), key_tbl_1)

  key_tbl_2 <- tibble(logical(nrow(df)))[-1]

  output_3 <- set_key_cond(df, key_tbl_2, FALSE)
  expect_true(inherits(output_3, "keyed_df"))
  expect_identical(attr(output_3, "keys"), key_tbl_2)

  output_4 <- set_key_cond(df, key_tbl_2, TRUE)
  expect_false(inherits(output_4, "keyed_df"))
  expect_identical(attr(output_4, "keys"), NULL)
})
