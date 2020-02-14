test_that("caching works with stylerignore when multi-token lines", {
  on.exit(cache_deactivate())
  text1 <- c(
    "# styler: off",
    "1 + 1",
    "# styler: on",
    "# a comment"
  )
  activate_testthat_cache()
  cache_clear(ask = FALSE)
  activate_testthat_cache()
  expect_equal(
    as.character(style_text(text1)),
    text1
  )

  text2 <- c(
    "# styler: off",
    "1 + 1",
    "# styler: on",
    "# a comment"
  )
  expect_equal(
    as.character(style_text(text2)),
    text2
  )

  text3 <- c(
    "# styler: off",
    "1 + 1 #comment2",
    "# styler: on",
    "#a comment"
  )
  text3_correct <- c(
    "# styler: off",
    "1 + 1 #comment2",
    "# styler: on",
    "# a comment"
  )

  expect_equal(
    as.character(style_text(text3)),
    text3_correct
  )

  expect_equal(
    as.character(style_text(text3_correct)),
    text3_correct
  )

  text4 <- c(
    "# styler: off",
    "1 +1",
    "# styler: on",
    "# a comment"
  )

  expect_equal(
    as.character(style_text(text4)),
    text4
  )

  text5 <- c(
    "# styler: off",
    "1 +1;3",
    "# styler: on",
    "# a comment"
  )

  expect_equal(
    as.character(style_text(text5)),
    text5
  )
})


test_that("caching works with stylerignore when no mulit-token lines are present", {
  on.exit(cache_deactivate())
  text6 <- c(
    "# styler: off",
    "1 + 1",
    "    x(5)",
    "# styler: on",
    "# a comment"
  )
  activate_testthat_cache()
  cache_clear(ask = FALSE)
  activate_testthat_cache()
  expect_equal(
    as.character(style_text(text6)),
    text6
  )
  text7 <- c(
    "# styler: off",
    "1 +1",
    "f()",
    "    x(5)",
    "# styler: on",
    "# a comment"
  )
  expect_equal(
    as.character(style_text(text7)),
    text7
  )
})
