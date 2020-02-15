cache_clear(ask = FALSE)
cache_activate()
# Using cache 1.3.1.9000 at /Users/lorenz/Library/Caches/R/R.cache/styler/1.3.1.9000.
text7 <- c(
  "call(",
  "# styler: off",
  "1 +1,",
  "f(),",
  "    x(5))",
  "# styler"
)
style_text(text7)

text7 <- c(
  "call(",
  "# styler: off",
  "1 +1,",
  "f(),",
  "    x(5))",
  "# styler: on"
)
# problem: function call is cached, but contains a comment. Since it is a
# comment in a function call, will be preserved since with drop_cached_chidlren(),
# all comments are preserved (but we should only preserve top-level comments).

style_text(text7)
