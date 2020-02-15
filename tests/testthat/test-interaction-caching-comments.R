fresh_testthat_cache()
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

text8 <- c(
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

style_text(text8)

# however, for some reason, we cannot isolate the problem when stylerignore is
# not used. It appears as then, the call is not cached properly

# here is a smaller reprex
fresh_testthat_cache()
compute_parse_data_nested(text7, tidyverse_style())

cache_deactivate()

text7_styled <- as.character(style_text(text7))
activate_testthat_cache()
cache_by_expression(text7, tidyverse_style())
compute_parse_data_nested(text8, tidyverse_style())
