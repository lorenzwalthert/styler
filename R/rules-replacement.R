force_assignment_op <- function(pd) {
  to_replace <- pd$token == "EQ_ASSIGN"
  pd$token[to_replace] <- "LEFT_ASSIGN"
  pd$text[to_replace] <- "<-"
  pd
}

#' Removes quotes in the argument `package` in library()
#'
#' Function finds the position in the parse table that corresponds to the
#' argument `package`, if any. The argument corresponds to the first
#' unnamed argument, or to the named argument `package`.
#' Note that for this, we can't use `match.call()` because it does not
#' preserve the order. Then, the quotes are removed if and only if the
#' package argument is a string and not a symbol plus `character.only` is set
#' to `FALSE`.
#' @examples
#' style_text("tidyverse")
#' style_text("tidyverse", character.only = TRUE)
#' @keywords internal
remove_quotes_in_library_call <- function(pd) {
  if (pd$token[1] == "expr" &&
      pd$child[[1]]$text == "library") {
    expressions <- which(pd$token == "expr")[-1]
    named_expressions <- which(pd$token == "SYMBOL_SUB") + 2L
    names(named_expressions) <- pd$text[named_expressions - 2L]
    unnamed_expressions <- setdiff(expressions, named_expressions)
    idx_pkg <- which(pmatch(names(named_expressions), names(formals("library"))) == 1)
    if (length(idx_pkg) == 1L) {
      pos_pkg <- named_expressions[idx_pkg]
    } else if (length(unnamed_expressions) >= 1L){
      pos_pkg <- unnamed_expressions[1]
    } else {
      return(pd)
    }

    call_string <- post_visit(pd, list(extract_terminals)) %>%
      enrich_terminals() %>%
      serialize_parse_data_flattened(1)
    specified_call <- as.list(match.call(library, as.call(parse_text(call_string))))
    complete_call <- purrr::list_modify(as.list(formals(library)), !!! specified_call[-1])

    if (pd$child[[pos_pkg]]$token == "STR_CONST" &&
        !complete_call$character.only
    ) {
      pd$child[[pos_pkg]]$token[1] <- "SYMBOL"
      pd$child[[pos_pkg]]$text[1] <- substr(pd$child[[pos_pkg]]$text[1], start = 2L,
                                      nchar(pd$child[[pos_pkg]]$text[1]) - 1L)

    }
  }
  pd
}

resolve_semicolon <- function(pd) {
  is_semicolon <- pd$token == "';'"
  if (!any(is_semicolon)) return(pd)
  pd$lag_newlines[lag(is_semicolon)] <- 1L
  pd <- pd[!is_semicolon, ]
  pd
}
