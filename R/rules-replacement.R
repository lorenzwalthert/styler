force_assignment_op <- function(pd) {
  to_replace <- pd$token == "EQ_ASSIGN"
  pd$token[to_replace] <- "LEFT_ASSIGN"
  pd$text[to_replace] <- "<-"
  pd
}

#' Removes quotes in the argument `package` in library()
#'
#' Function finds the position in the parse table that corresponds to the
#' argument `package`, if any. The argument corresponds to the named argument
#' `package` or the first unnamed argument otherwise.
#' Note that for fiding the argument `package`, we can't use `match.call()`
#' because it does not
#' preserve the order. Then, the quotes are removed if and only if the
#' package argument is a string and not a symbol plus `character.only` is set
#' to `FALSE`. To determine the value of `character.only`, we can use
#' [match.call()].
#' @examples
#' style_text("library('tidyverse', character.only = TRUE)")
#' style_text("library(tidyverse, character.only = TRUE)")
#' @keywords internal
remove_quotes_in_library_call <- function(pd) {
  if (pd$token[1] == "expr" &&
      pd$child[[1]]$text == "library") {
    argument_package_idx <- find_pos_argument(pd, "package")
    if (is.na(argument_package_idx)) {
      return(pd)
    }
    call_string <- post_visit(pd, list(extract_terminals)) %>%
      enrich_terminals() %>%
      serialize_parse_data_flattened(1)
    specified_call <- as.list(match.call(library, as.call(parse_text(call_string))))
    complete_call <- purrr::list_modify(as.list(formals(library)), !!! specified_call[-1])

    if (pd$child[[argument_package_idx]]$token == "STR_CONST" &&
        !complete_call$character.only
    ) {
      pd$child[[argument_package_idx]]$token[1] <- "SYMBOL"
      pd$child[[argument_package_idx]]$text[1] <-
        substr(pd$child[[argument_package_idx]]$text[1], start = 2L,
        nchar(pd$child[[argument_package_idx]]$text[1]) - 1L
      )

    }
  }
  pd
}

#' Find the index of a positional argument
#' Assumes that `pd` is a function call, which can be tested with
#' [is_function_call()] for example.
#' @param pd A parse table
#' @param arg_name The name of an argument.
#' @return
#' Returns the index of the argument in the parse table.
#' @keywords internal
find_pos_argument <- function(pd, arg_name) {
  fun_name <- pd$child[[1]]$text
  arg_pos_full <- which(names(formals(fun_name)) == arg_name)[1]

  stopifnot(length(fun_name) == 1)
  expressions <- which(pd$token == "expr")[-1]
  named_expressions <- which(pd$token == "SYMBOL_SUB") + 2L
  names(named_expressions) <- pd$text[named_expressions - 2L]
  unnamed_expressions <- setdiff(expressions, named_expressions)


  arg_pos_this_call <- which(
    pmatch(names(named_expressions), names(formals(fun_name))) == arg_pos_full
  )
  if (length(arg_pos_this_call) == 1L) {
    pos <- named_expressions[arg_pos_this_call]
  } else if (length(unnamed_expressions) >= 1L){
    pos <- unnamed_expressions[arg_pos_full]
  } else {
    pos <- NA
  }
}

resolve_semicolon <- function(pd) {
  is_semicolon <- pd$token == "';'"
  if (!any(is_semicolon)) return(pd)
  pd$lag_newlines[lag(is_semicolon)] <- 1L
  pd <- pd[!is_semicolon, ]
  pd
}
