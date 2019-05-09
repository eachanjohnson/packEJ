#' Failback operator
#'
#' Binary operator: if the left-hand-side is \code{NULL}, \code{0}, \code{FALSE}, or \code{NA},
#' returns the right-hand-side. Otherwise returns the left-hand-side.
#'
#' @param x Value which might be 0-ish
#' @param y Failback value
#'
#' @return LHS or RHS
#'
#' @export
#'
`%or%` <- function(x, y) {

  predicate <- is.null(x) || (x == 0 | x == FALSE | is.na(x))

  if ( predicate )
    y
  else
    x
}

#' Ternary functions
#'
#' Ternary functions.
#'
#' @param x Condition which evaluates to \code{logical}
#' @param y Truth value
#'
#' @return Object of class \code{ternary}
#'
#' @export
#'
`%?%` <- function(x, y) {

  if ( is.logical(x) & length(x) > 0){
    r <- list(predicate=x)
    # delayedAssign('z', y)
    r$true <- y
    return ( structure(r, class='ternary') )
  }
  else
    stop('LHS is not non-zero-length logical: ', class(x), '[', length(x), ']')

}

#' @param x Object of class \code{ternary}
#' @param y False value
#'
#' @return LHS or RHS
#'
#' @export
#'
#' @rdname `%?%`
`%:%` <- function(x, z) {

  delayedAssign('zz', z)

  if ( inherits(x, 'ternary') ) {
    if ( x$predicate ) {
      x$true #%or% NA
    } else {
      zz #%or% NA
    }
  }
  else stop('LHS is not ternary: ', class(x))

}

#' Print with timestamp
#'
#' Wrapper for \code{cat} which adds a timestamp and newline.
#'
#' @param ... Values to concatenate and print to console using \code{cat}.
#'
#' @export
#'
println <- function(...) cat(date(), '>', ..., '\n')

#' Concatenate a character vector
#'
#' @param x Object to join
join <- function(x, ...) UseMethod('join')

#' @param x Character vector
#'
#' @return Character vector of length 1.
#'
#' @export
#' @rdname join
join.character <- function(x, char=', ', ...) paste(x, collapse=char)

#' @param x Numeric vector
#'
#' @return Character vector of length 1.
#'
#' @export
#' @rdname join
join.numeric <- function(x, char=', ', ...) paste(as.character(x), collapse=char)


#' Capitalize a string or vector of strings
#'
#' @param x Object to capitalize
capitalize <- function(x, ...) UseMethod('capitalize')

#' @export
#' @rdname capitalize
capitalise <- capitalize

#' @param x Character vector
#'
#' @return Character vector with first character capitalized.
#'
#' @export
#' @rdname capitalize
capitalize.character <- function(x, lowertail=FALSE) {

  first <- toupper(substr(x, 1, 1))

  ending <- substr(x, 2, nchar(x))

  if ( lowertail ) {
    ending <- tolower(ending)
  }

  return ( paste0(first, ending) )

}

#' Convert to exponent 10
#'
#' @param x Numeric
#'
#' @return Formatted expression
#'
#' @export
expo_log10 <- scales::trans_format('log10', scales::math_format(10^.x))
