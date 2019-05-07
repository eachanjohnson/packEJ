#' @export
println <- function(...) cat(date(), '>', ..., '\n')

#' @export
colorscheme <- function(color=NULL) {

  color_scheme <- list(rajah='#F7B267', steelblue='#4298B5', brickred='#C33C54',
                       sandstorm='#EAD94C', deepkoamaru='#3B3561')

  if (is.null(color)) return ( color_scheme )

  if(is.character(color)) color <- match.arg(color, names(color_scheme))

  return(color_scheme[[color]])

}

#' Concatenate a character vector
#'
#' @param x Object to join
join <- function(x, ...) UseMethod('join')

#' @param x Character vector
#' @export
#' @rdname join
join.character <- function(x, char=', ', ...) paste(x, collapse=char)

#' Capitalize a string or vector of strings
#'
#' @param x Object to capitalize
capitalize <- function(x, ...) UseMethod('capitalize')

#' @param x Character vector
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
