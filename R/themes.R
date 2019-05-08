#' Set Eachan's default plotting theme
#'
#' @param font_size Font size to use
#' @param font Font to use
#'
#' @export
#'
set_theme <- function(font_size=15, font='Coolvetica Rg')
  theme_set(theme_classic(base_size=font_size, base_family=font))

#' Eachan's color scheme
#'
#' Return either  a list of colors, or match first argument to single color and return that as hex.
#'
#' @param color Color name
#'
#' @return Hex color
#'
#' @export
#'
colorscheme <- function(color=NULL) {

  color_scheme <- list(rajah='#F7B267', steelblue='#4298B5', brickred='#C33C54',
                       sandstorm='#EAD94C', deepkoamaru='#3B3561')

  if (is.null(color)) return ( color_scheme )

  if(is.character(color)) color <- match.arg(color, names(color_scheme))

  return(color_scheme[[color]])

}
