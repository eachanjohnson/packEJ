println <- function(...) cat(date(), '>', ..., '\n')

colorscheme <- function(color=NULL) {

  color_scheme <- list(rajah='#F7B267', steelblue='#4298B5', brickred='#C33C54',
                       sandstorm='#EAD94C', deepkoamaru='#3B3561')

  if (is.null(color)) return ( color_scheme )

  if(is.character(color)) color <- match.arg(color, names(color_scheme))

  return(color_scheme[[color]])

}
