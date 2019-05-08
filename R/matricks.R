#' Matricks
#'
#' Matrix with side information. See \link{matrix} for description of parameters.
#'
#' @param data Matrix
#' @param rows_title Title or ID for rownames
#' @param cols_title Title or ID for colnames
#' @param rows_annotation Data frame of anntotations for rows. Must have a column name in common with rows_title
#' @param cols_annotation Data frame of anntotations for columns. Must have a column name in common with cols_title.
#'
#' @return Matricks object; inherits from \code{matrix}.
#'
#' @export
#'
#' @examples
#' annotation_df <- data.frame(col_id=LETTERS[1:3], annotation=letters[1:3])
#' m <- matrix(rnorm(9), ncol=3)
#' colnames(m) <- annotation_df$col_id
#' Matricks(m, cols_annotation=annotation_df)
Matricks <- function(data=NA,
                     rows_title=NULL, cols_title=NULL,
                     rows_annotation=NULL, cols_annotation=NULL,
                     ...) {

  if ( ! inherits(data, 'matrix') ) stop('Matricks needs a matrix')

  new_matrix <- data

  if ( is.null(rows_annotation) && is.null(cols_annotation) ) {

    warning('No annotation information supplied; returning a basic matrix.')

    return ( new_matrix )

  }


  rowtitle <- names(dimnames)[1] %or% rows_title %or% colnames(rows_annotation)[1]
  coltitle <- names(dimnames)[2] %or% cols_title %or% colnames(cols_annotation)[1]

  if ( ! is.null(rows_annotation) ) {

    row_keys <- rows_annotation[ , rowtitle, drop=TRUE]
    null_rows_and_numeric_key <- is.null(rownames(new_matrix)) &&
      is.numeric(row_keys) &&
      all(seq_len(nrow(new_matrix)) %in% row_keys)

    all_rows_in_key <- !is.null(rownames(new_matrix)) && all(rownames(new_matrix) %in% row_keys)

      if ( ! null_rows_and_numeric_key && ! all_rows_in_key )
        stop('Mismatch between rownames and rowname annotations')

  }

  if ( ! is.null(cols_annotation) ) {

    col_keys <- cols_annotation[ , coltitle, drop=TRUE]
    null_cols_and_numeric_key <- is.null(colnames(new_matrix)) &&
      is.numeric(col_keys) &&
      all(seq_len(ncol(new_matrix)) %in% col_keys)

    all_cols_in_key <- !is.null(colnames(new_matrix)) && all(colnames(new_matrix) %in% col_keys)

    if ( ! (null_cols_and_numeric_key || all_cols_in_key) )
      stop('Mismatch between colnames and colname annotations')

  }

  attr(new_matrix, 'rows_title')     <- rowtitle
  attr(new_matrix, 'cols_title')     <- coltitle
  names(attr(new_matrix, 'dimnames'))[1] <- rowtitle
  names(attr(new_matrix, 'dimnames'))[2] <- coltitle
  attr(new_matrix, 'rows_annotation') <- rows_annotation
  attr(new_matrix, 'cols_annotation') <- cols_annotation

  class(new_matrix) <- c('matricks', class(new_matrix))

  return ( new_matrix )

}

#' Show a Matricks
#'
#' Pretty printing
#'
#' @param x \code{\link{Matricks}} object.
#'
#' @export
#'
#' @examples
#' annotation_df <- data.frame(col_id=LETTERS[1:3], annotation=letters[1:3])
#' m <- matrix(rnorm(9), ncol=3)
#' colnames(m) <- annotation_df$col_id
#' m2 <- Matricks(m, cols_annotation=annotation_df)
#' print(m2)
#'
print.matricks <- function(x, ...) {

  rowtitle <- attr(x, 'rows_title')
  coltitle <- attr(x, 'cols_title')

  cat('A Matricks object:', nrow(x), 'rows x', ncol(x), 'columns\n')
  if ( !is.null(rowtitle) ) {

    rowann <- attr(x, 'rows_annotation')
    cat('Row labels:', rowtitle, ';', class(rowann), '[', join(dim(rowann), ','), ']\n')
    print(head(rowann))

  }

  if ( !is.null(coltitle) ) {

    colann <- attr(x, 'cols_annotation')
    cat('Column labels:', coltitle, ';', class(colann), '[', join(dim(colann), ','), ']\n')
    print(head(colann))

  }

  col_extent <- min(10, ncol(x))
  row_extent <- min(10, nrow(x))

  print(unclass(x)[seq_len(row_extent), seq_len(col_extent), drop=FALSE])

  return ( invisible(x) )

}

#' Subset a matricks
#'
#' Subset matrix and annotations on the fly
#'
#' @param x \code{\link{Matricks}} object.
#' @param i Row indices
#' @param j Column indicies
#' @param drop Not implemented
#'
#' @export
#'
#' @examples
#' annotation_df <- data.frame(col_id=LETTERS[1:3], annotation=letters[1:3])
#' m <- matrix(rnorm(9), ncol=3)
#' colnames(m) <- annotation_df$col_id
#' m2 <- Matricks(m, cols_annotation=annotation_df)
#' print(m2[1:3, ])
#'
`[.matricks` <- function(x, i, j,
                         drop=missing(i) %or% length(cols) == 1) {

  original_class <- class(x)

  new_matrix <- NextMethod()

  annotation_names <- setdiff(names(attributes(x)), names(attributes(new_matrix)))

  for ( n in annotation_names ) {

    this_is_annotation <- grepl('_annotation$', n)
    this_title <- this_is_annotation %?% attr(x, gsub('_annotation$', '_title', n)) %:% attr(x, n)
    dim_name_f <- grepl('^cols_', n) %?% colnames %:% rownames

    if (this_is_annotation) {

      old_attr <- attr(x, n)
      new_attr <- droplevels(old_attr[old_attr[ , this_title, drop=TRUE] %in% dim_name_f(new_matrix), ])

    } else {

      new_attr <- this_title

    }

    attr(new_matrix, n) <- new_attr

  }

  return ( invisible(new_matrix) )

}




