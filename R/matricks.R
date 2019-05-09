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
#' annotation_df <- data.frame(col_id=LETTERS[1:5], annotation=letters[1:5])
#' m <- matrix(rnorm(9), ncol=3)
#' colnames(m) <- annotation_df$col_id[1:3]
#' Matricks(m, cols_annotation=annotation_df)
#'
Matricks <- function(data=NA,
                     rows_title=NULL, cols_title=NULL,
                     rows_annotation=NULL, cols_annotation=NULL,
                     ...) {

  if ( ! inherits(data, 'matrix') )
    stop('Matricks needs a matrix')

  new_matrix <- data

  if ( is.null(rows_annotation) && is.null(cols_annotation) ) {

    warning('No annotation information supplied; returning a basic matrix.')

    return ( new_matrix )

  }


  rowtitle <- names(dimnames)[1] %or% rows_title %or% colnames(rows_annotation)[1] %or% NA
  coltitle <- names(dimnames)[2] %or% cols_title %or% colnames(cols_annotation)[1] %or% NA

  dimnames(new_matrix) <- setNames(list(rownames(data), colnames(data)), c(rowtitle, coltitle))

  if ( ! is.null(rows_annotation) ) {

    row_keys <- rows_annotation[ , rowtitle, drop=TRUE]
    null_rows_and_numeric_key <- is.null(rownames(new_matrix)) &&
      is.numeric(row_keys) &&
      all(seq_len(nrow(new_matrix)) %in% row_keys)

    all_rows_in_key <- !is.null(rownames(new_matrix)) && all(rownames(new_matrix) %in% row_keys)

    if ( ! null_rows_and_numeric_key && ! all_rows_in_key )
      stop('Mismatch between rownames (',
            rownames(new_matrix),
           ') and rowname annotations (',
            row_keys,
           ')')

    row_keys <- intersect(row_keys, rownames(new_matrix))

    rows_annotation <- rows_annotation[rows_annotation[ , rowtitle, drop=TRUE] %in% row_keys, ]

    new_matrix <- new_matrix[row_keys, , drop=FALSE]

    #names(attr(new_matrix, 'dimnames'))[1] <- rowtitle

  }

  if ( ! is.null(cols_annotation) ) {

    col_keys <- cols_annotation[ , coltitle, drop=TRUE]
    null_cols_and_numeric_key <- is.null(colnames(new_matrix)) &&
      is.numeric(col_keys) &&
      all(seq_len(ncol(new_matrix)) %in% col_keys)

    all_cols_in_key <- !is.null(colnames(new_matrix)) && all(colnames(new_matrix) %in% col_keys)

    if ( ! (null_cols_and_numeric_key || all_cols_in_key) )
      stop('Mismatch between colnames (',
           colnames(new_matrix),
           ') and colname annotations (',
           col_keys,
           ')')

    col_keys <- intersect(col_keys, colnames(new_matrix))

    cols_annotation <- cols_annotation[cols_annotation[ , coltitle, drop=TRUE] %in% col_keys, ]

    new_matrix <- new_matrix[ , col_keys, drop=FALSE]

    #names(attr(new_matrix, 'dimnames'))[2] <- coltitle

  }

  attr(new_matrix, 'rows_title')     <- rowtitle
  attr(new_matrix, 'cols_title')     <- coltitle
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
  if ( !is.na(rowtitle) ) {

    rowann <- attr(x, 'rows_annotation')
    cat('Row labels:', rowtitle, ';', class(rowann), '[', join(dim(rowann), ','), ']\n')
    print(head(rowann))

  }

  if ( !is.na(coltitle) ) {

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
#' @param drop As usual for [
#' @param filter A data frame for a filtering join on side-information, or a named vector of
#'   column names and values to keep.
#'
#' @export
#'
#' @examples
#' annotation_df <- data.frame(col_id=LETTERS[1:3], annotation=letters[1:3])
#' m <- matrix(rnorm(9), ncol=3)
#' colnames(m) <- annotation_df$col_id
#' m2 <- Matricks(m, cols_annotation=annotation_df)
#' filter_df <- data.frame(col_id=LETTERS[1:2])
#' print(subset(m2, filter=filter_df))
#'
#' print(subset(m2, filter=list(col_id=LETTERS[1:2])))
#'
subset.matricks <- function(x, i, j,
                            drop=missing(i) %or% length(cols) == 1,
                            filter=NULL) {

  if ( is.null(filter) ) return ( x[i, j, drop=drop] )

  colann_columns <- colnames(attr(x, 'cols_annotation'))
  rowann_columns <- colnames(attr(x, 'rows_annotation'))

  filter_rows    <- any(names(filter) %in% rowann_columns)
  filter_columns <- any(names(filter) %in% colann_columns)

  if ( filter_rows && filter_columns )
    stop('Column names for filter in both row and column annotations')

  filter_attr <- filter_rows %?% attr(x, 'rows_annotation') %:% attr(x, 'cols_annotation')
  dim_title <- filter_rows %?% attr(x, 'rows_title') %:% attr(x, 'cols_title')

  if ( inherits(filter, 'data.frame') ) {

    common_columns <- intersect(colnames(filter), colnames(filter_attr))

    new_attr <- filter_attr %>%
      dplyr::semi_join(filter, by=common_columns)

  } else {

    new_attr <- filter_attr

    for ( i in seq_along(filter) ) {

      new_attr <- new_attr[new_attr[ , names(filter)[i]] %in% filter[[i]], ]

    }

  }

  dimname_subset <- new_attr[ , dim_title, drop=TRUE]

  if ( filter_rows )    x <- x[dimname_subset, ]
  if ( filter_columns ) x <- x[ , dimname_subset]

  return ( x )

}


#' @param i Row indices
#' @param j Column indicies
#' @param drop As usual for [
#'
#' @export
#' @rdname subset.matricks
#'
#' @examples
#' annotation_df <- data.frame(col_id=LETTERS[1:3], annotation=letters[1:3])
#' m <- matrix(rnorm(9), ncol=3)
#' colnames(m) <- annotation_df$col_id
#' m2 <- Matricks(m, cols_annotation=annotation_df)
#' print(m2[1:3, ])
#'
`[.matricks` <- function(x, i, j,
                         drop=FALSE) {

  original_class <- class(x)

  new_matrix <- unclass(x)[i, j, drop=drop]

  if ( drop ) return (new_matrix)

  rowann <- attr(x, 'rows_annotation')
  colann <- attr(x, 'cols_annotation')

  if (!missing(i) && !is.numeric(i) && !is.null(rowann)) {

    rownames(rowann) <- rowann[ , attr(x, 'rows_title'), drop=TRUE]

  }

  if (!missing(j) && !is.numeric(j) && !is.null(colann)) {

    rownames(colann) <- colann[ , attr(x, 'cols_title'), drop=TRUE]
  }

  rowann <- rowann[i, , drop=FALSE]
  rownames(rowann) <- NULL
  colann <- colann[j, , drop=FALSE]
  rownames(colann) <- NULL

  new_matrix <- Matricks(new_matrix,
                         rows_title=attr(x, 'rows_title'),
                         cols_title=attr(x, 'cols_title'),
                         rows_annotation=rowann,
                         cols_annotation=colann)

  # if ( !is.null(attr(new_matrix, 'cols_annotation')) )
  #   attr(new_matrix, 'cols_annotation') <- attr(new_matrix, 'cols_annotation')[ , j, drop=FALSE]
  #
  # if ( !is.null(attr(new_matrix, 'rows_annotation')) )
  #   attr(new_matrix, 'rows_annotation') <- attr(new_matrix, 'rows_annotation')[ , i, drop=FALSE]


  return ( invisible(new_matrix) )

}




