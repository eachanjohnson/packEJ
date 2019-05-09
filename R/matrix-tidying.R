#' Cast to tibble
#'
#' @return Data frame
as_df <- function(x, ...) UseMethod('as_df')

#' @param x Data frame
#' @inheritParams as_df
#' @export
#' @rdname as_df
#'
#'
#' @importFrom dplyr %>%
as_df.data.frame <- function(x, rownames='rownames', ...) {

  x %>%
    tibble::rownames_to_column('rownames') %>%
    tibble::as_tibble()

}

#' @param x Matrix
#' @inheritParams as_df
#' @export
#' @rdname as_df
#'
#'
#' @importFrom dplyr %>%
as_df.matrix <- function(x, rownames='rownames', ...) {

  x %>%
    as.data.frame() %>%
    tibble::rownames_to_column('rownames') %>%
    tibble::as_tibble()

}

#' Turn a vector into a data frame
#'
#' Turn a vector into a data frame.
#'
#' @param x Vector
#' @param key_name Column name for keys
#' @param value_name Column name for values
#' @param keys If x is unnamed, use these as keys
.as_df_v <- function(x, key_name='key', value_name='value', keys=NULL, ...) {

  key_vector <- is.null(names(x)) %?% keys %:% names(x)
  key_vector <- key_vector %or% seq_along(x)

  df <- tibble::tibble(key=key_vector, value=unname(x))
  colnames(df) <- c(key_name, value_name)

  return ( df )

}

#' @inheritParams .as_df_v
#' @export
#' @rdname as_df
as_df.logical <- function(x, ...) {

  .as_df_v(x, ...)

}

#' @inheritParams .as_df_v
#'
#' @export
#' @rdname as_df
as_df.character <- function(x, ...) {

  .as_df_v(x, ...)

}

#' @inheritParams .as_df_v
#'
#' @export
#' @rdname as_df
as_df.numeric <- function(x, ...) {

  .as_df_v(x, ...)

}

#' Pivot data structure to long
#'
#' Force tidy format
#'
#' @param x Data structure
#'
#' @return Data frame in long format
pivot_long <- function(x, ...) UseMethod('pivot_long')

#' @rdname pivot_long
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' pivot_long(matrix(rnorm(9), 3), rownames='observation')
pivot_long.matrix <- function(x, varnames=names(dimnames(x)),
                              ...,
                              na.rm = FALSE, as.is = FALSE, value.name='value') {

  dots <- list(...)
  key <- dots$key %or% varnames[2] %or% 'key'
  value <- dots$value %or% value.name %or% 'value'
  rownames <- dots$rownames %or% varnames[1] %or% 'rownames'

  df <- as_df(x, rownames='rownames') %>%
    tidyr::gather(key=key,
                  value=value,
                  -rownames,
                  na.rm=na.rm)

  names(df)[names(df) == 'rownames'] <- rownames
  names(df)[names(df) == 'key']      <- key
  names(df)[names(df) == 'value']    <- value

  return ( df )

}

#' @rdname pivot_long
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' pivot_long(data.frame(A=rnorm(9), B=LETTERS[1:9], C=letters[1:9]), rownames='observation', id.vars='B')
pivot_long.data.frame <- function(x, id.vars=NULL, measure.vars=NULL,
                                  variable.name="key", ...,
                                  na.rm=FALSE, value.name="value",
                                  factorsAsStrings=TRUE) {

  dots <- list(...)
  key <- dots$key %or% variable.name
  value <- dots$value %or% value.name
  #rownames <- dots$rownames %or% varnames[1] %or% 'rownames'

  measure_vars <- colnames(x)
  if ( !is.null(id.vars) ) measure_vars <-  colnames(x)[! colnames(x) %in% id.vars]
  if ( !is.null(measure.vars) ) measure_vars <-  colnames(x)[colnames(x) %in% measure.vars]

  df_id <- as_df(x[ , ! colnames(x) %in% measure_vars, drop=FALSE], rownames='rownames')

  df <- as_df(x[ , measure_vars, drop=FALSE], rownames='rownames') %>%
    tidyr::gather(key=key,
                  value=value,
                  -rownames,
                  na.rm=na.rm) %>%
    dplyr::left_join(df_id, by='rownames') %>%
    dplyr::select(-rownames)


  #names(df)[names(df) == 'rownames'] <- rownames
  names(df)[names(df) == 'key']      <- key
  names(df)[names(df) == 'value']    <- value

  return ( df )

}

#' @rdname pivot_long
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' annotation_df <- data.frame(col_id=LETTERS[1:3], annotation=letters[1:3])
#' m <- matrix(rnorm(9), ncol=3)
#' colnames(m) <- annotation_df$col_id
#' m2 <- Matricks(m, cols_annotation=annotation_df)
#' pivot_long(m2)
#'
pivot_long.matricks <- function(x, varnames=names(dimnames(x)),
                              ...,
                              na.rm = FALSE, as.is = FALSE, value.name='value') {

  df <- pivot_long(unclass(x), varnames=varnames,
                   ...,
                   na.rm=na.rm, as.is=as.is, value.name=value.name)

  cols_annotations <- attr(x, 'cols_annotation')
  rows_annotations <- attr(x, 'rows_annotation')

  if ( !is.null(cols_annotations) ) df <- df %>% dplyr::left_join(cols_annotations, by=attr(x, 'cols_title'))
  if ( !is.null(rows_annotations) ) df <- df %>% dplyr::left_join(rows_annotations, by=attr(x, 'rows_title'))

  return ( df )

}

#' Pivot data structure to wide
#'
#' Force wide (matrix) format
#'
#' @param x Data frame
#'
#' @return Data frame in wide format Matricks
pivot_wide <- function(x, ...) UseMethod('pivot_wide')

#' @rdname pivot_wide
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' annotation_df <- data.frame(col_id=LETTERS[1:3], annotation=letters[1:3])
#' m <- matrix(rnorm(9), ncol=3)
#' colnames(m) <- annotation_df$col_id
#' m2 <- Matricks(m, cols_annotation=annotation_df)
#' d <- pivot_long(m2)
#' pivot_wide(d, 'col_id', 'rownames', 'value', cols_annotation='annotation')
#'
pivot_wide.data.frame <- function(x, key.vars, id.vars, value.var,
                                  rows_annotation=NULL,
                                  cols_annotation=NULL,
                                  fun.aggregate=mean,
                                  ...,
                                  fill=NULL, drop=TRUE) {

  dots <- list(...)
  key <- dots$key %or% key.vars #%or% 'key'
  value <- dots$value %or% value.var #%or% 'value'
  #rownames <- dots$rownames %or%  %or% 'rownames'

  x$col_id <- vapply(seq_len(nrow(x)), function(i) join(x[i, key, drop=TRUE], char='__'), 'a')
  # print(x$col_id)
  x$row_id <- vapply(seq_len(nrow(x)), function(i) join(x[i, id.vars, drop=TRUE], char='__'), 'a')
  # print(x$row_id)

  annotation_vars <- setdiff(colnames(x), c(key, id.vars, value))

  # print(annotation_vars)

  rowann_vars <- missing(rows_annotation) %?%
    setdiff(annotation_vars, c(cols_annotation, 'col_id')) %:%
                      c(rows_annotation, 'row_id')
  colann_vars <- missing(cols_annotation) %?%
    setdiff(annotation_vars, c(rows_annotation, 'row_id')) %:%
    c(cols_annotation, 'col_id')

  rowann <- x[ , rowann_vars, drop=FALSE] %>%
    dplyr::distinct()

  # print(rowann)

  if ( ncol(rowann) < 2) rowann <- NULL

  colann <- x[ , colann_vars, drop=FALSE] %>%
    dplyr::distinct()

  # print(colann)

  if ( ncol(colann) < 2 ) colann <- NULL

  df <- x %>%
    reshape2::acast(row_id ~ col_id,
                    value.var=value,
                    fun.aggregate=fun.aggregate,
                    fill=fill)

  m <- Matricks(df,
                rows_title=is.null(rowann) %?% NULL %:% 'row_id',
                cols_title=is.null(colann) %?% NULL %:% 'col_id',
                rows_annotation=rowann, cols_annotation=colann)

  return ( m )

}

