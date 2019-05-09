#' Add log axis
#'
#' @export
#'
log_plot <- function(x, ...) UseMethod('log_plot')

#' @param x A data frame
#' @param axes Which axes to log-transform
#'
#' @return \code{ggplot2} object
#'
#' @export
#' @rdname log_plot
log_plot.gg <- function(x, axes='x') {

  sides <- c(x='b', y='l', xy='bl')

  if ( grepl('x', axes) ) x <- x + ggplot2::scale_x_log10(labels=expo_log10)
  if ( grepl('y', axes) ) x <- x + ggplot2::scale_y_log10(labels=expo_log10)

  x <- x + ggplot2::annotation_logticks(sides=sides[axes])

  return ( x )

}

#' Plot a dose response curve
#'
#' @export
plot_dose_response <- function(x, ...) UseMethod('plot_dose_response')

#' @param x A data frame
#' @param xvar X-axis column
#' @param yvar Y-axis column
#' @param wt String to match for WT strains
#' @param highlights Data frame for filtering high;ighted conditions
#' @param group Grouping
#' @param facet Faceting
#' @param bg_color Color for bulk of strains
#' @param wt_color Color for WT strains
#' @param palette Palette for highlighted lines
#' @param highlight_size Size of highlighted interaction lines
#' @param filename Filename to save
#' @param panel_width Width of each panel in inches
#' @param panel_height Height of each panel in inches
#' @param legend_width Width of legend in inches
#'
#' @return ggplot2 object
#'
#' @export
#' @importFrom dplyr %>%
#'
#' @rdname plot_dose_response
plot_dose_response.data.frame <- function(x, xvar, yvar, wt='H37Rv', highlights=NULL,
                                          group='paste(strain, compound)',
                                          facet='compound_name',
                                          bg_color='grey80',
                                          wt_color='grey40',
                                          palette='Dark2',
                                          highlight_size=1,
                                          alpha=1,
                                          labeller=ggplot2::as_labeller(function(x) x),
                                          scales='free_x',
                                          filename=NULL,
                                          panel_width=2, panel_height=2, legend_width=1) {

  if ( is.null(highlights) ) {
    highlight_data <- x
    legend_width <- 0
  } else {
    highlight_data <- x %>% dplyr::semi_join(highlights)
  }

  wt_data <- x %>% dplyr::filter(grepl(wt, strain))

  n_panels <- x[ , facet] %>% distinct() %>% nrow()
  n_row <- ceiling(sqrt(n_panels))
  n_col <- ceiling(n_panels / nrow)

  g <- (ggplot2::ggplot(x,
                        ggplot2::aes_string(xvar, yvar, group=group)) +
          ggplot2::geom_hline(yintercept=0) +
          ggplot2::geom_line(color=bg_color, alpha=alpha) +
          ggplot2::geom_line(data=wt_data, color=wt_color, size=highlight_size, alpha=alpha) +
          ggplot2::geom_line(ggplot2::aes(color=strain), highlight_data, size=highlight_size, alpha=alpha) +
          ggplot2::scale_color_brewer(palette=palette) +
          ggplot2::facet_wrap(facet, scales=scales, labeller=labeller) %>%
          log_plot(x)) +
    ggplot2::labs(x=xvar, y=yvar)

  if (!is.null(filename)) {

    fig_width <- panel_width * n_col + legend_width
    fig_height <- panel_width * n_row

    println('Saving plot as', filename, '...')
    ggplot2::ggsave(filename, plot=g, width=fig_width, height=fig_height)

  }

  return ( g )

}

map_labels_to_palette <- function(labels, palette) {

  extended_palette <- rep(palette, times=ceiling(length(labels) / length(palette)))[seq_len(length(labels))]

  setNames(extended_palette, labels)

}

#' Plot a heat map
#'
#' @param x A matrix, Matricks, or data frame
#'
#' @return A plot
#' @export
heat_map <- function(x, ...) UseMethod('heat_map')

#' @param filename File to save as
#' @param width File width
#' @param height File height
#' @param hclustfun Function for clustering
#' @param shrink_labels Length-2 numeric vector for how much to shrink row and column labels
#' @param heat_palette RColorBrewer palette for values
#' @param row_palette RColorBrewer palette for row side-information
#' @param col_palette RColorBrewer palette for column side-information
#' @param ... Passed to \code{gplots::heatmap.2}
#'
#' @export
#' @rdname heat_map
#'
#' @examples
#' m <- matrix(rnorm(100), ncol=10)
#' col_labels <- rep(LETTERS[1:5], 2)
#' heat_map(m, col_side_info=col_labels, show_row_dendro=FALSE, cluster_rows=FALSE)
#'
heat_map.matrix <- function(x, filename=NULL, width=12, height=12,
                            cluster_rows=TRUE,
                            cluster_cols=TRUE,
                            show_row_dendro=TRUE,
                            show_col_dendro=TRUE,
                            hclustfun=function(x) hclust(x, 'ward.D2'),
                            distfun=function(x) dist(x),
                            shrink_labels=c(1, 1),
                            heat_palette='RdBu',
                            row_palette='Dark2',
                            col_palette='Dark2',
                            row_side_info=NULL,
                            col_side_info=NULL,
                            trace='none',
                            trace_color='black',
                            ...) {

  colors <- RColorBrewer::brewer.pal(9, heat_palette)

  row_dendro <- FALSE

  if ( cluster_rows ) {
    row_hclust <- hclustfun(distfun(x))
    row_dendro <- as.dendrogram(row_hclust)
  }

  col_dendro <- FALSE

  if ( cluster_cols ) {
    col_hclust <- hclustfun(distfun(t(x)))
    col_dendro <- as.dendrogram(col_hclust)
  }

  if ( ! is.null(row_side_info) ) {

    ordering <- cluster_rows %?% row_hclust$order %:% seq_along(row_side_info)

    row_side_labels <- unique(row_side_info[ordering])
    row_colors      <- RColorBrewer::brewer.pal(8, row_palette)
    row_color_map   <- unname(map_labels_to_palette(row_side_labels, row_colors)[row_side_info])

  } else {
    row_color_map <- rep('white', nrow(x))
  }

  if ( ! is.null(col_side_info) ) {

    ordering <- cluster_cols %?% col_hclust$order %:% seq_along(col_side_info)

    col_side_labels <- unique(col_side_info[ordering])
    col_colors      <- RColorBrewer::brewer.pal(8, col_palette)
    col_color_map   <- unname(map_labels_to_palette(col_side_labels, col_colors)[col_side_info])

  } else {
    col_color_map <- rep('white', ncol(x))
  }

  if ( !is.null(filename) ) {

    pdf(filename, width=width, height=height)
    on.exit(dev.off())

  }

  dendro_option <- (show_row_dendro & show_col_dendro) %?%
    'both' %:% (show_row_dendro %?%
    'row' %:% (show_col_dendro %?% 'column' %:% 'none'))

  g <- gplots::heatmap.2(x,
                         Rowv=row_dendro,
                         Colv=col_dendro,
                         dendrogram=dendro_option,
                         cexRow=shrink_labels[1],
                         cexCol=shrink_labels[2],
                         trace=trace, tracecol=trace_color,
                         col=colors,
                         ColSideColors=col_color_map,
                         RowSideColors=row_color_map,
                         key.title='',
                         ...)

  return ( invisible(g) )

}

#' @param x A data frame
#' @inheritParams heat_map.matrix
#'
#' @export
#' @rdname heat_map
#'
#' @examples
#' m <- as_df(matrix(rnorm(100), ncol=4))
#' col_labels <- rep(LETTERS[1:2], 2)
#' heat_map(m, col_side_info=col_labels, show_row_dendro=FALSE, cluster_rows=FALSE)
#'
heat_map.data.frame <- function(x, filename=NULL, width=12, height=12,
                                hclustfun=function(x) hclust(x, 'ward.D2'),
                                shrink_labels=c(1, 1),
                                heat_palette='RdBu',
                                row_palette='Dark2',
                                col_palette='Dark2',
                                row_side_info=NULL,
                                col_side_info=NULL,
                                ...) {

  x <- x[ , sapply(x, is.numeric)]

  heat_map(as.matrix(x),
           filename=filename, width=width, height=height,
           hclustfun=hclustfun,
           shrink_labels=shrink_labels,
           heat_palette=heat_palette,
           row_palette=row_palette,
           col_palette=col_palette,
           row_side_info=row_side_info,
           col_side_info=col_side_info,
           ...)

}

#' @param x A \code{Matricks} object
#' @inheritParams heat_map.matrix
#'
#' @export
#' @rdname heat_map
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' annotation_df <- data.frame(col_id=LETTERS[1:4], annotation=rep(letters[1:2], 2))
#' m <- matrix(rnorm(100), ncol=4)
#' colnames(m) <- annotation_df$col_id
#' m2 <- Matricks(m, cols_annotation=annotation_df)
#' heat_map(m2, col_side_info='annotation')
#'
heat_map.matricks <- function(x, filename=NULL, width=12, height=12,
                                hclustfun=function(x) hclust(x, 'ward.D2'),
                                shrink_labels=c(1, 1),
                                heat_palette='RdBu',
                                row_palette='Dark2',
                                col_palette='Dark2',
                                row_side_info=NULL,
                                col_side_info=NULL,
                                ...) {

  rowann <- attr(x, 'rows_annotation')
  row_color_labels <- (is.null(row_side_info) && is.null(rowann)) %?%
    NULL %:% as.character(dplyr::pull(rowann, row_side_info))

  colann <- attr(x, 'cols_annotation')
  col_color_labels <- (is.null(col_side_info) && is.null(colann)) %?%
    NULL %:% as.character(dplyr::pull(colann, col_side_info))

  heat_map(unclass(x),
           filename=filename, width=width, height=height,
           hclustfun=hclustfun,
           shrink_labels=shrink_labels,
           heat_palette=heat_palette,
           row_palette=row_palette,
           col_palette=col_palette,
           row_side_info=row_color_labels,
           col_side_info=col_color_labels,
           ...)

}
