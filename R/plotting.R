#' Convert to exponent 10
#'
#' @param x Numeric
#'
#' @return Formatted expression
#'
#' @export
expo_log10 <- scales::trans_format('log10', scales::math_format(10^.x))

#' Add log axis to ggplot2
log_plot <- function(x, ...) UseMethod('log_plot')

#' Add log axis to ggplot2
#'
#' Add log axis to ggplot2.
#'
#' @param x A data frame
#' @param axes Which axes to log-transform
#'
#' @return ggplot2 object
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
plot_dose_response <- function(x, ...) UseMethod('plot_dose_response')

#' Plot a dose response curve
#'
#' Create a new factor from two existing factors, where the new factor's levels
#' are the union of the levels of the input factors.
#'
#' @param x A data frame
#' @param xvar X-axis column
#' @param yvar Y-axis column
#' @param wt
#' @param highlights
#' @param group
#' @param facet
#' @param bg_color
#' @param wt_color
#' @param palette
#' @param highlight_size
#' @param filename
#' @param panel_width
#' @param panel_height
#' @param legend_width
#'
#' @return ggplot2 object
#'
#' @export
#' @importFrom dplyr %>%
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
                                          panel_width=2, panel_height=2, legend_width=1
) {

  if (is.null(highlights)) {
    highlight_data <- x
  } else {
    highlight_data <- x %>% dplyr::semi_join(highlights)
  }

  wt_data <- x %>% dplyr::filter(grepl(wt, strain))

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
  print(g)

}

#' Plot a heat map
heat_map <- function(x, ...) UseMethod('heat_map')

#' Plot a heat map.
#'
#' @param x A matrix
#'
#' @return A plot
#'
#' @export
#' @rdname heat_map
heat_map.matrix <- function(x, filename=NULL, width=12, height=12,
                            hclustfun=function(x) hclust(x, 'ward.D2'),
                            shrink_labels=c(1, 1),
                            heat_palette='RdBu',
                            row_palette=NULL,
                            col_palette=NULL,
                            ...) {

  colors <- RColorBrewer::brewer.pal(9, heat_palette)


  if (!is.null(filename) ) {

    pdf(filename, width=width, height=height)
    on.exit(dev.off())

  }

  g <- gplots::heatmap.2(x,
                         trace='none', tracecol='black',
                         col=colors,
                         hclustfun=hclustfun,
                         cexRow=shrink_labels[1],
                         cexCol=shrink_labels[2],
                         key.title='',
                         ...)

  return ( invisible(g) )

}

#' @param x A data frame
#'
#' @return A plot
#' @export
#' @rdname heat_map
heat_map.data.frame <- function(x, filename=NULL, width=12, height=12,
                                hclustfun=function(x) hclust(x, 'ward.D2'),
                                shrink_labels=c(1, 1),
                                heat_palette='RdBu',
                                row_palette=NULL,
                                col_palette=NULL,
                                ...) {

  heat_map(as.matrix(x),
           filename=filename, width=width, height=height,
           hclustfun=hclustfun,
           shrink_labels=shrink_labels,
           heat_palette=heat_palette,
           row_palette=row_palette,
           col_palette=col_palette,
           ...)

}
