#' Missing data pattern
#'
#' Obtain a plot of the pattern of missing data and/or return the pattern as a
#' matrix.
#' @param data data frame
#' @param color vector of length two, that specifies the colour used to indicate
#'              observed and missing values (in that order)
#' @param border colour of the grid
#' @param plot logical; should the missing data pattern be plotted?
#'             (default is \code{TRUE})
#' @param pattern logical; should the missing data pattern be returned as
#'                matrix? (default is \code{FALSE})
#' @param print_xaxis,print_yaxis logical; should the x-axis (below the plot)
#'                                and y-axis (on the right) be printed?
#' @param ylab y-axis label
#' @inheritParams ggplot2::theme
#' @importFrom rlang .data
#' @param sort_columns logical; should the columns be sorted by number of missing
#'                    values? (default is `TRUE`)
#' @param ... optional additional parameters, currently not used
#'
#' @seealso See the vignette
#' \href{https://nerler.github.io/JointAI/articles/VisualizingIncompleteData.html}{Visualizing Incomplete Data}
#' for more examples.
#'
#' @note This function requires the
#' \href{https://CRAN.R-project.org/package=ggplot2}{\strong{ggplot2}} package
#' to be installed.
#' @export
#'
#' @examples
#' op <- par(mar = c(3, 1, 1.5, 1.5), mgp = c(2, 0.6, 0))
#' md_pattern(wideDF)
#' par(op)
#'
md_pattern <- function(data, color = c(grDevices::grey(0.1),
                                       grDevices::grey(0.7)),
                       border = grDevices::grey(0.5),
                       plot = TRUE, pattern = FALSE, print_xaxis = TRUE,
                       ylab = 'Number of observations per pattern',
                       print_yaxis = TRUE, legend.position = 'bottom',
                       sort_columns = TRUE, ...) {

  naX <- ifelse(is.na(data), 0, 1)
  unaX <- unique(naX)

  NApat <- apply(naX, 1, paste, collapse = "")
  NAupat <- apply(unaX, 1, paste, collapse = "")

  Nmis <- colSums(naX == 0)

  tab <- table(NApat)
  Npat <- tab[match(NAupat, names(tab))]

  # sort rows
  unaX <- unaX[order(Npat, decreasing = TRUE), ]
  Npat <- sort(Npat, decreasing = TRUE)
  rownames(unaX) <- rev(seq_len(nrow(unaX)))

  if (sort_columns) {
    vars <- colnames(unaX)[order(Nmis)]
    unaX <- unaX[, order(Nmis)]
    Nmis <- sort(Nmis)
  } else {
    vars <- colnames(unaX)
  }
  colnames(unaX) <- seq_len(ncol(unaX))


  if (plot) {
    if (!requireNamespace('ggplot2', quietly = TRUE))
      errormsg("This function requires the 'ggplot2' package to be installed.")

    if (print_yaxis == FALSE) {
      ylab <- ''
    }

    p <- ggplot2::ggplot(melt_matrix(unaX),
                         ggplot2::aes(as.numeric(.data$V2),
                                      as.numeric(.data$V1),
                                      fill = as.character(.data$value))) +
      ggplot2::geom_tile(color = border) +
      ggplot2::scale_y_continuous(position = 'right',
                         breaks = rev(seq_len(length(Npat))),
                         labels = if (print_yaxis)
                           Npat else rep('', length(Npat)),
                         expand = c(0,0)) +
      ggplot2::scale_x_continuous(position = 'top',
                         breaks = seq_len(ncol(unaX)),
                         labels = vars,
                         sec.axis = if (print_xaxis)
                           ggplot2::sec_axis(~.,
                                    name = 'Number of missing values',
                                    breaks = seq_len(ncol(unaX)),
                                    labels = Nmis) else ggplot2::waiver(),
                         expand = c(0, 0)) +
      ggplot2::scale_fill_manual(name = '',
                        limits = c("1", "0"),
                        values = color,
                        labels = c('observed', 'missing')) +
      ggplot2::theme(legend.position = legend.position,
            panel.background = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.text.x.top = ggplot2::element_text(angle = -90, hjust = 1)) +
      ggplot2::ylab(ylab) +
      ggplot2::xlab('')
    # print(p)
  }

  if (pattern) {
    rownames(unaX) <- seq_len(nrow(unaX))
    colnames(unaX) <- vars
    md_pat <- rbind(cbind(unaX, Npat = Npat),
                    Nmis = c(Nmis, sum(Nmis)))
  }

  if (plot & pattern) {
    list(plot = p, pattern = md_pat)
  } else if (plot) {
    p
  } else if (pattern) {
    md_pat
  }
}
