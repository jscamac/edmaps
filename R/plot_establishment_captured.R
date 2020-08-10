#' Plots the cumulative proportion of establishment likelihood captured
#'
#' Plots the cumulative proportion of establishment likelihood captured as a 
#' greater number of top ranking risk cells is considered.
#' 
#' @param data A \code{data.frame} as derived from 
#'   \code{\link{captured_by_ncells}}.
#' @param xlab Character. x axis label.
#' @param ylab Character. y axis label.
#' @param legend_title Character. Optional legend title.
#' @param legend Legend position. Either a position name (one of 
#'   \code{"none"}, \code{"top"}, \code{"bottom"}, \code{"left"}, or 
#'   \code{"right"}, or a vector of two normalised coordinates ranging from 0 
#'   to 1, e.g. \code{c(1, 0)} (for bottom-right).
#' @param prop_line Numeric. Proportion in which to add a vertical line. 
#'   Default is NULL (no line).
#' @param y_limit Numeric vector giving the minimum and maximum y-axis limits.
#'   If omitted this will be determined based on the data.
#' @param width Width of plot. If not defined will use size of current graphic
#'   device.
#' @param height Height of plot. If not defined will use size of current 
#'  graphic device.
#' @param units Character. Units corresponding to \code{height} and 
#'   \code{width}. Can be \code{"in"}, \code{"cm"}, or \code{"mm"}. Default is 
#'   inches (\code{"in"}).
#' @param outfile Character. Output image file path. Containing directory
#'   will be created recursively if it does not already exist.
#' @return An image is written to \code{outfile} if provided, and otherwise a
#'   \code{ggplot} object is returned.
#' @importFrom ggplot2 ggplot aes geom_line scale_color_brewer xlab scale_y_continuous ylab theme_classic theme geom_segment ggsave
#' @importFrom dplyr group_by filter
#' @importFrom magrittr "%>%"
#' @export
plot_establishment_captured <- function(data, xlab = "Number of cells", 
  ylab = "Proportion", legend_title = NULL, legend = "right",  prop_line, 
  y_limit = NULL, width = NA, height = NA, units = c("in", "cm", "mm"), 
  outfile) {
  
  units <- match.arg(units)
  
  p1 <- ggplot2::ggplot(
    data, ggplot2::aes(x = ncell, y = proportion, col = map)
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_color_brewer(legend_title, type ="qual",palette = "Set1") +
    ggplot2::xlab(xlab) +
    ggplot2::scale_y_continuous(expand = c(0,0),limits = y_limit) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend)
  
  # Add vertical proportion line
  if(!missing(prop_line)) {
    prop_line <- data %>% 
      dplyr::group_by(map) %>% 
      dplyr::filter(
        abs(proportion - prop_line) == min(abs(proportion - prop_line))
      )
    
    p1 <- p1 + 
      ggplot2::geom_segment(
        data = prop_line, 
        ggplot2::aes(x = ncell, y = proportion, xend = ncell, yend = -Inf), 
        linetype ="dotted",colour ="black", na.rm=TRUE
      )
  }
  
  # outfile supplied
  if(!missing(outfile)) {
    
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    ggplot2::ggsave(filename =  outfile, width = width, height = height, 
                    units = units, plot = p1)
  } else {
    p1
  }
}
