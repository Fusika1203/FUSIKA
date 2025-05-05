#' Plot a customized violin plot (FUSIKA style)
#'
#' @param data A data frame containing the data.
#' @param group_col A string, name of the column with group labels (x-axis).
#' @param value_col A string, name of the column with numeric values (y-axis).
#' @param title A string, plot title (usually gene name).
#' @param show_signif Logical, whether to show statistical comparisons (default = TRUE).
#' @param signif_offset Numeric, how much to offset significance labels above max (used if y_manual is NULL).
#' @param y_manual Optional vector of y-positions for significance labels (same length as number of comparisons).
#' @param tip_length Numeric, length of bracket tip (default = 0.01)
#'
#' @return A ggplot object of the violin plot.
#' @import ggplot2
#' @import ggpubr
#' @export
plot_violin_fusika <- function(data, group_col, value_col, title = "Gene",
                               show_signif = TRUE, signif_offset = 1,
                               y_manual = NULL, tip_length = 0.01) {
  library(ggplot2)
  library(ggpubr)

  # Validate input
  stopifnot(is.data.frame(data))
  stopifnot(group_col %in% names(data))
  stopifnot(value_col %in% names(data))

  data[[group_col]] <- factor(data[[group_col]])
  data[[value_col]] <- as.numeric(data[[value_col]])

  # Custom quantile functions
  q1 <- function(x) quantile(x, 0.25)
  q3 <- function(x) quantile(x, 0.75)

  # Build base plot
  p <- ggplot(data, aes_string(x = group_col, y = value_col, fill = group_col)) +
    geom_violin(trim = FALSE, scale = "area", color = "black", linewidth = 0.5) +
    geom_jitter(width = 0.15, size = 0.8, color = "white") +
    stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "white", linewidth = 0.7) +
    stat_summary(fun = q1, geom = "crossbar", width = 0.5, color = "black", linetype = "dashed", linewidth = 0.3) +
    stat_summary(fun = q3, geom = "crossbar", width = 0.5, color = "black", linetype = "dashed", linewidth = 0.3) +
    theme_classic() +
    scale_fill_manual(values = c("blue", "red", "green4", "orange", "purple", "brown")[1:length(levels(data[[group_col]]))]) +
    labs(title = title, y = "Log2(exp.)", x = NULL) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.text = element_text(size = 12),
      axis.title.y = element_text(size = 14),
      legend.position = "none"
    )

  # Add significance comparisons if requested
  if (show_signif && length(unique(data[[group_col]])) > 1) {
    comps <- combn(levels(data[[group_col]]), 2, simplify = FALSE)

    if (is.null(y_manual)) {
      # Tự động tính y.position
      max_per_group <- aggregate(data[[value_col]], list(data[[group_col]]), max)
      names(max_per_group) <- c("group", "ymax")

      y.position <- sapply(comps, function(comp) {
        max(max_per_group$ymax[max_per_group$group %in% comp]) + signif_offset
      })
    } else {
      # Dùng y_manual nếu có
      y.position <- y_manual
      if (length(y.position) != length(comps)) {
        stop("Length of y_manual must match number of group comparisons.")
      }
    }

    p <- p + stat_compare_means(
      mapping = aes_string(x = group_col, y = value_col),
      comparisons = comps,
      label = "p.signif",
      hide.ns = TRUE,
      method = "wilcox.test",
      y.position = y.position,
      tip.length = tip_length
    )
  }

  return(p)
}

