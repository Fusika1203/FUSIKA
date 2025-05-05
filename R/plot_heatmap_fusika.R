#' Plot a customized heatmap (FUSIKA style)
#'
#' @param data A numeric matrix (samples x genes).
#' @param group A vector of group labels (length = nrow(data)).
#'
#' @return A ComplexHeatmap object.
#' @examples
#' set.seed(123)
#' mat <- matrix(rnorm(40*20), nrow = 40, ncol = 20)
#' group <- rep(c("Group1", "Group2"), each = 20)
#' plot_heatmap_fusika(mat, group)
#'
#' @import ComplexHeatmap
#' @import circlize
#' @export
plot_heatmap_fusika <- function(data, group) {
  library(ComplexHeatmap)
  library(circlize)
  
  # Kiểm tra input
  if (!is.matrix(data)) stop("Input data must be a matrix.")
  if (length(group) != nrow(data)) stop("Length of group must match number of samples (rows) in data.")
  
  # Xác định màu cho từng nhóm
  group_levels <- unique(group)
  n_group <- length(group_levels)
  
  base_colors <- c("#FF0000", "#0000FF", "#00CC00", "#FF9900", "#9900FF", "#FF66CC", "#00CCCC", "#999900") # Red, Blue, Green, Orange, Purple, Pink, Cyan, Olive
  if (n_group > length(base_colors)) {
    more_colors <- grDevices::rainbow(n_group - length(base_colors))
    color_palette <- c(base_colors, more_colors)
  } else {
    color_palette <- base_colors[1:n_group]
  }
  
  group_colors <- setNames(color_palette, group_levels)
  
  # Tạo annotation
  ha = rowAnnotation(
    category = group,
    col = list(category = group_colors),
    show_annotation_name = FALSE,
    annotation_width = unit(5, "mm")
  )
  
  # Vẽ heatmap
  Heatmap(
    data,
    name = "Expression",
    col = colorRamp2(c(-2, 0, 3), c("blue", "white", "red")),
    cluster_rows = FALSE,
    cluster_columns = TRUE,
    show_row_names = FALSE,
    show_column_names = TRUE,
    left_annotation = ha,
    heatmap_legend_param = list(
      title = "Z-score",
      at = c(-2, -1, 0, 1, 2, 3),
      color_bar = "continuous"
    ),
    border = FALSE,
    column_title = NULL,
    row_title = NULL
  )
}