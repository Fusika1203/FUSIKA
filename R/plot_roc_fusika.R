#' Plot multiple ROC curves in one plot (FUSIKA style)
#'
#' @param data_list A list of data.frames (multi-dataset mode), or a single data.frame (multi-gene mode).
#' @param met_col Name of the column with true labels.
#' @param pred_cols Vector of column names with prediction scores (multi-gene) or a single name (multi-dataset).
#' @param labels Optional vector of labels (e.g., gene names or dataset names).
#' @param colors Optional vector of colors for ROC curves.
#' @param title Main title of the ROC plot.
#' @param legacy.axes Whether to use legacy x-axis (1 - specificity).
#' @param print_auc Whether to display AUC values in the legend.
#'
#' @return A base R plot with ROC curves.
#' @import pROC
#' @export
plot_roc_fusika <- function(data_list, met_col, pred_cols, labels = NULL, colors = NULL,
                            title = "ROC Curve", legacy.axes = TRUE, print_auc = TRUE) {
  library(pROC)

  multi_dataset <- is.list(data_list) && all(sapply(data_list, is.data.frame))

  if (multi_dataset) {
    n <- length(data_list)
    if (length(colors) < n) colors <- rainbow(n)
    if (is.null(labels)) labels <- paste0("Model_", seq_len(n))

    roc_obj <- roc(data_list[[1]][[met_col]], data_list[[1]][[pred_cols]])
    plot(roc_obj, legacy.axes = legacy.axes, col = colors[1], lwd = 2,
         main = title, xlab = "1 - Specificity", ylab = "Sensitivity")

    if (print_auc) {
      auc_val <- round(auc(roc_obj), 3)
      legend_labels <- paste0(labels[1], ", AUC = ", auc_val)
    } else {
      legend_labels <- labels[1]
    }

    for (i in 2:n) {
      roc_i <- roc(data_list[[i]][[met_col]], data_list[[i]][[pred_cols]])
      plot(roc_i, add = TRUE, col = colors[i], lwd = 2)
      if (print_auc) {
        legend_labels <- c(legend_labels, paste0(labels[i], ", AUC = ", round(auc(roc_i), 3)))
      } else {
        legend_labels <- c(legend_labels, labels[i])
      }
    }

    legend("bottomright", legend = legend_labels, col = colors, lwd = 2)

  } else {
    df <- data_list
    n <- length(pred_cols)
    if (length(colors) < n) colors <- rainbow(n)
    if (is.null(labels)) labels <- pred_cols

    roc_obj <- roc(df[[met_col]], df[[pred_cols[1]]])
    plot(roc_obj, legacy.axes = legacy.axes, col = colors[1], lwd = 2,
         main = title, xlab = "1 - Specificity", ylab = "Sensitivity")

    if (print_auc) {
      auc_val <- round(auc(roc_obj), 3)
      legend_labels <- paste0(labels[1], ", AUC = ", auc_val)
    } else {
      legend_labels <- labels[1]
    }

    for (i in 2:n) {
      roc_i <- roc(df[[met_col]], df[[pred_cols[i]]])
      plot(roc_i, add = TRUE, col = colors[i], lwd = 2)
      if (print_auc) {
        legend_labels <- c(legend_labels, paste0(labels[i], ", AUC = ", round(auc(roc_i), 3)))
      } else {
        legend_labels <- c(legend_labels, labels[i])
      }
    }

    legend("bottomright", legend = legend_labels, col = colors, lwd = 2)
  }
}
