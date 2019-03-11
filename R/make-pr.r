#' Precision-recall curve
#'
#' This function calculates precision and recall to plot a precision-recall curve
#' @param data data-frame that contains fitted values and known outcomes
#' @param predictor column in `data` that contains fitted values
#' @param known_class column in `data` that contains true or actual classification
#'
#' @keywords
#' @export
#' @examples
#' library(tidyverse)
#' library(broom)
#' library(tidyroc)
#'
#' # get `biopsy` dataset from `MASS`
#' data(biopsy, package = "MASS")
#'
#' # change column names from `V1`, `V2`, etc. to informative variable names
#' colnames(biopsy) <-
#'   c(
#'     "ID",
#'     "clump_thickness",
#'     "uniform_cell_size",
#'     "uniform_cell_shape",
#'     "marg_adhesion",
#'     "epithelial_cell_size",
#'     "bare_nuclei",
#'     "bland_chromatin",
#'     "normal_nucleoli",
#'     "mitoses",
#'     "outcome"
#'   )
#'
#' # fit a logistic regression model to predict tumour type
#' glm(outcome ~ clump_thickness + uniform_cell_shape,
#'   family = binomial,
#'   data = biopsy
#' ) %>%
#'   augment() %>% # use broom to add glm output to the original data frame
#'   make_pr(predictor = .fitted, known_class = outcome) %>% # get values to plot an ROC curve
#'   ggplot(aes(x = recall, y = precision)) + # plot false positive rate against true positive rate
#'   geom_line()
#'
# this function calls `measure_perf()` to get precision and recall
# this function works on grouped data-frames
make_pr <- function(data, ...) {
  group_map(data, make_pr_ungrouped, ...)
}

# this function calls `measure_perf()` to get precision and recall
# this function works on ungrouped data-frames
make_pr_ungrouped <- function(data, key, predictor, known_class) {
  # use tidy eval
  predictor <- rlang::enquo(predictor)
  known_class <- rlang::enquo(known_class)

  pred_values <- rlang::eval_tidy(predictor, data)
  known_values <- rlang::eval_tidy(known_class, data)

  # get binary classification values
  df <- measure_perf(data = data, predictor = pred_values, known_class = known_values)

  # add recall and precision to the data-frame `data`
  df %>%
    dplyr::select(-c(tnr, fpr, fnr), recall = tpr, precision = ppv) %>% # remove extra columns to keep data-frames simpler
    dplyr::add_row(recall = 0, precision = 1) %>% # add recall = 0 and precision = 1 to make sure a precision-recall curve is anchored at 0, 1
    dplyr::arrange(recall, desc(precision)) # order output by recall (ascending order) and then precision (descending order) to plot the precision-recall curve correctly
}
