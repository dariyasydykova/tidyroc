#' Receiver operating characteristic (ROC) curve
#'
#' This function calculates true positive rate and false positive rate to plot an ROC curve.
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
#'   make_roc(predictor = .fitted, known_class = outcome) %>% # get values to plot an ROC curve
#'   ggplot(aes(x = fpr, y = tpr)) + # plot false positive rate against true positive rate
#'   geom_line()
#'
# this function calls `measure_perf()` to get true positive and false positive rates
# this function works on grouped data-frames
make_roc <- function(data, ...) {
  dplyr::group_map(data, make_roc_ungrouped, ...)
}

# this function calls `measure_perf()` to get true positive and false positive rates
# this function works on ungrouped data-frames
make_roc_ungrouped <- function(data, key, predictor, known_class) {
  # use tidy eval
  predictor <- rlang::enquo(predictor)
  known_class <- rlang::enquo(known_class)

  pred_values <- rlang::eval_tidy(predictor, data)
  known_values <- rlang::eval_tidy(known_class, data)

  # get binary classification values
  df <- measure_perf(
    data = data,
    predictor = pred_values,
    known_class = known_values
  )

  # add true positive rate and false positive rate to the data-frame `data`
  df %>%
    dplyr::select(-c(tnr, fnr, ppv)) %>% # remove extra columns to keep data-frames simpler
    dplyr::add_row(tpr = 0, fpr = 0) %>% # add true positive = 0 and false positive = 0 to make sure an ROC curve is always anchored at 0, 0
    dplyr::arrange(fpr, tpr) # order output by false positive and then true positive rate to plot an ROC curve correctly
}
