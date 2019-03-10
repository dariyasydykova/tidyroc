#' Precision-recall curve
#'
#' this function calculates precision and recall to plot a precision-recall curve
#' @param probabilities
#'
#' @keywords
#' @export
#' @examples

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
