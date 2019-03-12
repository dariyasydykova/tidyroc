#' Measure model performance
#'
#' This function evaluates a binary classification model. The function calculates true positive rate, false positive rate, true negative rate, false negative rate, and positive predictive value. These values are added to the data-frame provided in `data`.
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
#'  c("ID",
#'    "clump_thickness",
#'    "uniform_cell_size",
#'    "uniform_cell_shape",
#'    "marg_adhesion",
#'    "epithelial_cell_size",
#'    "bare_nuclei",
#'    "bland_chromatin",
#'    "normal_nucleoli",
#'    "mitoses",
#'    "outcome")
#'
#' # fit a logistic regression model to predict tumour type
#' glm(outcome ~ clump_thickness + uniform_cell_shape,
#'    family = binomial,
#'    data = biopsy
#' ) %>%
#'  augment() %>% # use broom to add glm output to the original data frame
#'  measure_perf(predictor = .fitted, known_class = outcome)
#'

# this functions calculates the true positive rate (tpr)
# pred_values = predictors or fitted values
# pos_pred = predictor values for known positive outcomes
# npos = total number of known positives
calc_tpr <- function(pred_values, pos_pred, npos){
  true_pos_rate <- sapply(
    pred_values,
    function(x) if (is.na(x)) {
      NA
    } else{
      sum(pos_pred >= x, na.rm = TRUE) / npos
    }
  )

  true_pos_rate
}

# this functions calculates the false positive rate (fpr)
# pred_values = predictors or fitted values
# neg_pred = predictor values for known negative outcomes
# nneg = total number of known negatives
calc_fpr <-function(pred_values, neg_pred, nneg){
  false_pos_rate <- sapply(
    pred_values,
    function(x) if (is.na(x)) {
      NA
    } else {
      sum(neg_pred >= x, na.rm = TRUE) / nneg
    }
  )

  false_pos_rate
}

# this functions calculates the true negative rate (tnr)
calc_tnr <- function(pred_values, neg_pred, nneg){
  true_neg_rate <- sapply(
    pred_values,
    function(x) if (is.na(x)) {
      NA
    } else {
      sum(neg_pred < x, na.rm = TRUE) / nneg
    }
  )

  true_neg_rate
}

# this functions calculates the false negative rate (fnr)
calc_fnr <-function(pred_values, pos_pred, npos){
  false_neg_rate <- sapply(
    pred_values,
    function(x) if (is.na(x)) {
      NA
    } else {
      sum(pos_pred < x, na.rm = TRUE) / npos
    }
  )

  false_neg_rate
}

# this functions calculates the positive predictive value (ppv)
calc_ppv <- function(pred_values, pos_pred, neg_pred){
  pos_pred_value <- sapply(
    pred_values,
    function(x) if (is.na(x)) {
      NA
    } else {
      sum(pos_pred >= x, na.rm = TRUE) / (sum(pos_pred >= x, na.rm = TRUE) + sum(neg_pred >= x, na.rm = TRUE))
    }
  )

  pos_pred_value
}

# this function uses predictor values and known classification to measure performace of a binary classification model
# this function works on grouped data-frames
measure_perf <- function(data, ...) {
  group_map(data, measure_perf_ungrouped, ...)
}

# this function uses predictor values and known classification to measure performace of a binary classification model
# this function works on data-frames that are not grouped
measure_perf_ungrouped <- function(data, key, predictor, known_class) {

  # use tidy eval
  predictor <- rlang::enquo(predictor)
  known_class <- rlang::enquo(known_class)

  pred_values <- rlang::eval_tidy(predictor, data)
  known_values <- rlang::eval_tidy(known_class, data)

  # convert known outcomes to numeric to perform calculations on it
  # positives = 1, negatives = 0
  pos_values <- as.numeric(factor(known_values)) - 1 # use factors to match glm() output

  # if any of predictor = NA, change to positive = 1 or positive = 0 to positive = NA
  # this ensures that the positive or negative is omitted from further calculations
  pos_values <- ifelse(is.na(pred_values), NA, pos_values)

  # if any of known_class = NA, change predictor to predictor = NA
  # this ensure that tpr, fpr, tnr, fnr, and ppv become NA
  pred_values <- ifelse(is.na(pos_values), NA, pred_values)

  # count total positives and total negatives to calculate true positive and false positive rates
  npos <- sum(pos_values, na.rm = TRUE) # total known positives
  nneg <- sum(1 - pos_values, na.rm = TRUE) # total known negatives

  # get predictor values for positive and negative outcomes to figure out whether it is a true positive or a false positive
  pos_pred <- pred_values[pos_values == 1 & !is.na(pos_values)] # predictors for known positives
  neg_pred <- pred_values[pos_values == 0 & !is.na(pos_values)] # predictors for known negatives

  # adds true positive rate and false positive rate to the data-frame `data`
  data %>%
    dplyr::mutate(
      tpr = calc_tpr(pred_values, pos_pred, npos),
      fpr = calc_fpr(pred_values, neg_pred, nneg),
      tnr = calc_tnr(pred_values, neg_pred, nneg),
      fnr = calc_fnr(pred_values, pos_pred, npos),
      ppv = calc_ppv(pred_values, pos_pred, neg_pred)
    )
}
