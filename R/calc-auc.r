#' Area under a curve (AUC)
#'
#' This function calculates the area under an ROC curve or the area under a precision-recall curve.
#' @param x the variable plotted on the x-axis of the curve plot, e.g. for a plot with an ROC curve, x-axis is the false positive rate
#' @param y the varialbe plotted on the y-axis of the curve plot, e.g. for a plot with an ROC curve, y-axis is the true positive rate
#'
#' @keywords
#' @export
#' @examples
#' # load tidyverse packages
#' library(tidyverse)
#' library(broom)
#'
#' # load cowplot to change plot theme
#' library(cowplot)
#'
#' # load tidyroc
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
#'  augment() %>% # use broom to add glm output to the #' original data frame
#'  make_roc(predictor = .fitted, known_class = outcome) %>% # get values to plot an ROC curve
#'  summarise(auc = calc_auc(x = fpr, y = tpr)) # calculate the area under an ROC curve
#'

calc_auc <- function(x, y) {
  # get the width between each point to get the width of the rectangle
  width = abs(lead(x) - x)

  # get the height of the rectanlge
  height1 = y

  # get the height of the triangle
  height2 = abs(lead(y) - y)

  # calculate the area of the rectanlge and the triangle
  area = width*(height1 + height2 * 0.5)

  # sum all the rectangles and triangles
  sum(area, na.rm = TRUE)
}
