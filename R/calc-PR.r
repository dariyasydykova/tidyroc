#' Receiver operating characteristic curve (ROC)
#'
#' this function calculates true positive rate and false positive rate to make an ROC curve
#' @param probabilities
#'
#' @keywords
#' @export
#' @examples
#' calc_ROC(glm.out$fitted.values, lr_data$Species, model.name = "model1", data = lr_data)

# this function calculates precision and recall
calc_PR <- function(probabilities, known_truth, model.name = NULL) {
  outcome <- as.numeric(factor(known_truth)) - 1
  pos <- sum(outcome) # total known positives
  pos_probs <- outcome * probabilities # probabilities for known positives
  neg_probs <- (1 - outcome) * probabilities # probabilities for known negatives
  recall <- sapply(
    probabilities,
    function(x) sum(pos_probs >= x) / pos
  )
  precision <- sapply(
    probabilities,
    function(x) sum(pos_probs >= x) / (sum(pos_probs >= x) + sum(neg_probs >= x))
  )
  if (is.null(model.name)) {
    result <- data.frame(precision, recall)
  } else {
    result <- data.frame(precision, recall, model.name)
  }

  result %>% arrange(recall, desc(precision))
}
