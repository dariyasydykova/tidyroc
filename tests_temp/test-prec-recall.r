library(rlang)
library(dplyr)
library(ggplot2)
library(broom)

# make a reduced iris data set that only contains virginica and versicolor species
iris.small <- filter(iris, Species %in% c("virginica", "versicolor"))

# fit a logistic regression model to the data
d <- glm(Species ~ Petal.Width + Petal.Length + Sepal.Width,
         data = iris.small,
         family = binomial) %>%
  augment() %>%
  prec_recall(predictor = .fitted, positive = Species)

d %>%
  ggplot(aes(recall, precision)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))