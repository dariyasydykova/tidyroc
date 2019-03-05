library(rlang)
library(dplyr)
library(ggplot2)
library(broom)

# make a reduced iris data set that only contains virginica and versicolor species
iris.small <- filter(iris, Species %in% c("virginica", "versicolor"))

# fit a logistic regression model to the data
d1 <- glm(Species ~ Petal.Width + Petal.Length + Sepal.Width,
         data = iris.small,
         family = binomial) %>%
  augment() %>%
  calc_roc(predictor = .fitted, positive = Species) %>%
  mutate(model = "model1")

d2 <- glm(Species ~ Petal.Width,
         data = iris.small,
         family = binomial) %>%
  augment() %>%
  calc_roc(predictor = .fitted, positive = Species) %>%
  mutate(model = "model2")

d <- bind_rows(d1, d2)

d %>%
  ggplot(aes(false_pos, true_pos, color = model)) +
  geom_line() +
  geom_point()
