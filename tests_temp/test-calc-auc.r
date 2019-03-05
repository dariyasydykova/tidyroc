# make a reduced iris data set that only contains virginica and versicolor species
iris.small <- filter(iris, Species %in% c("virginica", "versicolor"))

# fit a logistic regression model to the data
d1 <- glm(Species ~ Petal.Width + Petal.Length + Sepal.Width,
          data = iris.small,
          family = binomial) %>%
  augment() %>%
  calc_roc(predictor = .fitted, positive = Species) ->
  roc

roc %>% mutate(width = false_pos - lag(false_pos),
               height1 = true_pos,
               height2 = true_pos - lag(true_pos),
               area = width*(height1 + height2 * 0.5)) -> roc1

roc1 %>% summarise(auc = sum(area, na.rm = TRUE))
