tidyroc
=======

This is a repository for an R package `tidyroc`. `tidyroc` is currently
under development, and I plan to release it this summer (summer 2019).

Usage
-----

`tidyroc` has 3 primary functions:

-   `make_roc()` calculates true positive and false positive rates to
    plot a receiver operating characteristic (ROC) curve
-   `make_pr()` calculates presicion and recall (true positive rate) to
    plot a precision-recall curve  
-   `calc_auc()` calculates the area under an ROC (AUC)

`tidyroc` is designed to be integrated with the Tidyverse. It is
intended to work with `broom`, `dplyr`, and `ggplot2`. Here is a simple
use case.

    glm(outcome ~ clump_thickness + uniform_cell_shape, # fit a model with 2 predictors
      family = binomial,
      data = biopsy
    ) %>%
      augment() %>% # use broom to add glm output to the original data frame
      make_roc(predictor = .fitted, known_class = outcome) %>% # get values to plot an ROC curve
      ggplot(aes(x = fpr, y = tpr)) + # plot false positive rate against true positive rate
      geom_line()

Examples
--------

Before I make any plots, I fit two logitistic regression models. I will
use the `iris` dataset for this purpose. First, I reduce `iris` to only
contain data for two species of irises. This ensures that the outcome is
binary.

I load the libraries needed to run the examples below.

    # load tidyverse packages
    library(tidyverse)
    library(broom)

    # load cowplot to change plot theme
    library(cowplot)

    # load MASS to access `biopsy` dataset
    library(MASS)

    # load tidyroc
    devtools::load_all(".")

I reduce the `iris` dataset to contain information only about virginica
and versicolor. I fit two logistic regression models that attempt to
predict the two species of irises. The first model tries uses petal
width, petal length, and sepal width as predictors. The second model
uses only sepal width as a predictor.

    # change column names from `V1`, `V2`, etc. to informative variable names
    colnames(biopsy) <- 
        c("ID",
          "clump_thickness",
          "uniform_cell_size",
          "uniform_cell_shape",
          "marg_adhesion",
          "epithelial_cell_size",
          "bare_nuclei",
          "bland_chromatin",
          "normal_nucleoli",
          "mitoses",
          "outcome")

    # fit a logistic regression model to predict tumor types
    glm_out1 <- glm(
      formula = outcome ~ clump_thickness + uniform_cell_shape + marg_adhesion + bare_nuclei + bland_chromatin + normal_nucleoli,
      family = binomial,
      data = biopsy
    ) %>%
      augment() %>%
      mutate(model = "m1") # name the model

    # fit a different logistic regression model to predict tumor types
    glm_out2 <- glm(outcome ~ clump_thickness,
      family = binomial,
      data = biopsy
    ) %>%
      augment() %>%
      mutate(model = "m2") # name the model

    # combine the two datasets to make an ROC curve for each model
    glm_out <- bind_rows(glm_out1, glm_out2)

    # plot the distribution of fitted values to see both models' outcomes
    glm_out %>%
      ggplot(aes(x = .fitted, fill = outcome)) +
      geom_density(alpha = 0.6, color = NA) +
      scale_fill_manual(values = c("#F08A5D", "#B83B5E")) +
      facet_wrap(~ model)

![](figures/unnamed-chunk-3-1.png) Now that I have fitted values, I can
make a plot with two ROC curves and a plot with two precision-recall
curves. I can also calculate the area under each of the ROC curves.

#### Plot ROC curves for 2 different models

    # plot ROC curves
    glm_out %>%
      group_by(model) %>% # group to get individual ROC curve for each model
      make_roc(predictor = .fitted, known_class = outcome) %>% # get values to plot an ROC curve
      ggplot(aes(x = fpr, y = tpr, color = model)) +
      geom_line(size = 1.1) +
      geom_abline(slope = 1, intercept = 0, size = 0.4) +
      scale_color_manual(values = c("#48466D", "#3D84A8")) +
      theme_cowplot()

![](figures/unnamed-chunk-4-1.png)

#### Plot precision-recall curves for 2 different models

    # plot precision-recall curves using the data-frame we generated in the previous example
    glm_out %>%
      group_by(model) %>% # group to get individual precision-recall curve for each model
      make_pr(predictor = .fitted, known_class = outcome) %>% # get values to plot a precision-recall curve
      ggplot(aes(x = recall, y = precision, color = model)) +
      geom_line(size = 1.1) +
      coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
      scale_color_manual(values = c("#48466D", "#3D84A8")) +
      theme_cowplot()

![](figures/unnamed-chunk-5-1.png)

#### Calculate AUC of two ROC curves

    glm_out %>%
      group_by(model) %>% # group to get individual precision-recall curve for each model
      make_roc(predictor = .fitted, known_class = outcome) %>%
      summarise(auc = calc_auc(x = fpr, y = tpr))

    ## # A tibble: 2 x 2
    ##   model   auc
    ##   <chr> <dbl>
    ## 1 m1    0.996
    ## 2 m2    0.910
