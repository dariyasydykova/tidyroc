tidyroc
=======

This is a repository for an R package `tidyroc`. `tidyroc` is currently
under development, and I plan to release it this summer (summer 2019).

Installation
------------

You can install `tidyroc` with the following command

    remotes::install_github("dariyasydykova/tidyroc")

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

    # load tidyverse packages
    library(tidyverse)
    library(broom)

    # load cowplot to change plot theme
    library(cowplot)

    # load tidyroc
    library(tidyroc)

    # get `biopsy` dataset from `MASS`
    data(biopsy, package = "MASS")

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

    # fit a logistic regression model to predict tumour type
    glm(outcome ~ clump_thickness + uniform_cell_shape, 
      family = binomial,
      data = biopsy
    ) %>%
      augment() %>% # use broom to add glm output to the original data frame
      make_roc(predictor = .fitted, known_class = outcome) %>% # get values to plot an ROC curve
      ggplot(aes(x = fpr, y = tpr)) + # plot false positive rate against true positive rate
      geom_line()

![](figures/unnamed-chunk-2-1.png)

Example with two logistic regression models
-------------------------------------------

    # load tidyverse packages
    library(tidyverse)
    library(broom)

    # load cowplot to change plot theme
    library(cowplot)

    # load MASS to access `biopsy` dataset
    library(MASS)

    # load tidyroc
    library(tidyroc)

I use the `biopsy` dataset from `MASS` package. This dataset contains
information about biopsies of breast cancer tumours for 699 patients. I
fit two logistic regression models that attempt to predict tumour type,
benign or malignant.

    # get `biopsy` dataset from `MASS`
    data(biopsy, package = "MASS")

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

![](figures/unnamed-chunk-4-1.png)

Now that I have fitted values, I can make a plot with two ROC curves and
a plot with two precision-recall curves. I can also calculate the area
under each of the ROC curves.

### ROC curve

    # plot ROC curves
    glm_out %>%
      group_by(model) %>% # group to get individual ROC curve for each model
      make_roc(predictor = .fitted, known_class = outcome) %>% # get values to plot an ROC curve
      ggplot(aes(x = fpr, y = tpr, color = model)) +
      geom_line(size = 1.1) +
      geom_abline(slope = 1, intercept = 0, size = 0.4) +
      scale_color_manual(values = c("#48466D", "#3D84A8")) +
      theme_cowplot()

![](figures/unnamed-chunk-5-1.png)

### Precision-recall curve

    # plot precision-recall curves using the data-frame we generated in the previous example
    glm_out %>%
      group_by(model) %>% # group to get individual precision-recall curve for each model
      make_pr(predictor = .fitted, known_class = outcome) %>% # get values to plot a precision-recall curve
      ggplot(aes(x = recall, y = precision, color = model)) +
      geom_line(size = 1.1) +
      coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
      scale_color_manual(values = c("#48466D", "#3D84A8")) +
      theme_cowplot()

![](figures/unnamed-chunk-6-1.png)

### AUC values

    glm_out %>%
      group_by(model) %>% # group to get individual precision-recall curve for each model
      make_roc(predictor = .fitted, known_class = outcome) %>%
      summarise(auc = calc_auc(x = fpr, y = tpr))

    ## # A tibble: 2 x 2
    ##   model   auc
    ##   <chr> <dbl>
    ## 1 m1    0.996
    ## 2 m2    0.910

    glm_out %>%
      group_by(model) %>% # group to get individual precision-recall curve for each model
      make_pr(predictor = .fitted, known_class = outcome) %>%
      summarise(auc = calc_auc(x = recall, y = precision))

    ## # A tibble: 2 x 2
    ##   model   auc
    ##   <chr> <dbl>
    ## 1 m1    0.991
    ## 2 m2    0.951
