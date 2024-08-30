# R Tidymodels: A Tidyverse Like Ecosystem For Efficient Machine Learning In R
# https://www.appsilon.com/post/r-tidymodels

# load
install.packages('vip')
library(vip)
library(readr)
library(dplyr)
library(tidymodels)
library(rpart.plot)

# get
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
wine_data <- read_delim(url, col_names = TRUE, delim = ";")
head(wine_data)

# count
wine_data %>%
  group_by(quality) %>%
  count()

# group
wine_data <- wine_data %>%
  mutate(quality_fct = case_when(
    quality %in% c(3, 4) ~ "bad",
    quality %in% c(5, 6) ~ "good",
    quality %in% c(7, 8) ~ "great"
  )) %>%
  select(-quality)
wine_data$quality_fct <- factor(wine_data$quality_fct, levels = c("bad", "good", "great"))

wine_data %>%
  group_by(quality_fct) %>%
  count()

# split
set.seed(42)
split <- initial_split(wine_data, prop = 0.8, strata = quality_fct)
train_data <- training(split)
test_data <- testing(split)

dim(train_data)
dim(test_data)

# normalize
wine_recipe <-
  recipe(quality_fct ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors())
wine_recipe

# decision tree
wine_model <-
  decision_tree(mode = "classification") %>%
  set_engine("rpart")

# workflow
wine_workflow <-
  workflow() %>%
  add_model(wine_model) %>%
  add_recipe(wine_recipe)
wine_workflow

# model fit
wine_fit <-
  wine_workflow %>%
  fit(data = train_data)
wine_fit

# model viz
wine_fit %>%
  extract_fit_engine() %>%
  rpart.plot()

# feature importance
wine_fit %>%
  extract_fit_parsnip() %>%
  vip()

# prediction evaluation
wine_preds <-
  augment(wine_fit, test_data) %>%
  select(quality_fct, .pred_class, .pred_bad, .pred_good, .pred_great) %>%
  rename(
    actual = quality_fct,
    predicted = .pred_class
  )
wine_preds

wine_preds %>% accuracy(truth = actual, predicted)
wine_preds %>% precision(truth = actual, predicted)
wine_preds %>% recall(truth = actual, predicted)

wine_preds %>%
  conf_mat(truth = actual, predicted)

wine_preds %>%
  summary()

wine_preds %>%
  roc_curve(truth = actual, .pred_bad:.pred_great) %>%
  autoplot()

wine_preds %>%
  pr_curve(truth = actual, .pred_bad:.pred_great) %>%
  autoplot()



