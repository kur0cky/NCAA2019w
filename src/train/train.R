# training

library(tidyverse)
library(xgboost)
library(recipes)
library(glmnet)
library(ranger)

features <- read.csv("data/train/features.csv", stringsAsFactors = FALSE)
sample <- read.csv("data/WSampleSubmissionStage1.csv", 
                   stringsAsFactors = FALSE) %>% 
  as_tibble()
seeds <- read_csv("data/processed/seed.csv")
source("src/function/validate.R")

tr <- features %>% 
  filter(as.integer(str_sub(ID, 1, 4)) < 2014) %>% 
  drop_na()
te <- features %>% 
  semi_join(sample, by = "ID") %>% 
  arrange(ID)

tr_df <- tr %>% 
  select(-ID, -target)

tr_lab <- tr$target

te_df <- te %>% 
  select(-ID, -target) 

# recipes----
set.seed(1)
rec = recipe( ~ ., data = tr_df) %>% 
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  # step_pca(all_numeric(), threshold = .9) %>%
  step_ica(all_numeric(), num_comp = 10) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  prep()
set.seed(1)
rec2 = recipe( ~ ., data = tr_df) %>% 
  step_corr(all_numeric(), threshold = .98) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  prep()

tr_rec <- bake(rec, tr_df) 
te_rec <- bake(rec, te_df)
dtrain <- xgb.DMatrix(as.matrix(bake(rec2, tr_df)),
                      label =  tr_lab)
dtest <- xgb.DMatrix(as.matrix(bake(rec2, te_df)),
                     label = te$target)
# gbtree----
param <- list(max_depth = 1,
              min_child_weight = 1,
              colsample_bytree = 0.5,
              subsample = 1,
              eta = .02,
              silent = 1, 
              lambda = 0,
              gamma = 0,
              booster = "gbtree",
              objective = "binary:logistic",
              eval_metric = "logloss",
              nthread = 1)

set.seed(1)
cv <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 20,
             early_stopping_rounds = 10,
             prediction = TRUE)

set.seed(1)
bst <- xgb.train(params = param, dtrain, nrounds = cv$best_iteration)


# gblinear----

paraml <- list(eta = .01,
              silent = 1, 
              lambda = 0,
              lambda_bias = 0,
              alpha = 0,
              booster = "gblinear",
              objective = "binary:logistic",
              eval_metric = "logloss",
              nthread = 1)

set.seed(1)
cv_l <- xgb.cv(params = paraml, dtrain, nrounds = 10000, nfold = 20,
             early_stopping_rounds = 10,
             prediction = TRUE)

set.seed(1)
linear <- xgb.train(params = paraml, dtrain, nrounds = cv_l$best_iteration)

# dart----

paramd <- list(max_depth = 1,
               min_child_weight = 3,
               eta = .02,
               silent = 10, 
               booster = "dart",
               objective = "binary:logistic",
               eval_metric = "logloss",
               nthread = 1)

set.seed(1)
cv_d <- xgb.cv(params = paramd, dtrain, nrounds = 10000, nfold = 20,
               early_stopping_rounds = 10,
               prediction = TRUE)

set.seed(1)
dart <- xgb.train(params = paramd, dtrain, nrounds = cv_d$best_iteration)

submit <- tibble(ID = te$ID,
                 Pred_l = predict(linear, dtest),
                 Pred_b = predict(bst, dtest),
                 Pred_d = predict(dart, dtest),
                 Pred = (Pred_l + Pred_b + Pred_d)/ 3)

validate(submit)
validate_y(submit)

xgb.importance(colnames(dtrain), bst)
xgb.importance(colnames(dtrain), dart)
xgb.importance(colnames(dtrain), linear)

# linear model----

fit_glm <- glm(tr_lab ~ . -1,
               data = tr_rec,
               family = binomial) 
summary(fit_glm)
library(glmnet)

set.seed(1)
cvfit <- cv.glmnet(x = as.matrix(tr_df), y = tr_lab, family = "binomial", standardize = TRUE,
                   alpha = 1,
                   nfolds = 20)
fit_lasso <- glmnet(x = as.matrix(tr_df), y = tr_lab, family = "binomial", standardize = TRUE,
                    alpha = 1,
                    lambda = cvfit$lambda.min)

set.seed(1)
cvfit <- cv.glmnet(x = as.matrix(tr_df), y = tr_lab, family = "binomial", standardize = TRUE,
                   alpha = 0,
                   nfolds = 20)
fit_ridge <- glmnet(x = as.matrix(tr_df), y = tr_lab, family = "binomial", standardize = TRUE,
                    alpha = 0,
                    lambda = cvfit$lambda.min)

library(ranger)

set.seed(1)
fit_rf <- ranger(tr_lab ~ .,
                 data = tr_df,
                 num.trees = 1000,
                 mtry = 3,
                 # probability = TRUE,
                 importance = "impurity")

set.seed(1)
fit_ext <- ranger(tr_lab ~ .,
                  data = tr_df,
                  mtry = 3,
                  num.trees = 1000,
                  importance = "permutation",
                  splitrule = "extratrees")


submit <- submit %>% 
  mutate(pred_glm = predict(fit_glm, newdata = te_rec, type = "response"),
         pred_lasso = predict(fit_lasso, as.matrix(te_df), type = "response"),
         pred_ridge = predict(fit_ridge, as.matrix(te_df), type = "response"),
         pred_rf = predict(fit_rf, te_df)$predictions,
         pred_ext = predict(fit_ext, te_df)$predictions) %>% 
  mutate(pred_rf = if_else(pred_rf < .025, .025, pred_rf),
         pred_rf = if_else(pred_rf > .975, .975, pred_rf),
         pred_ext = if_else(pred_ext < .025, .025, pred_ext),
         pred_ext = if_else(pred_ext > .975, .975, pred_ext),
         Pred_l = predict(linear, dtest),
         Pred_b = predict(bst, dtest),
         Pred_d = predict(dart, dtest),) %>% 
  mutate(Pred = (pred_lasso + pred_ridge + pred_ext + pred_rf*2 +
                   Pred_l + Pred_b*2 + Pred_d) / 9)

  

validate(submit)
validate_y(submit)
sub <- submit %>% 
  select(ID, Pred) %>% 
  arrange(ID)
sub %>%
  write_csv("data/submit/first.csv")

# boruta----

# library(Boruta)
# set.seed(111)
# res_boruta <- Boruta(tr_lab ~ . -1,
#                      data = tr_df,
#                      doTrace = 2,
#                      maxRuns = 500)
# print(res_boruta)
# plot(res_boruta)
# attStats(res_boruta) %>%
#   rownames_to_column("feature") %>%
#   arrange(desc(meanImp))
# plotImpHistory(res_boruta)


sub <- submit %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(seeds, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(seeds, by = c("Season", "team2" = "TeamID")) %>% 
  transmute(ID, Pred, 
            Seed1 = seed.x, 
            Seed2 = seed.y)

sub$Pred[sub$Seed1 == 16 & sub$Seed2 == 1] = 0.0001
sub$Pred[sub$Seed1 == 15 & sub$Seed2 == 2] = 0.0001
sub$Pred[sub$Seed1 == 14 & sub$Seed2 == 3] = 0.0001
sub$Pred[sub$Seed1 == 13 & sub$Seed2 == 4] = 0.0001
sub$Pred[sub$Seed1 == 1 & sub$Seed2 == 16] = 0.9999
sub$Pred[sub$Seed1 == 2 & sub$Seed2 == 15] = 0.9999
sub$Pred[sub$Seed1 == 3 & sub$Seed2 == 14] = 0.9999
sub$Pred[sub$Seed1 == 4 & sub$Seed2 == 13] = 0.9999
validate_y(submit)
validate_y(sub)

sub %>% 
  select(ID, Pred) %>% 
  write_csv("data/submit/first_override.csv")
