# training

library(tidyverse)
library(xgboost)
library(recipes)
library(glmnet)
library(ranger)
library(kernlab)

features <- read.csv("data/train/features.csv", stringsAsFactors = FALSE) %>% 
  group_by(ID) %>% 
  mutate(rating_sum = sum((massey_r_diff > 0) + 
                            (od_diff > 0)+
                            (elo_all_diff > 0)+
                            (elo_r_diff > 0)+
                            (RPI_diff > 0)+
                            (colley_r_diff > 0)+
                            (markov_win_diff > 0)+
                            (nmf > 0)+
                            (keener_scorediff_diff > 0))) %>% 
  ungroup()
sample <- read.csv("data/WSampleSubmissionStage1.csv", 
                   stringsAsFactors = FALSE) %>% 
  as_tibble()
sample2 <- read.csv("data/WSampleSubmissionStage2.csv", 
                    stringsAsFactors = FALSE) %>% 
  as_tibble()
seeds <- read_csv("data/processed/seed.csv")
source("src/function/validate.R")

tr <- features %>% 
  filter(as.integer(str_sub(ID, 1, 4)) < 2019,
         # as.integer(str_sub(ID, 1, 4)) > 2002
  ) 
tr <- bind_rows(
  tr,
  tr %>% 
    mutate(target = 1-target) %>% 
    mutate_at(vars(everything(), -target, -ID), function(x) -x)
)
te <- features %>% 
  semi_join(sample2, by = "ID") %>% 
  arrange(ID)

tr_df <- tr %>% 
  select(-ID, -target)

tr_lab <- tr$target

te_df <- te %>% 
  select(-ID, -target) 

submit <- tibble(ID = te$ID)

# glm----
set.seed(1)
rec_glm = recipe( ~ ., data = tr_df) %>% 
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  # step_pca(all_predictors(), -massey_r_diff, num_comp = 5) %>%
  prep()
tr_glm <- bake(rec_glm, tr_df) 
te_glm <- bake(rec_glm, te_df)

fit_glm <- glm(tr_lab ~ . -1-elo_score_diff-rating_mean_diff-log_seed_diff-coef_nmf_diff-massey_3yr_diff-
                 basis_nmf_diff-nmf-elo_2r_diff-colley_r_diff-keener_scorediff_diff-keener_scorerate_diff,
               data = tr_glm,
               family = binomial) 
summary(fit_glm)
submit$Pred_glm = predict(fit_glm, newdata = te_glm, type = "response")


# glmnet----
set.seed(1)
rec_glmnet = recipe( ~ .-1, data = tr_df) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  prep()

tr_glmnet <- bake(rec_glmnet, tr_df) %>% 
  select(-elo_score_diff,-rating_mean_diff,-log_seed_diff,-coef_nmf_diff,-massey_3yr_diff,-
           basis_nmf_diff,-nmf,-elo_2r_diff,-colley_r_diff,-keener_scorediff_diff,-keener_scorerate_diff)
te_glmnet <- bake(rec_glmnet, te_df)%>% 
  select(-elo_score_diff,-rating_mean_diff,-log_seed_diff,-coef_nmf_diff,-massey_3yr_diff,-
           basis_nmf_diff,-nmf,-elo_2r_diff,-colley_r_diff,-keener_scorediff_diff,-keener_scorerate_diff)


set.seed(1)
cvfit <- cv.glmnet(x = as.matrix(tr_glmnet), y = tr_lab, family = "binomial", standardize = TRUE,
                   alpha = 1,
                   nfolds = 100)
fit_lasso <- glmnet(x = as.matrix(tr_glmnet), y = tr_lab, family = "binomial", standardize = TRUE,
                    alpha = 1,
                    lambda = cvfit$lambda.min)
cvfit$cvm %>% min
set.seed(1)
fit_ridge <- glmnet(x = as.matrix(tr_glmnet), y = tr_lab, family = "binomial", standardize = TRUE,
                    alpha = 0,
                    lambda = 0.01)


submit$Pred_lasso = predict(fit_lasso, as.matrix(te_glmnet), type = "response")
submit$Pred_ridge = predict(fit_ridge, as.matrix(te_glmnet), type = "response")
validate(submit)
validate_y(submit)

# ranger----
set.seed(1)
rec_ranger = recipe( ~ ., data = tr_df) %>% 
  # step_corr(all_numeric(), - massey_r_diff, threshold = .9) %>%
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  prep()

tr_ranger <- bake(rec_ranger, tr_df) 
te_ranger <- bake(rec_ranger, te_df) 

set.seed(1)
fit_rf <- ranger(tr_lab ~ .,
                 data = tr_ranger,
                 num.trees = 2000,
                 mtry = 1,
                 # probability = TRUE,
                 importance = "impurity",
                 classification = T,
                 probability = T)

set.seed(1)
fit_ext <- ranger(tr_lab ~ .,
                  data = tr_ranger,
                  mtry = 1,
                  num.trees = 2000,
                  importance = "impurity",
                  splitrule = "extratrees",
                  classification = T,
                  probability = T)
submit$Pred_rf = predict(fit_rf, te_ranger)$predictions[,1]
submit$Pred_ext = predict(fit_ext, te_ranger)$predictions[,1]

validate(submit)
validate_y(submit)

# xgblinear----
set.seed(1)
rec_gblinear = recipe( ~ ., data = tr_df) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  prep()

tr_gblinear <- bake(rec_gblinear, tr_df) %>% 
  select(-elo_score_diff,-rating_mean_diff,-log_seed_diff,-coef_nmf_diff,-massey_3yr_diff,-
           basis_nmf_diff,-nmf,-elo_2r_diff,-colley_r_diff,-keener_scorediff_diff,-keener_scorerate_diff)
te_gblinear <- bake(rec_gblinear, te_df) %>% 
  select(-elo_score_diff,-rating_mean_diff,-log_seed_diff,-coef_nmf_diff,-massey_3yr_diff,-
           basis_nmf_diff,-nmf,-elo_2r_diff,-colley_r_diff,-keener_scorediff_diff,-keener_scorerate_diff) 
dtrain_gblinear <- xgb.DMatrix(as.matrix(tr_gblinear),
                               label =  tr_lab)
dtest_gblinear <- xgb.DMatrix(as.matrix(te_gblinear),
                              label = te$target)
param_gblinear <- list(eta = .1,
                       silent = 1, 
                       lambda = 0,
                       lambda_bias = 0,
                       alpha = 0,
                       booster = "gblinear",
                       objective = "binary:logistic",
                       eval_metric = "logloss",
                       nthread = 1)

set.seed(1)
cv_gblinear <- xgb.cv(params = param_gblinear, dtrain_gblinear, nrounds = 300, nfold = 30,
                      early_stopping_rounds = 20)

set.seed(1)
fit_gblinear <- xgb.train(params = param_gblinear, dtrain_gblinear, nrounds = cv_gblinear$best_iteration)

submit$Pred_gblinear = predict(fit_gblinear, dtest_gblinear)

validate(submit)
validate_y(submit)

# gbtree----
set.seed(1)
rec_gbtree = recipe( ~ ., data = tr_df) %>% 
  # step_corr(all_numeric(), - massey_r_diff, threshold = .99) %>%
  # step_pca(all_numeric(), -rating_sum, num_comp = 7) %>%
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  prep()

tr_gbtree <- bake(rec_gbtree, tr_df)
te_gbtree <- bake(rec_gbtree, te_df)
dtrain_gbtree <- xgb.DMatrix(as.matrix(tr_gbtree),
                             label =  tr_lab)
dtest_gbtree <- xgb.DMatrix(as.matrix(te_gbtree),
                            label = te$target)
param_gbtree <- list(max_depth = 2,
                     min_child_weight = 1,
                     colsample_bytree = 0.7,
                     subsample = 1,
                     eta = .1,
                     silent = 1, 
                     lambda = 0,
                     gamma = 0,
                     booster = "gbtree",
                     objective = "binary:logistic",
                     eval_metric = "logloss",
                     nthread = 1)

set.seed(1)
cv_gbtree <- xgb.cv(params = param_gbtree, dtrain_gbtree, nrounds = 300, nfold = 30,
                    early_stopping_rounds = 30)

set.seed(1)
fit_gbtree <- xgb.train(params = param_gbtree, dtrain_gbtree, nrounds = 50)

submit$Pred_gbtree = predict(fit_gbtree, dtest_gbtree)

validate(submit)
validate_y(submit)

# dart----
set.seed(1)
rec_dart = recipe( ~ ., data = tr_df) %>% 
  # step_corr(all_numeric(), - massey_r_diff, threshold = .99) %>%
  # step_pca(all_numeric(), -rating_sum, num_comp = 7) %>%
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  prep()

tr_dart <- bake(rec_dart, tr_df)
te_dart <- bake(rec_dart, te_df)
dtrain_dart <- xgb.DMatrix(as.matrix(tr_dart),
                           label =  tr_lab)
dtest_dart <- xgb.DMatrix(as.matrix(te_dart),
                          label = te$target)
param_dart <- list(max_depth = 2,
                   min_child_weight = 1,
                   eta = .1,
                   silent = 10, 
                   booster = "dart",
                   objective = "binary:logistic",
                   eval_metric = "logloss",
                   nthread = 1)

set.seed(1)
cv_dart <- xgb.cv(params = param_dart, dtrain_dart, nrounds = 300, nfold = 30,
                  early_stopping_rounds = 30)

set.seed(1)
fit_dart <- xgb.train(params = param_dart, dtrain_dart, nrounds = cv_dart$best_iteration)

submit$Pred_dart = predict(fit_dart, dtest_dart)

# importance----
left_join(importance(fit_rf) %>% 
            tibble(Feature = names(.),
                   imp_rf = .),
          importance(fit_ext) %>% 
            tibble(Feature = names(.),
                   imp_ext = .),
          by = "Feature") %>% 
  left_join(as_tibble(xgb.importance(colnames(dtrain_gblinear), fit_gblinear)), by = "Feature") %>% 
  ggplot(aes(imp_rf, imp_ext))+
  geom_text(aes(label = Feature))


# submit----
submit <- sample2

submit$Pred_glm = predict(fit_glm, newdata = te_glm, type = "response")
submit$Pred_lasso = predict(fit_lasso, as.matrix(te_glmnet), type = "response")
submit$Pred_ridge = predict(fit_ridge, as.matrix(te_glmnet), type = "response")
submit$Pred_rf = predict(fit_rf, te_ranger)$predictions[,1]
submit$Pred_ext = predict(fit_ext, te_ranger)$predictions[,1]
submit$Pred_gblinear = predict(fit_gblinear, dtest_gblinear)
submit$Pred_gbtree = predict(fit_gbtree, dtest_gbtree)
submit$Pred_dart = predict(fit_dart, dtest_dart)


sub <- submit %>% 
  gather(model, pred, -ID) %>% 
  mutate(pred = if_else(pred < 0.025, 0.025, pred),
         pred = if_else(pred > 0.975, 0.975, pred)) %>% 
  spread(model, pred) %>% 
  mutate(Pred = (Pred_gblinear + Pred_gbtree + Pred_dart) / 4 + 
           (Pred_lasso + Pred_ridge + Pred_glm) / 12) %>% 
  select(ID, Pred)

sub2 <- sub %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(seeds, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(seeds, by = c("Season", "team2" = "TeamID")) %>% 
  transmute(ID, Pred, 
            Seed1 = seed.x, 
            Seed2 = seed.y)

sub2$Pred[sub2$Seed1 == 16 & sub2$Seed2 == 1] = 0.0001
sub2$Pred[sub2$Seed1 == 15 & sub2$Seed2 == 2] = 0.0001
sub2$Pred[sub2$Seed1 == 14 & sub2$Seed2 == 3] = 0.0001
sub2$Pred[sub2$Seed1 == 13 & sub2$Seed2 == 4] = 0.0001
sub2$Pred[sub2$Seed1 == 1 & sub2$Seed2 == 16] = 0.9999
sub2$Pred[sub2$Seed1 == 2 & sub2$Seed2 == 15] = 0.9999
sub2$Pred[sub2$Seed1 == 3 & sub2$Seed2 == 14] = 0.9999
sub2$Pred[sub2$Seed1 == 4 & sub2$Seed2 == 13] = 0.9999
sub2 <- sub2 %>% 
  select(ID, Pred)

sub2 %>%
  mutate(Pred = if_else(ID == "2019_3243_3276", .9999, Pred)) %>%
  write_csv("data/submit/first.csv")
sub2 %>%
  mutate(Pred = if_else(ID == "2019_3243_3276", .0001, Pred)) %>%
  write_csv("data/submit/second.csv")
