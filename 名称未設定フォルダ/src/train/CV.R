# training 
# practice of cross validation using rsample

library(tidyverse)
library(tidymodels)

# training

library(tidyverse)
library(xgboost)
library(recipes)
library(glmnet)
library(ranger)


features <- read.csv("data/train/features.csv", stringsAsFactors = FALSE) 
# sample <- read.csv("data/WSampleSubmissionStage1.csv", 
#                    stringsAsFactors = FALSE) %>% 
#   as_tibble()
seeds <- read_csv("data/processed/seed.csv")
source("src/function/validate.R")

tr <- features %>% 
  filter(as.integer(str_sub(ID, 1, 4)) < 2019,
         # as.integer(str_sub(ID, 1, 4)) >= 2010
         ) %>% 
  drop_na()
tr <- bind_rows(
  tr,
  tr %>% 
    mutate(target = 1-target) %>% 
    mutate_at(vars(everything(), -target, -ID), function(x) -x)
)

tr_df <- tr %>% 
  select(-ID, -target)

tr_lab <- tr$target

# glm
for(i in 5){
  set.seed(1)
  rec <- recipe(target ~ . - ID, data = tr) %>% 
    step_center(all_numeric(), -all_outcomes()) %>% 
    step_scale(all_numeric(), -all_outcomes()) #%>% 
    # step_pca(all_numeric(), -all_outcomes(), -massey_r_diff, 
    #          -od_diff, -markov_win_diff, -seed_diff, -massey_r_diff, -elo_r_diff, -elo_3r_diff,
    #          -elo_all_diff, -elo_score_all_diff, -Pyth_ratio_diff, -RPI_diff,
    #          num_comp = 1)
  
  df_cv <- tr %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 9, group = "Season")
  
  tmp <- df_cv %>% 
    mutate(rec = map(splits, prepper, rec))
  
  pred_func <- function(split_obj, rec_obj, model_obj, ...) {
    df_pred <- bake(rec_obj, assessment(split_obj)) 
    df_pred <- df_pred %>% 
      mutate(Pred = predict(model_obj, df_pred, type = "response")) %>% 
      select(target, Pred)
    return(df_pred)
  }
  
  
  tmp2 <- tmp %>% 
    mutate(fit_glm = map(rec, ~ glm(target ~ . -elo_score_diff-rating_mean_diff-log_seed_diff-coef_nmf_diff-massey_3yr_diff-
                                    basis_nmf_diff-nmf-elo_2r_diff-colley_r_diff-keener_scorediff_diff-keener_scorerate_diff,
                                    data = dplyr::select(juice(.x), -ID), 
                                    family = binomial)),
           predicted = pmap(list(splits, rec, fit_glm),
                            pred_func),
           score = map_dbl(predicted, ~ .x %>% 
                             summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
                             unlist()))
  tmp2 %>% 
    select(score) %>% 
    summarise(max = max(score),
              mean = mean(score),
              median = median(score),
              min = min(score),
              sd = sd(score)) %>% 
    print()
  print(i)

}

# randomforest
for(i in 1){
  set.seed(1)
  
  df_cv <- tr %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 21, group = "Season")
  
  fit_func <- function(split){
    df <- analysis(split)
    fit <- ranger(target ~ .-ID-Season,
                  data = df,
                  mtry = i,
                  num.trees = 200,
                  classification = T,
                  probability = T,
                  importance = "impurity") 
    return(fit)
  }
  
  pred_func <- function(fit, split){
    df <- assessment(split)
    df %>% 
      select(target) %>% 
      mutate(Pred = predict(fit, df)$predictions[,1],
             Pred = if_else(Pred < 0.025, 0.025, Pred),
             Pred = if_else(Pred > 0.975, 0.975, Pred))
  }
  
  tmp2 <- df_cv %>% 
    mutate(fit = map(splits, fit_func),
           predicted = pmap(list(fit, splits), pred_func),
           score = map_dbl(predicted, ~ .x %>% 
                             summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
                             unlist()))
  tmp2 %>% 
    select(score) %>% 
    summarise(max = max(score),
              mean = mean(score),
              median = median(score),
              min = min(score),
              sd = sd(score)) %>% 
    print()
  print(i)
  gc()
}

# extratrees
for(i in 1:4){
  df_cv <- tr %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 16, group = "Season")
  
  fit_func <- function(cv){
    df = analysis(cv)
    fit <- ranger(target ~ . - ID - Season,
                  data = df,
                  mtry = 1,
                  num.trees = 1000)
    return(fit)
  }
  pred_func <- function(fit, split){
    df <- assessment(split)
    df %>% 
      select(target) %>% 
      mutate(Pred = predict(fit, df)$predictions[,1],
             Pred = if_else(Pred < 0.025, 0.025, Pred),
             Pred = if_else(Pred > 0.975, 0.975, Pred))
  }
  set.seed(1)
  tmp2 <- df_cv %>% 
    mutate(fit = map(splits, ~ ranger(target ~ .-ID - Season,
                                      data = analysis(.x),
                                      mtry = i,
                                      num.trees = 2000,
                                      classification = T,
                                      probability = T,
                                      splitrule = "extratrees")),
           predicted = map2(fit, splits, ~ pred_func(.x,.y)),
           score = map_dbl(predicted, ~ .x %>% 
                             summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
                             unlist()))
  tmp2 %>% 
    select(score) %>% 
    summarise(max = max(score),
              mean = mean(score),
              median = median(score),
              min = min(score),
              sd = sd(score)) %>% 
    print()
  print(i)
}

# lasso
for(i in 0.004444){
  df_cv <- tr %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 16, group = "Season")
  
  fit_func <- function(cv){
    df = analysis(cv) %>% 
      select(-Season, -ID)
    fit <- glmnet(x = as.matrix(select(df, -target)), 
                  y = select(df, target)[,1], 
                  family = "binomial", 
                  standardize = TRUE,
                  alpha = 1,
                  lambda = i)
    return(fit)
  }
  pred_func <- function(fit, split){
    df <- assessment(split) %>% 
      select(-Season, -ID, -target)
    res <- assessment(split) %>% 
      select(target) %>% 
      mutate(Pred = c(predict(fit, as.matrix(df), type = "response")))
    return(res)
  }
  set.seed(1)
  tmp2 <- df_cv %>% 
    mutate(fit = map(splits, fit_func),
           predicted = map2(fit, splits, ~ pred_func(.x,.y)),
           score = map_dbl(predicted, ~ .x %>% 
                             summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
                             unlist()))
  tmp2 %>% 
    select(score) %>% 
    summarise(max = max(score),
              mean = mean(score),
              median = median(score),
              min = min(score),
              sd = sd(score)) %>% 
    print()
  print(i)
}

# ridge
for(i in c(0:40)*0.001){
  df_cv <- tr %>% 
    select(-elo_score_diff,-rating_mean_diff,-log_seed_diff,-coef_nmf_diff,-massey_3yr_diff,-
             basis_nmf_diff,-nmf,-elo_2r_diff,-colley_r_diff,-keener_scorediff_diff,-keener_scorerate_diff) %>%
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 16, group = "Season")
  
  fit_func <- function(cv){
    df = analysis(cv) %>% 
      select(-Season, -ID)
    fit <- glmnet(x = as.matrix(select(df, -target)), 
                  y = select(df, target)[,1], 
                  family = "binomial", 
                  standardize = TRUE,
                  alpha = 0,
                  lambda = i)
    return(fit)
  }
  pred_func <- function(fit, split){
    df <- assessment(split) %>% 
      select(-Season, -ID, -target)
    res <- assessment(split) %>% 
      select(target) %>% 
      mutate(Pred = c(predict(fit, as.matrix(df), type = "response")))
    return(res)
  }
  set.seed(1)
  tmp2 <- df_cv %>% 
    mutate(fit = map(splits, fit_func),
           predicted = map2(fit, splits, ~ pred_func(.x,.y)),
           score = map_dbl(predicted, ~ .x %>% 
                             summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
                             unlist()))
  tmp2 %>% 
    select(score) %>% 
    summarise(max = max(score),
              mean = mean(score),
              median = median(score),
              min = min(score),
              sd = sd(score)) %>% 
    # .$mean %>% 
    print()
  print(i)
}

# xgbl
for(i in 0.94){
  set.seed(1)
  rec <- tr %>% 
    recipe(target ~ . - ID, data = tr)%>% 
    # step_corr(all_numeric(),-massey_r_diff, threshold = i) %>% 
    step_center(all_numeric()) %>% 
    step_scale(all_numeric())
  
  df_cv <- tr %>% 
    # select(-elo_score_diff,-rating_mean_diff,-log_seed_diff,-coef_nmf_diff,-massey_3yr_diff,-
    #          basis_nmf_diff,-nmf,-elo_2r_diff,-colley_r_diff,-keener_scorediff_diff,-keener_scorerate_diff) %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 21, group = "Season")
  
  tmp <- df_cv %>% 
    mutate(rec = map(splits, prepper, rec))
  
  fit_func <- function(cv, rec){
    dtrain <- xgb.DMatrix(juice(rec) %>% 
                            select(-elo_score_diff,-rating_mean_diff,-log_seed_diff,-coef_nmf_diff,-massey_3yr_diff,-
                                     basis_nmf_diff,-nmf,-elo_2r_diff,-colley_r_diff,-keener_scorediff_diff,-keener_scorerate_diff) %>% 
                            select(-ID, -target) %>% 
                            as.matrix(),
                          label =  analysis(cv) %>% 
                            select(target) %>% 
                            .[,1])
    dtest<- xgb.DMatrix(cv %>% 
                          assessment() %>% 
                          bake(rec,.) %>% 
                          select(-elo_score_diff,-rating_mean_diff,-log_seed_diff,-coef_nmf_diff,-massey_3yr_diff,-
                                   basis_nmf_diff,-nmf,-elo_2r_diff,-colley_r_diff,-keener_scorediff_diff,-keener_scorerate_diff) %>% 
                          select(-ID, -target) %>% 
                          as.matrix(),
                        label =  assessment(cv) %>% 
                          select(target) %>% 
                          .[,1])
    params <- list(eta = .1,
                   silent = 1, 
                   lambda = 0,
                   alpha = 0,
                   booster = "gblinear",
                   objective = "binary:logistic",
                   eval_metric = "logloss",
                   nthread = 1)
    watchlist <- list(train= dtrain, test = dtest)
    set.seed(1)
    cv <- xgb.train(params = params, data =dtrain, nrounds = 300,
                   early_stopping_rounds = 30,verbose = F,
                   watchlist = watchlist,
                   prediction = TRUE)
    return(cv)
  }
  # pred_func <- function(fit, split){
  #   df <- assessment(split) %>% 
  #     select(-Season, -ID, -target)
  #   res <- assessment(split) %>% 
  #     select(target) %>% 
  #     mutate(Pred = c(predict(fit, as.matrix(df), type = "response")))
  #   return(res)
  # }
  set.seed(1)
  tmp2 <- tmp %>% 
    mutate(fit = map2(splits, rec, fit_func))
  tmp2 %>% 
    .$fit %>% 
    lapply(function(x) x$evaluation_log[x$best_iteration,]) %>% 
    do.call("rbind",.) %>% 
    # rownames_to_column("iter") %>% 
    summarise_at(vars("test_logloss"), funs(max, mean,median,min,sd)) %>% 
    # as_tibble() %>% 
    print()
  tmp2 %>%
    .$fit %>%
    lapply(function(x) x$evaluation_log[x$best_iteration,]) %>%
    do.call("rbind",.) %>%
    summary() %>% 
    print()
  print(i)
  # print(set_lambda_alpha[i,])
}

# xgbtree
for(i in 1){
  set.seed(1)
  rec <- tr %>% 
    recipe(target ~ . - ID, data = tr)%>% 
    # step_corr(all_numeric(),-massey_r_diff, threshold = .95) %>%
    step_center(all_numeric()) %>% 
    step_scale(all_numeric())
  
  df_cv <- tr %>% 
    # select(-elo_score_diff,-rating_mean_diff,-log_seed_diff,-coef_nmf_diff,-massey_3yr_diff,-
    #          basis_nmf_diff,-nmf,-elo_2r_diff,-colley_r_diff,-keener_scorediff_diff,-keener_scorerate_diff) %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 21, group = "Season")
  
  tmp <- df_cv %>% 
    mutate(rec = map(splits, prepper, rec))
  
  fit_func <- function(cv, rec){
    dtrain <- xgb.DMatrix(juice(rec) %>% 
                            select(-ID, -target) %>% 
                            as.matrix(),
                          label =  analysis(cv) %>% 
                            select(target) %>% 
                            .[,1])
    dtest<- xgb.DMatrix(cv %>% 
                          assessment() %>% 
                          bake(rec,.) %>% 
                          select(-ID, -target) %>% 
                          as.matrix(),
                        label =  assessment(cv) %>% 
                          select(target) %>% 
                          .[,1])
    params <- list(max_depth = 2,
                   min_child_weight = 5,
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
    watchlist <- list(train= dtrain, test = dtest)
    set.seed(1)
    cv <- xgb.train(params = params, data =dtrain, nrounds = 300,
                    early_stopping_rounds = 30,verbose = F,
                    watchlist = watchlist,
                    prediction = TRUE)
    return(cv)
  }
  # pred_func <- function(fit, split){
  #   df <- assessment(split) %>% 
  #     select(-Season, -ID, -target)
  #   res <- assessment(split) %>% 
  #     select(target) %>% 
  #     mutate(Pred = c(predict(fit, as.matrix(df), type = "response")))
  #   return(res)
  # }
  set.seed(1)
  tmp2 <- tmp %>% 
    mutate(fit = map2(splits, rec, fit_func))
  tmp2 %>% 
    .$fit %>% 
    lapply(function(x) x$evaluation_log[x$best_iteration,]) %>% 
    do.call("rbind",.) %>% 
    # rownames_to_column("iter") %>% 
    summarise_at(vars("test_logloss"), funs(max, mean,median,min,sd)) %>% 
    # as_tibble() %>% 
    print()
  tmp2 %>%
    .$fit %>%
    lapply(function(x) x$evaluation_log[x$best_iteration,]) %>%
    do.call("rbind",.) %>%
    mutate(Season = sapply(tmp2$splits, function(x) unique(assessment(x)$Season))) %>% 
    arrange(Season) %>% 
    summary() %>% 
    print()
  print(i)
  # print(set_lambda_alpha[i,])
}

# xgbtree
for(i in 1:10){
  set.seed(1)
  rec <- tr %>% 
    recipe(target ~ . - ID, data = tr)%>% 
    # step_corr(all_numeric(),-massey_r_diff, threshold = .95) %>%
    step_center(all_numeric()) %>% 
    step_scale(all_numeric())
  
  df_cv <- tr %>% 
    # select(-elo_score_diff,-rating_mean_diff,-log_seed_diff,-coef_nmf_diff,-massey_3yr_diff,-
    #          basis_nmf_diff,-nmf,-elo_2r_diff,-colley_r_diff,-keener_scorediff_diff,-keener_scorerate_diff) %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 21, group = "Season")
  
  tmp <- df_cv %>% 
    mutate(rec = map(splits, prepper, rec))
  
  fit_func <- function(cv, rec){
    dtrain <- xgb.DMatrix(juice(rec) %>% 
                            select(-ID, -target) %>% 
                            as.matrix(),
                          label =  analysis(cv) %>% 
                            select(target) %>% 
                            .[,1])
    dtest<- xgb.DMatrix(cv %>% 
                          assessment() %>% 
                          bake(rec,.) %>% 
                          select(-ID, -target) %>% 
                          as.matrix(),
                        label =  assessment(cv) %>% 
                          select(target) %>% 
                          .[,1])
    params <- list(max_depth = 2,
                   min_child_weight = 1,
                   eta = .1,
                   silent = 10, 
                   booster = "dart",
                   objective = "binary:logistic",
                   eval_metric = "logloss",
                   nthread = 1)
    watchlist <- list(train= dtrain, test = dtest)
    set.seed(1)
    cv <- xgb.train(params = params, data =dtrain, nrounds = 300,
                    early_stopping_rounds = 30,verbose = F,
                    watchlist = watchlist,
                    prediction = TRUE)
    return(cv)
  }
  # pred_func <- function(fit, split){
  #   df <- assessment(split) %>% 
  #     select(-Season, -ID, -target)
  #   res <- assessment(split) %>% 
  #     select(target) %>% 
  #     mutate(Pred = c(predict(fit, as.matrix(df), type = "response")))
  #   return(res)
  # }
  set.seed(1)
  tmp2 <- tmp %>% 
    mutate(fit = map2(splits, rec, fit_func))
  tmp2 %>% 
    .$fit %>% 
    lapply(function(x) x$evaluation_log[x$best_iteration,]) %>% 
    do.call("rbind",.) %>% 
    # rownames_to_column("iter") %>% 
    summarise_at(vars("test_logloss"), funs(max, mean,median,min,sd)) %>% 
    # as_tibble() %>% 
    print()
  tmp2 %>%
    .$fit %>%
    lapply(function(x) x$evaluation_log[x$best_iteration,]) %>%
    do.call("rbind",.) %>%
    mutate(Season = sapply(tmp2$splits, function(x) unique(assessment(x)$Season))) %>% 
    arrange(Season) %>% 
    summary() %>% 
    print()
  print(i)
  # print(set_lambda_alpha[i,])
}

# glm
# PC5, massey_r_diff
# max  mean median   min     sd
# <dbl> <dbl>  <dbl> <dbl>  <dbl>
# 0.565 0.423  0.421 0.348 0.0569


# features selection
# max  mean median   min     sd
# <dbl> <dbl>  <dbl> <dbl>  <dbl>
# 0.539 0.423  0.415 0.345 0.0551

# lasso
# max  mean median   min     sd
# <dbl> <dbl>  <dbl> <dbl>  <dbl>
# 0.538 0.428  0.428 0.369 0.0506

# ridge
# max  mean median   min     sd
# <dbl> <dbl>  <dbl> <dbl>  <dbl>
# 0.539 0.428  0.418 0.360 0.0467

# ranger
# max  mean median   min     sd
# <dbl> <dbl>  <dbl> <dbl>  <dbl>
# 0.537 0.432  0.423 0.354 0.0489

# extratrees
# max  mean median   min     sd
# <dbl> <dbl>  <dbl> <dbl>  <dbl>
# 0.533 0.438  0.439 0.380 0.0438

# gblinear
# max      mean   median      min         sd
# 0.546585 0.4172681 0.416684 0.352239 0.05030751

# # gbtree
# max      mean  median      min         sd
# 0.53284 0.4167458 0.40438 0.357667 0.05247171

# dart
# max      mean   median      min         sd
# 0.532348 0.4168859 0.422548 0.354648 0.05172831