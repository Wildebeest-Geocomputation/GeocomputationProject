library(pROC)

k <- 5

pres_idx <- which(pa == 1)
bg_idx <- which(pa == 0)

folds <- numeric(length(pa))
folds[pres_idx] <- sample(cut(seq(1, length(pres_idx)), breaks = k, labels = FALSE))
folds[bg_idx] <- sample(cut(seq(1, length(bg_idx)),   breaks = k, labels = FALSE))

cv_auc_scores <- numeric(k)

for(i in 1:k){

  train_idx <- which(folds != i)
  test_idx <- which(folds == i)

  train_data <- model_data[train_idx, ]
  train_p <- pa[train_idx]
  test_data <- model_data[test_idx, ]
  test_p <- pa[test_idx]

  best_reg <- as.numeric(grid_search_result$best_params[[1]])

  best_fc <- as.character(grid_search_result$best_params[[2]])

  cv_model <- maxnet(p = train_p,
                     data = train_data,
                     regmult = best_reg,
                     f = maxnet.formula(train_p, train_data, classes = best_fc))
  pred_test <- predict(cv_model, test_data, type = "logistic")
  roc_obj <- roc(test_p, as.vector(pred_test), quiet = TRUE)
  cv_auc_scores[i] <- auc(roc_obj)

  message(paste("Fold", i, "AUC:", round(cv_auc_scores[i], 4)))
}

mean_auc <- mean(cv_auc_scores)
sd_auc <- sd(cv_auc_scores)

message(paste0("Mean AUC: ", round(mean_auc, 4)))
message(paste0("SD AUC:   ", round(sd_auc, 4)))
