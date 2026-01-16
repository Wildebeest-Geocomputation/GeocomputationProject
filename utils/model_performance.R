# Calculates the corrected AIC (AICc) of Hurvich and Tsai (1989). The AICc modifies the standard AIC with a correction for small sample sizes.
# https://www.rdocumentation.org/packages/sme/versions/1.0.2/topics/AICc

calculate_AICc <- function(model, p, data) {
  # based on https://jamiemkass.github.io/ENMeval/reference/aic.maxent.html
  preds <- predict(model, data, type = "logistic")
  pres_preds <- preds[p == 1]
  abs_preds <- preds[p == 0]
  # Raw values at occurrence localities
  std_pres_preds <- pres_preds / sum(abs_preds)
  # Log-Likelihood: sum(log(vals))
  LL <- sum(log(std_pres_preds))
  # number of non-zero coefficients (m$betas for maxnet)
  K <- length(model$betas)
  # number of occurrence localities
  n <- length(pres_preds)
  # AIC: 2 * K - 2 * LL
  AICc <- (2 * K - 2 * LL) + (2 * K * (K + 1)) / (n - K - 1)
  # if parameters exceed samples, return Inf
  if ((n - K - 1) <= 0) return(Inf)
  # original aic
  # AIC_value <- 2 * K - 2 * LL

  return(AICc)
}

grid_search <- function(
    data, pa, regmult_vals, feature_classes
    ) {

  # l: linear, original features
  # q: quadratic, squared terms
  # h: hinge, piecewise linear functions, allows for complex non-linear trends
  # p: product, this is interaction and will do product for all pairs of features
  # t: threshold, step functions, creates a binary split (0 or 1) at a specific cut-off value

  best_score <- Inf
  best_model <- NULL

  for (rm in regmult_vals) {
    for (fc in feature_classes) {
      try({
        my_formula <- maxnet.formula(p = pa, data = data, classes = fc)
        mod <- maxnet(p = pa, data = data, f = my_formula, regmult = rm)
        score <- calculate_AICc(mod, p = pa, data = data)

        message(paste("RM:", rm, "| FC:", fc, "| AICc:", round(score, 2)))

        if (!is.na(score) && score < best_score) {
          best_score <- score
          best_model <- mod
          best_params <- c(rm, fc)
        }
      }, silent = FALSE)
    }
  }
  return(list(best_model = best_model, best_score = best_score, best_params = best_params))
}

maxent_model_report <- function(
    model, data, pa_vector
    ) {

  base_dir <- "./Data/Performance/"
  # AUC
  library(ROCR)
  pred <- predict(me_model, newdata = model_data, type = "logistic")
  pred_obj <- prediction(pred, pa)
  auc <- performance(pred_obj, measure = "auc")
  print(paste("AUC:", auc@y.values[[1]]))


  pred_obj <- prediction(pred, pa)
  roc_perf <- performance(pred_obj, measure = "tpr", x.measure = "fpr")

  optimal_threshold <- 0.5

  tpr_vals <- roc_perf@y.values[[1]]
  fpr_vals <- roc_perf@x.values[[1]]
  cutoffs <- roc_perf@alpha.values[[1]]

  idx_05 <- which.min(abs(cutoffs - 0.5))
  thresh_05 <- cutoffs[idx_05]
  tpr_05 <- tpr_vals[idx_05]
  fpr_05 <- fpr_vals[idx_05]

  # Youden Index (Sensitivity + Specificity - 1 = TPR - FPR)
  youden_index <- tpr_vals - fpr_vals
  best_idx <- which.max(youden_index)
  optimal_threshold <- cutoffs[best_idx]

  # Y (TPR, True Positive Rate / Sensitivity)
  # X (FPR, False Positive Rate)
  # png(paste0(base_dir, 'roc.png'), width = 2000, height = 2000, res = 300)
  # plot(roc_perf,
  #      main = "ROC Curve",
  #      ylab = "True Positive Rate (Sensitivity)",
  #      xlab = "False Positive Rate")
  #
  # abline(a = 0, b = 1, lty = 2, col = "gray")
  # points(fpr_05, tpr_05, col = "blue", pch = 19, cex = 1.5)
  # text(fpr_05, tpr_05, labels = paste0("Default (0.5)\nSens: ", round(tpr_05, 2)), pos = 4, col = "blue", cex = 0.8, offset = 1)
  # points(fpr_vals[best_idx], tpr_vals[best_idx], col = "red", pch = 19, cex = 1.5)
  # text(fpr_vals[best_idx], tpr_vals[best_idx], labels = paste0("Opt Cutoff\n", round(optimal_threshold, 3)), pos = 4, col = "red", cex = 0.8)
  # dev.off()

  # confusion matrix
  library(caret)
  pred_factor <- factor(ifelse(pred > 0.5, 1, 0), levels = c(0, 1))
  actual_factor <- factor(pa, levels = c(0, 1))

  # No Information Rate: if we always predict the majority class
  # P-Value [Acc > NIR]: if our model is significantly better than NIR
  # Kappa: after deducting the probability of "just guessing correctly", the model's true discrimination ability
  # Mcnemar's Test P-Value: Bias, [(true false) - (false true)]^2/[(true false) + (false true)], finding two biased predictions
  # Sensitivity: TP / (TP + FN)
  # Specificity: TN / (TN + FP)
  # Pos Pred Value: TP / (TP + FP)
  # Neg Pred Value: TN / (TN + FN)
  # Prevalence: (TP + FN) / Total (same as NIR)
  # Detection Rate: TP / Total
  # Detection Prevalence: (TP + FP) / Total
  # Balanced Accuracy: (Sensitivity + Specificity) / 2
  confusionMatrix(pred_factor, actual_factor)
  # In our case, false positive means that we predict data centre is a good location when it is not.
  # saveRDS(cm, paste0(base_dir, 'model_confusion_matrix.rds'))
  # readRDS("./Data/Performance/model_confusion_matrix.rds")

  actual_factor <- factor(pa_vector, levels = c(0, 1))

  pred_factor_def <- factor(ifelse(pred > 0.5, 1, 0), levels = c(0, 1))
  cm_default <- confusionMatrix(pred_factor_def, actual_factor, positive = "1")
  saveRDS(cm_default, paste0(base_dir, 'model_confusion_matrix_default.rds'))
  plot_cm(cm_default, "Confusion Matrix (Default 0.5)",
          paste0(base_dir, "confusion_matrix_default.png"))
  # Youden (Optimized)
  pred_factor_opt <- factor(ifelse(pred > optimal_threshold, 1, 0), levels = c(0, 1))
  cm_youden <- confusionMatrix(pred_factor_opt, actual_factor, positive = "1")
  saveRDS(cm_youden, paste0(base_dir, 'model_confusion_matrix_youden.rds'))
  plot_cm(cm_youden, paste0("Confusion Matrix (Youden ", round(optimal_threshold, 3), ")"),
          paste0(base_dir, "confusion_matrix_youden.png"))

  return(list(
    default = list(threshold = 0.5, cm = cm_default),
    youden = list(threshold = optimal_threshold, cm = cm_youden)
  ))
}

plot_cm <- function(cm_obj, title_text, file_name) {
  cm_table <- as.data.frame(cm_obj$table)
  p <- ggplot(cm_table, aes(Reference, Prediction)) +
    geom_tile(aes(fill = Freq), color = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(label = Freq), vjust = 1, size = 5) +
    labs(title = title_text,
         fill = "Count",
         subtitle = paste0("Acc: ", round(cm_obj$overall['Accuracy'], 3),
                           " Sens: ", round(cm_obj$byClass['Sensitivity'], 3),
                           " Spec: ", round(cm_obj$byClass['Specificity'], 3))) +
    theme_minimal()

  print(p)
  ggsave(file_name, plot = p, width = 6, height = 4, dpi = 300)
}
