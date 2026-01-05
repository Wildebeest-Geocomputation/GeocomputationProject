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
  # AIC: (2*K - 2*LL) + (2*K)*(K+1)/(n-K-1)
  AICc <- (2 * K - 2 * LL) + (2 * K * (K + 1)) / (n - K - 1)
  # if parameters exceed samples, return Inf
  if ((n - K - 1) <= 0) return(Inf)

  return(AICc)
}

grid_search <- function(
    data, pa, regmult_vals, feature_classes
    ) {

  # l: linear
  # q: quadratic
  # h: hinge
  # p: product
  # t: threshold

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
    model, data
    ) {

  base_dir <- "./Data/Performance/"
  # AUC
  library(ROCR)
  pred <- predict(me_model, newdata = model_data, type = "logistic")
  pred_obj <- prediction(pred, pa)
  auc <- performance(pred_obj, measure = "auc")
  auc@y.values[[1]]

  pred_obj <- prediction(pred, pa)
  roc_perf <- performance(pred_obj, measure = "tpr", x.measure = "fpr")

  # Y (TPR, True Positive Rate / Sensitivity)
  # X (FPR, False Positive Rate)
  png(paste0(base_dir, 'roc.png'), width = 2000, height = 2000, res = 300)
  plot(roc_perf,
       main = "ROC Curve",
       ylab = "True Positive Rate (Sensitivity)",
       xlab = "False Positive Rate")
  abline(a = 0, b = 1, lty = 2, col = "gray")
  dev.off()

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
  # saveRDS(cm, "./Data/Performance/model_confusion_matrix.rds")
  saveRDS(cm, paste0(base_dir, 'model_confusion_matrix.rds'))
  # readRDS("./Data/Performance/model_confusion_matrix.rds")

  cm <- confusionMatrix(pred_factor, actual_factor)
  cm_table <- as.data.frame(cm$table)
  cm_table%>%
    ggplot(aes(Reference, Prediction)) +
    geom_tile(aes(fill = Freq), color = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(label = Freq), vjust = 1) +
    labs(title = "Confusion Matrix", fill = "Count") +
    theme_minimal()
  ggsave(paste0(base_dir, 'confusion_matrix.png'), width = 6, height = 4, dpi = 300)
}
