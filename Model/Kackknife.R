library(pROC)

# Get predictions
pred_full <- as.vector(predict(me_model, model_data, type = "logistic"))
roc_obj <- roc(pa, pred_full, quiet = TRUE)
auc_full <- as.numeric(auc(roc_obj))

# Calculate Youden threshold
coords_all <- coords(roc_obj, "all", ret = c("threshold", "sensitivity", "specificity"))
youden_index <- coords_all$sensitivity + coords_all$specificity - 1
threshold <- coords_all$threshold[which.max(youden_index)]

# TSS Calculation
pred_binary <- ifelse(pred_full >= threshold, 1, 0)
cm <- table(Observed = pa, Predicted = pred_binary)
sensitivity <- cm[2,2] / sum(cm[2,])
specificity <- cm[1,1] / sum(cm[1,])
tss <- sensitivity + specificity - 1

cat("\n=== MODEL PERFORMANCE ===\n")
cat("AUC:", round(auc_full, 4), "\n")
cat("TSS:", round(tss, 4), "\n")
cat("Threshold:", round(threshold, 4), "\n")
cat("Sensitivity:", round(sensitivity, 4), "\n")
cat("Specificity:", round(specificity, 4), "\n\n")

# Jackknife Test
var_names <- names(model_data)
jack_results <- data.frame(
  Variable = var_names,
  AUC_only = numeric(length(var_names)),
  AUC_without = numeric(length(var_names))
)

cat("=== JACKKNIFE TEST ===\n")
for (i in seq_along(var_names)) {
  var <- var_names[i]

  # Without this variable
  data_without <- model_data[, setdiff(names(model_data), var), drop = FALSE]
  model_without <- maxnet(p = pa, data = data_without)
  pred_without <- as.vector(predict(model_without, data_without, type = "logistic"))
  auc_without <- as.numeric(auc(roc(pa, pred_without, quiet = TRUE)))

  # Only this variable - FIX: ensure it's a proper data.frame
  data_only <- as.data.frame(model_data[, var, drop = FALSE])
  names(data_only) <- var
  model_only <- maxnet(p = pa, data = data_only)
  pred_only <- as.vector(predict(model_only, newdata = data_only, type = "logistic"))
  auc_only <- as.numeric(auc(roc(pa, pred_only, quiet = TRUE)))

  jack_results$AUC_only[i] <- auc_only
  jack_results$AUC_without[i] <- auc_without

  cat(gsub("_suitability", "", var), ":\n")
  cat("  Only:", round(auc_only, 3), " | Without:", round(auc_without, 3),
      " | Drop:", round(auc_full - auc_without, 3), "\n")
}

# Plot Jackknife
jack_results$Variable <- gsub("_suitability", "", jack_results$Variable)

par(mar = c(5, 8, 4, 2))
barplot(t(as.matrix(jack_results[, -1])), beside = TRUE, horiz = TRUE,
        names.arg = jack_results$Variable, las = 1,
        col = c("darkgreen", "red"),
        main = paste0("Jackknife Test (Full AUC = ", round(auc_full, 3), ")"),
        xlab = "AUC", xlim = c(0, 1))
abline(v = auc_full, lty = 2, lwd = 2, col = "blue")
legend("bottomright", legend = c("With only", "Without", "Full model"),
       fill = c("darkgreen", "red", NA), border = c("black", "black", NA),
       lty = c(NA, NA, 2), col = c(NA, NA, "blue"), bty = "n")
