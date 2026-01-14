# data_x <- model_data[, setdiff(names(model_data), "pa")]
#
# numeric_vars <- names(data_x)[sapply(data_x, is.numeric)]
# interaction_data <- data.frame(row.names = 1:nrow(data_x))
#
# count <- 0
# for (i in 1:(length(numeric_vars) - 1)) {
#   for (j in (i + 1):length(numeric_vars)) {
#
#     var1_name <- numeric_vars[i]
#     var2_name <- numeric_vars[j]
#
#     new_col_name <- paste0(var1_name, "_x_", var2_name)
#     interaction_data[[new_col_name]] <- data_x[[var1_name]] * data_x[[var2_name]]
#
#     count <- count + 1
#   }
# }
#
# full_interaction_data <- cbind(data_x, interaction_data)
#
# pa <- c(rep(1, nrow(presence_clean)), rep(0, nrow(bg_data)))
#
# my_formula <- maxnet.formula(p = pa, data = full_interaction_data, classes = 'lp')
# me_model <- maxnet(p = pa, data = full_interaction_data, f = my_formula, regmult = 0.05)


library(vip)
library(ROCR)
pred_wrapper <- function(object, newdata) {
  predict(object, newdata, type = "logistic")
}

auc_metric <- function(truth, estimate) {
  pred_obj <- ROCR::prediction(estimate, truth)
  # auc is the roc area under the curve
  perf <- ROCR::performance(pred_obj, "auc")
  return(perf@y.values[[1]])
}

importance_scores <- vi_permute(
  me_model,
  # train = full_interaction_data,
  train = model_data,
  target = pa,
  metric = auc_metric,
  pred_wrapper = pred_wrapper,
  nsim = 10,
  smaller_is_better = FALSE
)

plot_data <- importance_scores %>%
  arrange(desc(Importance)) %>%
  head(10) %>%
  mutate(Type = ifelse(str_detect(Variable, "_x_"), "Interaction", "Single"))

plot_data%>%
  ggplot() +
  geom_col(aes(x = reorder(Variable, Importance), y = Importance)) +
  coord_flip() +
  labs(x = "Variable", y = "Importance Score (Decrease in AUC)")



library(GGally)
model_data[,c("solar_suitability")]
ggpairs(model_data[,c("Annual_Median_Temperature_2001_2020", "solar_suitability")],
        title = "Scatter Plot Matrix (Spearman Correlation)",
        upper = list(continuous = wrap("cor",
                                       method = "spearman",
                                       size = 4,
                                       color = "black")),
        lower = list(continuous = wrap("smooth",
                                       alpha = 0.3,
                                       size = 0.1,
                                       color = "blue"))) +
  theme_minimal()


library(car)
vif_model <- lm(pa ~., data = model_data)
vif_result <- vif(vif_model)
vif_result
vif_df <- data.frame(
  Variable = names(vif_result),
  VIF = as.numeric(vif_result)
)
vif_df %>%
  arrange(desc(VIF)) %>%
  ggplot() +
  geom_col(aes(x = reorder(Variable, VIF), y = VIF)) +
  coord_flip() +
  labs(x = "Variable", y = "Variance Inflation Factor (VIF)") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = 5.5, label = "VIF = 5", color = "red", hjust = 0)
