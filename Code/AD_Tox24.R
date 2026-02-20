library(readr)
library(dplyr)
library(applicable)
library(ggplot2)

#data <- read_csv("C:/Users/zth3/OneDrive - CDC/QSAR_TOX24/data_for_wining_team/jupyter_notebook/gbr_results_with_standardized_residuals.csv")
data <- read_csv("code/gbr_results_with_standardized_residuals.csv")
desc_data <- data %>% 
  select(-c(SMILES, Actual, Predicted, Residual, `Standardized_Residual`, `Prediction_Quality`))
desc_train <- desc_data %>% filter(Set == "Training") %>% select(-Set)
desc_test <- desc_data %>% filter(Set == "Test") %>% select(-Set)
desc_val <- desc_data %>% filter(Set == "Validation") %>% select(-Set)

# Normalization
train_means <- sapply(desc_train, mean, na.rm = TRUE)
train_sds <- sapply(desc_train, sd, na.rm = TRUE)

scale_with_train <- function(df, means, sds) {
  as.data.frame(scale(df, center = means, scale = sds))
}

desc_train_scaled <- scale_with_train(desc_train, train_means, train_sds)
desc_test_scaled <- scale_with_train(desc_test, train_means, train_sds)
desc_val_scaled <- scale_with_train(desc_val, train_means, train_sds)

# AD Model
ad_model <- apd_hat_values(desc_train_scaled)
train_scores <- score(ad_model, new_data = desc_train_scaled, type = "numeric")
test_scores <- score(ad_model, new_data = desc_test_scaled, type = "numeric")
val_scores <- score(ad_model, new_data = desc_val_scaled, type = "numeric")
cutoff <- 0.08

# Result
data$leverage <- NA
data$AD <- NA
data$leverage[data$Set == "Training"] <- train_scores$hat_values
data$leverage[data$Set == "Test"] <- test_scores$hat_values
data$leverage[data$Set == "Validation"] <- val_scores$hat_values

data$AD_Label <- ifelse(data$leverage > cutoff, "Out", "In")
data$DataType <- ifelse(data$Set == "Training", "Train",
                        ifelse(data$Set == "Test", "Test", "Validation"))

# Plots
ggplot(data %>% filter(Set %in% c("Training", "Test")),
                aes(x = seq_along(leverage), y = leverage, color = DataType)) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_hline(yintercept = cutoff, linetype = "dotdash", color = "red", size = 1) +
  annotate("text", x = 50, y = cutoff + 0.005, label = paste("threshold:", cutoff), hjust = 0, size = 4) +
  labs(title = "Leverage Plot: Training and Test",
       x = "Index",
       y = "Leverage",
       color = "Set") +
  theme_minimal(base_size = 13)

ggplot(data %>% filter(Set %in% c("Training", "Validation")),
                aes(x = seq_along(leverage), y = leverage, color = DataType)) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_hline(yintercept = cutoff, linetype = "dotdash", color = "red", size = 1) +
  annotate("text", x = 50, y = cutoff + 0.005, label = paste("threshold:", cutoff), hjust = 0, size = 4) +
  labs(title = "Leverage Plot: Training and Validation",
       x = "Index",
       y = "Leverage",
       color = "Set") +
  theme_minimal(base_size = 13)

# Save results
library(writexl)
data <- data %>% select(-AD, -DataType)
write_xlsx(data, "full_data_with_AD_all_columns.xlsx")
getwd()

