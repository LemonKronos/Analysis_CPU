### --- Preprocessed data --- ###

# read
data <- read.csv("data/Cleaned_Intel_CPUs.csv", header = TRUE)
cat("Rows original      :", nrow(data), "\n")


# drop NA in price and Max_Memory_GB
data <- data[!is.na(data$Price_USD), ]
data <- data[!is.na(data$Max_Memory_GB), ]
cat("After drop price NA:", nrow(data), "\n")

# export
write.csv(data, "inferential_statistic/data/data_clean.csv", row.names = FALSE)
cat("Saved → inferential_statistic/data/data_clean.csv\n")

### --- Derive data --- ###

set.seed(123)
n <- nrow(data)
train_index <- sample(1:n, size = 0.8 * n)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
cat(
  "Số dòng trong train_data:",
  nrow(train_data), "\n"
)

cat(
  "Số dòng trong test_data :",
  nrow(test_data), "\n"
)

# Create model
model <- lm(Price_USD ~ nb_of_Cores + Turbo_Frequency_GHz + TDP_W + Cache_MB + Max_Memory_GB, data = train_data)
print(summary(model))


# Check model by test_data
predicted <- predict(model, newdata = test_data)
test_data$Predicted_Price_USD <- predicted
print(head(test_data[, c("Price_USD", "Predicted_Price_USD")], 5))

# 4.	Kiểm định giả định mô hình
train_data$residuals <- residuals(model)
shapiro_test <- shapiro.test(train_data$residuals)
print(shapiro_test)

# Q-Q plot
png("inferential_statistic/figure/Q-Q-Residuals.png", width = 14, height = 6, units = "in", res = 150)
qqnorm(train_data$residuals,
  main = "Normal Q-Q Plot of Residuals",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles",
  pch = 16,
  col = "steelblue"
)
qqline(train_data$residuals, col = "red", lwd = 2)
dev.off()

# Check the independent of residuals
library(lmtest)
dw_test <- dwtest(model)
print(dw_test)


# Residuals vs Fitted
png("inferential_statistic/figure/Residuals vs Fitted.png", width = 14, height = 6, units = "in", res = 150)
plot(model$fitted.values, residuals(model),
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted",
  pch = 16, col = "steelblue"
)
abline(h = 0, col = "black", lwd = 2, lty = 2)
lines(lowess(model$fitted.values, residuals(model)), col = "orange", lwd = 2)
dev.off()


# Evaluate model by test_data
library(Metrics)
rmse_value <- rmse(test_data$Price_USD, test_data$Predicted_Price_USD)
r2_value <- cor(test_data$Price_USD, test_data$Predicted_Price_USD)^2
cat("ĐÁNH GIÁ MÔ HÌNH GỐC TRÊN TEST_DATA:\n")
cat("-> RMSE:", round(rmse_value, 4), "\n")
# cat("-> R-quared:", round(r2_value, 4), "\n")
print(head(test_data, 5))


# Nomarlize by log_transform
train_data_log <- train_data
test_data_log <- test_data
num_vars <- c("Price_USD", "nb_of_Cores", "Turbo_Frequency_GHz", "TDP_W", "Cache_MB", "Max_Memory_GB")
for (var in num_vars) {
  train_data_log[[var]] <- log(as.numeric(train_data_log[[var]]))
  test_data_log[[var]] <- log(as.numeric(test_data_log[[var]]))
}

model2 <- lm(Price_USD ~ nb_of_Cores + Turbo_Frequency_GHz + TDP_W + Cache_MB + Max_Memory_GB, data = train_data_log)


cat("\n####################\nMô hình thứ hai\n####################\n")
print(summary(model2))

# Check model by test data
predicted_log <- predict(model2, newdata = test_data_log)
test_data_log$Predicted_Price_USD <- predicted_log
print(head(test_data_log[, c("Price_USD", "Predicted_Price_USD")], 5))


rmse_value_log <- rmse(test_data_log$Price_USD, test_data_log$Predicted_Price_USD)
cat("ĐÁNH GIÁ MÔ HÌNH GỐC TRÊN TEST_DATA_LOG:\n")
cat("-> RMSE:", round(rmse_value_log, 4), "\n")
print(head(test_data_log, 5))


data_wna <- read.csv("data/Cleaned_Intel_CPUs.csv", header = TRUE)

# Preplace col Max_Memory_GB by mean of its value depent on Vertical_Segment
mean_4_segments <- tapply(as.numeric(data_wna$Max_Memory_GB),
  data_wna$Vertical_Segment,
  FUN = function(x) mean(x, na.rm = TRUE)
)
na_rows <- is.na(data_wna$Max_Memory_GB)
segment_of_na <- data_wna$Vertical_Segment[na_rows]
data_wna$Max_Memory_GB[na_rows] <- mean_4_segments[segment_of_na]

data_wna <- data_wna[is.na(data_wna$Price_USD), ]

data_wna_log <- data_wna
predictors <- c("nb_of_Cores", "Turbo_Frequency_GHz", "TDP_W", "Cache_MB", "Max_Memory_GB")

for (var in predictors) {
  data_wna_log[[var]] <- log(as.numeric(data_wna_log[[var]]))
}

log_price_preds <- predict(model2, newdata = data_wna_log)

# Use adjustment by Duan's Smearing (when residual isnot nomarlize)
cf <- mean(exp(residuals(model2)))
price_pred_smearing <- exp(log_price_preds) * cf


data_wna$Predicted_Price_USD <- price_pred_smearing

write.csv(data_wna, "inferential_statistic/data/data_price_predicted.csv", row.names = FALSE)
