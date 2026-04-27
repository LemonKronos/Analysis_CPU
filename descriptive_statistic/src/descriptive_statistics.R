library(dplyr)
library(ggplot2)

# Đọc dữ liệu
df <- read.csv("data/Cleaned_Intel_CPUs.csv", stringsAsFactors = FALSE)


# ====================================================================
# 1A. HISTOGRAM CHO PRICE_USD (Chủ động bỏ NA bằng filter)
# ====================================================================
price_mean <- mean(df$Price_USD, na.rm = TRUE)
price_var <- var(df$Price_USD, na.rm = TRUE)
cat("Price - Mean:", price_mean, "| Variance:", price_var, "\n")

# Gán biểu đồ vào biến p_hist_price
p_hist_price <- ggplot(df %>% filter(!is.na(Price_USD)), aes(x = Price_USD)) 
+ geom_histogram(fill = "steelblue", color = "black", bins = 30, alpha = 0.8) 
+ geom_vline(xintercept = price_mean, color = "red", linetype = "dashed", size = 1.2) 
+ theme_minimal() 
+ labs(title = "Distribution of CPU Prices (Price_USD)",
       subtitle = "Red dashed line represents the Mean",
       x = "Price (USD)", 
       y = "Frequency")

# Lưu biểu đồ 1A
ggsave(filename = "descriptive_statistic/figure/Histogram_Price.png", plot = p_hist_price, width = 8, height = 5, dpi = 300)


# ====================================================================
# 1B. HISTOGRAM CHO TDP_W (Giữ nguyên df, không bỏ NA)
# ====================================================================
tdp_mean <- mean(df$TDP_W, na.rm = TRUE)
tdp_var <- var(df$TDP_W, na.rm = TRUE)
cat("TDP - Mean:", tdp_mean, "| Variance:", tdp_var, "\n")

# Gán biểu đồ vào biến p_hist_tdp (truyền thẳng df, không dùng filter)
p_hist_tdp <- ggplot(df, aes(x = TDP_W)) +
  geom_histogram(fill = "coral", color = "black", bins = 30, alpha = 0.8) +
  geom_vline(xintercept = tdp_mean, color = "red", linetype = "dashed", size = 1.2) +
  theme_minimal() +
  labs(title = "Distribution of Thermal Design Power (TDP_W)",
       subtitle = "Red dashed line represents the Mean",
       x = "TDP (Watts)", y = "Frequency")

# Lưu biểu đồ 1B
ggsave(filename = "descriptive_statistic/figure/Histogram_TDP.png", plot = p_hist_tdp, width = 8, height = 5, dpi = 300)


# ====================================================================
# 2. BOXPLOT: PRICE NHÓM THEO VERTICAL SEGMENT
# ====================================================================
df_boxplot <- df %>% filter(!is.na(Price_USD) & !is.na(Vertical_Segment) & Vertical_Segment != "")

# Gán biểu đồ vào biến p_box
p_box <- ggplot(df_boxplot, aes(x = Vertical_Segment, y = Price_USD, fill = Vertical_Segment)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16, outlier.size = 2.5) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 11, face = "bold")) +
  labs(title = "Price Distribution by Vertical Segment",
       subtitle = "Red dots represent Outliers",
       x = "Vertical Segment", y = "Price (USD)")

# Lưu biểu đồ 2
ggsave(filename = "descriptive_statistic/figure/Boxplot_Segment.png", plot = p_box, width = 8, height = 6, dpi = 300)