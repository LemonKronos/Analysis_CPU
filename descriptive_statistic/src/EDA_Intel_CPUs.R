suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(corrplot)
  library(tidyr)
  library(patchwork)
})

# Đọc dữ liệu đã được làm sạch từ file CSV
df <- read.csv("Cleaned_Intel_CPUs.csv", stringsAsFactors = FALSE)

# ====================================================================
# 2. THỐNG KÊ TÓM TẮT CÁC BIẾN CÒN LẠI
# ====================================================================

summary_vars <- c("nb_of_Cores", "Turbo_Frequency_GHz", "Cache_MB", "Max_Memory_GB", "Launch_Year")

summary_stats <- data.frame(
  Variable = summary_vars,
  Min = round(sapply(df[, summary_vars], function(x) min(x, na.rm = TRUE)), 2),
  Median = round(sapply(df[, summary_vars], function(x) median(x, na.rm = TRUE)), 2),
  Mean = round(sapply(df[, summary_vars], function(x) mean(x, na.rm = TRUE)), 2),
  Max = round(sapply(df[, summary_vars], function(x) max(x, na.rm = TRUE)), 2),
  NA_Count = sapply(df[, summary_vars], function(x) sum(is.na(x))),
  row.names = NULL
)

print(summary_stats)

# ============================================================================
# 2.1: ĐỒ THỊ PHÂN TÁN VỚI PRICE_USD
# ============================================================================

# 1. Chuẩn bị dữ liệu cho từng biểu đồ để xử lý NA nhất quán theo từng cặp biến
scatter_info <- data.frame(
  Variable = c("nb_of_Cores", "Turbo_Frequency_GHz", "Cache_MB", "Max_Memory_GB", "Launch_Year"),
  Label = c("(a) Theo Số lượng nhân", "(b) Theo Xung nhịp", "(c) Theo Bộ đệm Cache", "(d) Theo RAM hỗ trợ", "(e) Theo Năm ra mắt"),
  X_Label = c("nb_of_Cores", "Turbo_Frequency_GHz (GHz)", "Cache_MB", "Max_Memory_GB", "Launch_Year"),
  Color = c("forestgreen", "orchid", "cyan4", "salmon", "steelblue"),
  stringsAsFactors = FALSE
)

# Loại NA theo cặp (X, Price_USD) và giữ Price_USD > 0 để dùng được bản log10
for (i in seq_len(nrow(scatter_info))) {
  x_var <- scatter_info$Variable[i]
  keep_idx <- complete.cases(df[, c(x_var, "Price_USD")]) & is.finite(df[[x_var]]) & is.finite(df$Price_USD) & (df$Price_USD > 0)
  scatter_info$Used_N[i] <- sum(keep_idx)
  scatter_info$Dropped_N[i] <- nrow(df) - scatter_info$Used_N[i]
}

make_scatter_plot <- function(data, x_var, title_text, x_label, point_color, log_price = FALSE) {
  p <- ggplot(data, aes(x = .data[[x_var]], y = Price_USD)) +
    geom_point(color = point_color, alpha = 0.5, size = 1.5) +
    labs(title = title_text, x = x_label, y = "Price_USD") +
    theme_bw() +
    theme(plot.title = element_text(size = 11, face = "italic"))

  if (log_price) {
    p <- p + scale_y_log10() + labs(y = "Price_USD (log10 scale)")
  }
  
  p
}

# 2. Vẽ từng biểu đồ phân tán với dữ liệu đã lọc theo từng cặp biến
plot_list <- vector("list", nrow(scatter_info))
plot_list_log <- vector("list", nrow(scatter_info))

for (i in seq_len(nrow(scatter_info))) {
  x_var <- scatter_info$Variable[i]
  keep_idx <- complete.cases(df[, c(x_var, "Price_USD")]) & is.finite(df[[x_var]]) & is.finite(df$Price_USD) & (df$Price_USD > 0)
  plot_data <- df[keep_idx, , drop = FALSE]

  plot_list[[i]] <- make_scatter_plot(
    data = plot_data,
    x_var = x_var,
    title_text = scatter_info$Label[i],
    x_label = scatter_info$X_Label[i],
    point_color = scatter_info$Color[i],
    log_price = FALSE
  )

  plot_list_log[[i]] <- make_scatter_plot(
    data = plot_data,
    x_var = x_var,
    title_text = scatter_info$Label[i],
    x_label = scatter_info$X_Label[i],
    point_color = scatter_info$Color[i],
    log_price = TRUE
  )
}

# 3. Ghép 5 biểu đồ lại thành 1 lưới (Layout) bằng patchwork
combined_scatter <- (plot_list[[1]] | plot_list[[2]]) /
                    (plot_list[[3]] | plot_list[[4]]) /
                    (plot_list[[5]] | plot_spacer()) +
  plot_annotation(
    title = "Đồ thị phân tán của Price_USD theo các đặc tính kỹ thuật",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# 4. Thêm phiên bản log10 để quan sát xu hướng khi Price_USD lệch phải
combined_scatter_log <- (plot_list_log[[1]] | plot_list_log[[2]]) /
                        (plot_list_log[[3]] | plot_list_log[[4]]) /
                        (plot_list_log[[5]] | plot_spacer()) +
  plot_annotation(
    title = "Đồ thị phân tán (log10 Price_USD) theo các đặc tính kỹ thuật",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Lưu biểu đồ ra file
ggsave("ScatterPlots_Combined.png", plot = combined_scatter, width = 10, height = 12, dpi = 300)
ggsave("ScatterPlots_Combined_LogPrice.png", plot = combined_scatter_log, width = 10, height = 12, dpi = 300)

# ====================================================================
# 2.3. MA TRẬN TƯƠNG QUAN PEARSON + HEATMAP
# ====================================================================

numerical_vars <- c(
  "Price_USD", "nb_of_Cores", "Turbo_Frequency_GHz",
  "Launch_Year", "TDP_W", "Cache_MB", "Max_Memory_GB"
)

df_num <- df %>% select(all_of(numerical_vars))

# complete.obs: chỉ dùng các dòng đầy đủ cả 7 biến để tránh hệ số giả do NA
cor_matrix <- cor(df_num, use = "complete.obs", method = "pearson")

# Lưu heatmap tương quan ra file PNG
png(filename = "Heatmap_Correlation.png", width = 1600, height = 1200, res = 200)
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  diag = TRUE,
  col = colorRampPalette(c("blue", "white", "red"))(200),
  title = "Ma trận Tương quan Pearson - 7 Biến Số",
  mar = c(0, 0, 2, 0),
  tl.cex = 0.9,
  cl.cex = 0.8,
  number.cex = 0.7
)
invisible(dev.off())
