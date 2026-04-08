# nolint start: line_length_linter.
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

df <- read.csv("Intel_CPUs.csv", stringsAsFactors = FALSE)

# Helper function to dynamically print missing values with percentages
print_missing <- function(df_data, state_name) {
  # Header with a bit of breathing room
  cat("\n", rep("=", 40), sep = "")
  cat("\nSTATE: ", toupper(state_name))
  cat("\n", rep("=", 40), sep = "")

  tot <- nrow(df_data)
  cat("\nTotal Rows: ", tot, "\n")

  miss <- colSums(is.na(df_data))
  miss <- sort(miss[miss > 0], decreasing = TRUE)

  cat("\nMissing Values:\n")

  if (length(miss) == 0) {
    cat("  ✨ 0! Dataset is clean or NAs are safely accounted for.\n")
  } else {
    # Find max length of column names for alignment
    max_len <- max(nchar(names(miss)))

    for (i in seq_along(miss)) {
      col_name <- names(miss)[i]
      count <- miss[i]
      pct <- format(round((count / tot) * 100, 2), nsmall = 2)

      # Use sprintf for clean, padded columns
      # %-*s creates a left-aligned string with dynamic padding
      cat(sprintf("  %-*s : %d (%s%%)\n", max_len, col_name, count, pct))
    }
  }
  cat(rep("-", 40), "\n", sep = "")
}

# Raw state check
print_missing(df, "df")

df_cleaned <- suppressWarnings({
  df |>
    select(
      Processor_Number, Launch_Date, Recommended_Customer_Price, Vertical_Segment,
      nb_of_Cores, Processor_Base_Frequency, Max_Turbo_Frequency, Cache, TDP, Max_Memory_Size
    ) |>
    mutate(
      Price_USD = as.numeric(str_replace_all(Recommended_Customer_Price, "[\\$,]", "")),
      Launch_Year = as.numeric(str_extract(Launch_Date, "\\d{2}$")) + 2000,
      Base_Frequency_GHz = case_when(
        str_detect(Processor_Base_Frequency, "GHz") ~ as.numeric(str_replace(Processor_Base_Frequency, " GHz", "")),
        str_detect(Processor_Base_Frequency, "MHz") ~ as.numeric(str_replace(Processor_Base_Frequency, " MHz", "")) / 1000,
        TRUE ~ NA_real_
      ),
      Turbo_Frequency_GHz = case_when(
        str_detect(Max_Turbo_Frequency, "GHz") ~ as.numeric(str_replace(Max_Turbo_Frequency, " GHz", "")),
        str_detect(Max_Turbo_Frequency, "MHz") ~ as.numeric(str_replace(Max_Turbo_Frequency, " MHz", "")) / 1000,
        TRUE ~ NA_real_
      ),
      Cache_MB = as.numeric(str_extract(Cache, "\\d+(\\.\\d+)?")),
      TDP_W = as.numeric(str_extract(TDP, "\\d+(\\.\\d+)?")),
      Max_Memory_GB = case_when(
        str_detect(Max_Memory_Size, "TB") ~ as.numeric(str_extract(Max_Memory_Size, "\\d+(\\.\\d+)?")) * 1024,
        TRUE ~ as.numeric(str_extract(Max_Memory_Size, "\\d+(\\.\\d+)?"))
      )
    ) |>
    select(
      Processor_Number, Vertical_Segment, nb_of_Cores, Price_USD, Launch_Year,
      Base_Frequency_GHz, Turbo_Frequency_GHz, Cache_MB, TDP_W, Max_Memory_GB
    )
})

# Cleaned state check
print_missing(df_cleaned, "df_cleaned")

# Drop small number of rows with missing critical hardware info
df_filtered <- df_cleaned |>
  filter(
    !is.na(TDP_W),
    !is.na(Base_Frequency_GHz),
    !is.na(Cache_MB)
  )

print_missing(df_filtered, "After Dropping NA Rows")

# Variable specific Imputations & Trimming
df_final <- df_filtered |>
  # Logic-based imputations (Turbo Frequency fallback to Base)
  mutate(
    Turbo_Frequency_GHz = coalesce(Turbo_Frequency_GHz, Base_Frequency_GHz)
  ) |>
  select(
    Processor_Number, Vertical_Segment, nb_of_Cores, Price_USD, Launch_Year,
    Turbo_Frequency_GHz, Cache_MB, TDP_W, Max_Memory_GB
  )

# Final state check
print_missing(df_final, "df_final")

# Write to file
write.csv(df_final, "Cleaned_Intel_CPUs.csv", row.names = FALSE)


# -----------------------------------------------------
# -------------- PCA plotting -------------------------
# -----------------------------------------------------

# PCA strictly requires Complete Cases (no NAs). This drops rows just for the plot dataset.
pca_df <- df_final |> drop_na()

pca_data <- pca_df |> select(where(is.numeric))
pca_result <- prcomp(pca_data, scale. = TRUE)

# Create a dataframe with the 2D coordinates and the CPU Segment for coloring
pca_plot_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Segment = pca_df$Vertical_Segment
)

# Calculate how much "variance" (information) is captured by the 2 axes
pc1_var <- round(summary(pca_result)$importance[2, 1] * 100, 1)
pc2_var <- round(summary(pca_result)$importance[2, 2] * 100, 1)

# Plot it!
pca_plot <- ggplot(pca_plot_data, aes(x = PC1, y = PC2, color = Segment)) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "PCA of Intel CPUs: Visualizing Hardware Groupings",
    subtitle = "Points closer together have similar hardware specifications",
    x = paste0("Principal Component 1 (", pc1_var, "% of Variance)"),
    y = paste0("Principal Component 2 (", pc2_var, "% of Variance)"),
    color = "CPU Segment"
  ) +
  theme(legend.position = "bottom")

# Show in console and save as a high-res image
print(pca_plot)
ggsave("Intel_CPU_PCA_Map.png", pca_plot, width = 8, height = 6, dpi = 300)
print("PCA Plot saved as 'Intel_CPU_PCA_Map.png'!")
# nolint end: line_length_linter.