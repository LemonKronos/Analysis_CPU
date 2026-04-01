# nolint start: line_length_linter.
library(dplyr)
library(stringr)
library(ggplot2)

df <- read.csv("Intel_CPUs.csv", stringsAsFactors = FALSE)

# Raw state check
print("--- STATE: df ---")
print(paste("Total Rows:", nrow(df)))
missing_cleaned <- colSums(is.na(df))
print("Missing Values:")
print(sort(missing_cleaned[missing_cleaned > 0], decreasing = TRUE))

df_cleaned <- suppressWarnings({
  df |>
    select(
      Processor_Number, Launch_Date, Recommended_Customer_Price, Vertical_Segment, Lithography,
      nb_of_Cores, nb_of_Threads, Processor_Base_Frequency, Max_Turbo_Frequency, Cache, TDP,
      Max_Memory_Size, Max_nb_of_Memory_Channels, Max_Memory_Bandwidth,
      PCI_Express_Revision, Max_nb_of_PCI_Express_Lanes
    ) |>
    mutate(
      Price_USD = as.numeric(str_replace_all(Recommended_Customer_Price, "[\\$,]", "")),
      Launch_Year = as.numeric(str_extract(Launch_Date, "\\d{2}$")) + 2000,
      Lithography_nm = as.numeric(str_replace(Lithography, " nm", "")),
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
      ),
      Max_nb_of_Memory_Channels = as.numeric(Max_nb_of_Memory_Channels),
      Max_nb_of_PCI_Express_Lanes = as.numeric(Max_nb_of_PCI_Express_Lanes),
      PCI_Express_Revision = as.numeric(str_extract(PCI_Express_Revision, "\\d")), # ADDED
      Memory_Bandwidth_GBs = as.numeric(str_extract(Max_Memory_Bandwidth, "\\d+(\\.\\d+)?"))
    ) |>
    select(-Recommended_Customer_Price, -Launch_Date, -Lithography,
           -Processor_Base_Frequency, -Max_Turbo_Frequency, -Cache,
           -TDP, -Max_Memory_Size, -Max_Memory_Bandwidth)
})

# Cleaned state check
print("--- STATE: df_cleaned ---")
print(paste("Total Rows:", nrow(df_cleaned)))
missing_cleaned <- colSums(is.na(df_cleaned))
print("Missing Values:")
print(sort(missing_cleaned[missing_cleaned > 0], decreasing = TRUE))

# Drop rows with missing critical hardware info
df_filtered <- df_cleaned |>
  filter(
    !is.na(Lithography_nm),
    !is.na(TDP_W),
    !is.na(Base_Frequency_GHz),
    !is.na(Cache_MB)
  )

print("--- STATE: After Dropping NA Rows ---")
print(paste("Total Rows:", nrow(df_filtered)))
missing_filtered <- colSums(is.na(df_filtered))
print("Missing Values:")
print(sort(missing_filtered[missing_filtered > 0], decreasing = TRUE))

# Variable specific Imputations
df_imputed <- df_filtered |>
  # Logic-based imputations(Number of threads & Turbo Frequency)
  mutate(
    nb_of_Threads = coalesce(nb_of_Threads, nb_of_Cores),
    Turbo_Frequency_GHz = coalesce(Turbo_Frequency_GHz, Base_Frequency_GHz)
  ) |>

  # Era-based median (Lithography)
  group_by(Lithography_nm) |>
  mutate(Launch_Year = coalesce(Launch_Year, median(Launch_Year, na.rm = TRUE))) |>
  ungroup() |>

  # Segment & Core-based median (Price)
  group_by(Vertical_Segment, nb_of_Cores) |>
  mutate(Price_USD = coalesce(Price_USD, median(Price_USD, na.rm = TRUE))) |>
  ungroup() |>

  # Segment-based medians (Memory & PCIe limits)
  group_by(Vertical_Segment) |>
  mutate(
    Max_Memory_GB = coalesce(Max_Memory_GB, median(Max_Memory_GB, na.rm = TRUE)),
    Max_nb_of_Memory_Channels = coalesce(Max_nb_of_Memory_Channels, median(Max_nb_of_Memory_Channels, na.rm = TRUE)),
    Max_nb_of_PCI_Express_Lanes = coalesce(Max_nb_of_PCI_Express_Lanes, median(Max_nb_of_PCI_Express_Lanes, na.rm = TRUE))
  ) |>
  ungroup() |>

  # Segment & Era-based median (Memory Bandwidth)
  group_by(Vertical_Segment, Launch_Year) |>
  mutate(Memory_Bandwidth_GBs = coalesce(Memory_Bandwidth_GBs, median(Memory_Bandwidth_GBs, na.rm = TRUE))) |>
  ungroup()

# Safety net: Global median fallback for any groups that were 100% NA
df_final <- df_imputed |>
  mutate(across(where(is.numeric), ~ coalesce(., median(., na.rm = TRUE))))

# Final state check
print("--- STATE: df_final ---")
print(paste("Total Rows:", nrow(df_final)))
missing_final <- colSums(is.na(df_final))
if (length(missing_final[missing_final > 0]) == 0) {
  print("Missing Values: 0! Dataset is completely clean.")
} else {
  print("Missing Values:")
  print(sort(missing_final[missing_final > 0], decreasing = TRUE))
}

# Write to file
write.csv(df_final, "Cleaned_Intel_CPUs_Datas.csv", row.names = FALSE)


# -----------------------------------------------------
# -------------- PCA plotting -------------------------
# -----------------------------------------------------

pca_data <- df_final |> select(where(is.numeric))
pca_result <- prcomp(pca_data, scale. = TRUE)

# Create a dataframe with the 2D coordinates and the CPU Segment for coloring
pca_df <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Segment = df_final$Vertical_Segment
)

# Calculate how much "variance" (information) is captured by the 2 axes
pc1_var <- round(summary(pca_result)$importance[2,1] * 100, 1)
pc2_var <- round(summary(pca_result)$importance[2,2] * 100, 1)

# Plot it!
pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Segment)) +
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
