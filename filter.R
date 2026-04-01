# nolint start: line_length_linter.
library(dplyr)
library(stringr)

df <- read.csv("Intel_CPUs.csv", stringsAsFactors = FALSE)

df_cleaned <- df |>
  select(
    Processor_Number, Launch_Date, Recommended_Customer_Price, Vertical_Segment, Lithography,
    nb_of_Cores, nb_of_Threads, Processor_Base_Frequency, Max_Turbo_Frequency, Cache, TDP,
    Max_Memory_Size, Max_nb_of_Memory_Channels, Max_Memory_Bandwidth,
    Processor_Graphics_, PCI_Express_Revision, Max_nb_of_PCI_Express_Lanes
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
    Memory_Bandwidth_GBs = as.numeric(str_extract(Max_Memory_Bandwidth, "\\d+(\\.\\d+)?"))
  ) |>
  select(-Recommended_Customer_Price, -Launch_Date, -Lithography,
         -Processor_Base_Frequency, -Max_Turbo_Frequency, -Cache,
         -TDP, -Max_Memory_Size, -Max_Memory_Bandwidth)

df_final <- df_cleaned |>
  mutate(across(where(is.numeric), ~ coalesce(., median(., na.rm = TRUE)))) |>
  mutate(Processor_Graphics_ = ifelse(is.na(Processor_Graphics_) | Processor_Graphics_ == "", "Unknown", Processor_Graphics_))

colSums(is.na(df_final))

write.csv(df_final, "Cleaned_Intel_CPUs_Final.csv", row.names = FALSE)
# nolint end: line_length_linter.