# If see plus sign pres esc

library(tidyverse)

#Choose where the files are and were the final summary will be
parent_dir <- "/Volumes/Merci_4TB/ILASTIK - TRIM46/40X_TRIM46_CSV" #Change this
folders <- list.dirs(parent_dir, recursive = FALSE, full.names = TRUE)
folders <- folders[grepl("_csv$", basename(folders), ignore.case = TRUE)]

#Check to make sure data present
#print(folders)

summarize_folder_multiple_classes <- function(folder) {
  files <- list.files(folder, pattern = "_table\\.csv$", full.names = TRUE)
  df <- files %>% map_dfr(function(file) {
    temp <- read_csv(file, show_col_types = FALSE)
    if (nrow(temp) == 0) return(NULL)
    if ("Mean Intensity" %in% names(temp)) {
      temp <- temp %>% mutate(`Mean Intensity` = as.numeric(`Mean Intensity`))
    } else {
      temp$`Mean Intensity` <- NA_real_
    }
    # Normalize Predicted Class names
    temp <- temp %>%
      mutate(`Predicted Class` = case_when(
        `Predicted Class` %in% c("Mature Tangle", "Mature Tangles") ~ "Mature Tangles",
        `Predicted Class` %in% c("Full Axon Initial Segment", "Full Axon Initial Segments") ~ "Full Axon Initial Segment",
        TRUE ~ `Predicted Class`
      ))
    temp
  })
  
  if (is.null(df) || nrow(df) == 0) {
    return(tibble(Folder = basename(folder)))
  }
  
  folder_base <- basename(folder)
  
  classes_set1 <- c("Mature Tangles", "Single Aggregate", "Diffuse AT8", "Combined Aggregate")
  classes_set2 <- c("Full Axon Initial Segment", "Partial Axon Initial Segment", "Combined Axon Initial Segment", "Artifact")
  
  classes_to_use <- if (any(df$`Predicted Class` %in% classes_set1)) classes_set1 else classes_set2
  
  summary_list <- map(classes_to_use, function(cls) {
    subset_cls <- df %>% filter(`Predicted Class` == cls)
    tibble(
      !!paste0(cls, "_Total_Count") := nrow(subset_cls),
      !!paste0(cls, "_Mean_Intensity") := mean(subset_cls$`Mean Intensity`, na.rm = TRUE),
      !!paste0(cls, "_Median_Intensity") := median(subset_cls$`Mean Intensity`, na.rm = TRUE)
    )
  })
  
  summary_combined <- bind_cols(tibble(Folder = folder_base), bind_cols(summary_list))
  summary_combined
}

# Apply function to all folders
summary_tbl_all_classes <- folders %>% map_dfr(summarize_folder_multiple_classes)

print(summary_tbl_all_classes)
write_csv(summary_tbl_all_classes, file.path(parent_dir, "summary_table_all_classes.csv"))