# Install needed packages if not installed
install.packages(c("readxl", "pheatmap"))

library(readxl)
library(pheatmap)

# Path to your Excel file; replace with actual path
excel_file <- "~/Desktop/PSP HEAT MAP DATA 20X_AT8_summary_table_all_classes.xlsx"

# Expected row and column order
expected_rows <- c(
  "NPC 1", "NPC 2", "NPC 4", "NPC 5", "NPC 6", "NPC 7", "NPC 9", "NPC 10",
  "NPC 11", "NPC 12", "PO 1", "PO 9", "PO 2", "PO 3", "PO 5", "PO 10",
  "PO 8", "POD 16", "POD 17", "POD 18", "POD 5", "POD 19", "POD 20", "POD 21"
)

expected_cols <- c("Mature Tangle", "Diffuse Tau(pS202/pT205)", "Single Aggregate", "Combined Aggregate")

# Define exactly which sheet corresponds to which heatmap type
sheets_to_plot <- list(
  Density = "Density",           # Replace with actual density sheet tab name
  MeanIntensity = "Mean_Intensity",   # Replace with actual mean intensity tab name
  MedianIntensity = "Median_Intensity" # Replace with actual median intensity tab name
)

for (plot_type in names(sheets_to_plot)) {
  sheet <- sheets_to_plot[[plot_type]]
  
  df_raw <- read_excel(excel_file, sheet = sheet)
  
  # Remove unnamed or duplicated columns 
  colnames_raw <- names(df_raw)
  cols_to_keep <- !is.na(colnames_raw) & colnames_raw != "" & !duplicated(colnames_raw)
  df_raw <- df_raw[, cols_to_keep]
  
  # Make unique row names from first column values
  row_names <- make.unique(as.character(df_raw[[1]]))
  df <- as.data.frame(df_raw[, -1, drop = FALSE])
  rownames(df) <- row_names
  
  # Optionally customize expected columns per plot_type if needed
  expected_cols_used <- expected_cols  # default to your known columns
  # e.g., if plot_type == "MedianIntensity", expected_cols_used <- c(...)
  
  # Subset rows and columns with drop = FALSE to keep structure
  rows_present <- expected_rows[expected_rows %in% rownames(df)]
  cols_present <- expected_cols_used[expected_cols_used %in% colnames(df)]
  df <- df[rows_present, cols_present, drop = FALSE]
  
  # Convert to numeric matrix safely
  data_matrix <- as.matrix(data.frame(lapply(df, as.numeric)))
  rownames(data_matrix) <- rows_present
  colnames(data_matrix) <- cols_present
  
  # Check if matrix contains any finite data before plotting
  if (!any(is.finite(data_matrix))) {
    message(paste("Sheet", sheet, "contains no finite numeric data; skipping."))
    next
  }
  
  # Log-transform matrix for color scaling
  log_data <- log1p(data_matrix)
  
  # Create character matrix for labels, keep NAs as empty strings
  labels_matrix <- matrix(
    ifelse(is.na(data_matrix), "", format(data_matrix, big.mark = ",")),
    nrow = nrow(data_matrix), ncol = ncol(data_matrix)
  )
  rownames(labels_matrix) <- rownames(data_matrix)
  colnames(labels_matrix) <- colnames(data_matrix)
  
  # Create file name using plot_type to label files clearly
  file_name <- paste0(plot_type, "_heatmap.png")
  
  # Save heatmap as PNG
  png(filename = file_name, width = 800, height = 1000)
  pheatmap(
    log_data,
    scale = "none",
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    display_numbers = labels_matrix,
    number_color = "black",
    na_col = "lightgrey",          
    fontsize_number = 24,
    fontsize_row = 24,
    fontsize_col = 24,
    color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
    main = paste(plot_type, "Heatmap")
  )
  dev.off()
}