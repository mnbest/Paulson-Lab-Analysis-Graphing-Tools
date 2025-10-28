# Install required packages if not already installed
install.packages("ggplot2")
install.packages("dunn.test")
install.packages("ggbreak")
install.packages("RColorBrewer")
install.packages("readr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggsignif")  # Added for significance brackets

# Load libraries
library(ggplot2)
library(dunn.test)
library(ggbreak)      # For axis breaks (optional)
library(RColorBrewer)
library(readr)
library(tidyr)
library(dplyr)
library(ggsignif)     # Load ggsignif

# Load CSV
data <- read.csv("~/Desktop/PSP_Protein_Level_Data.csv", header = TRUE, stringsAsFactors = FALSE)
#Replace file destination
# Prepare data (excluding first column, which is sample names/folders)
ncols <- ncol(data)
graph_titles <- colnames(data)[2:ncols]
group_labels <- c(rep("Non-PSP Control", 9), rep("PSP Only", 7), rep("PSP and Other Diagnosis", 7))

dat_long <- data %>% 
  select(-1) %>% 
  mutate(Group = group_labels) %>% 
  pivot_longer(-Group, names_to = "Graph", values_to = "Value")

# Group Colors
colors_fill   <- c("Non-PSP Control"="#489FA7", "PSP Only"="#C43E96", "PSP and Other Diagnosis"="#F2B342")
colors_border <- c("Non-PSP Control"="#306771", "PSP Only"="#8A2060", "PSP and Other Diagnosis"="#FFA600")

# Iterate and plot with updated breaks, brackets, and y-axis lower limit buffer
for (gname in graph_titles) {
  cat("\nProcessing graph:", gname, "\n")
  
  # Remove NAs for this graph only
  dsub <- dat_long %>% filter(Graph == gname & !is.na(Value))
  if (nrow(dsub) == 0) {
    cat("Skipping", gname, "- no data\n")
    next
  }
  cat("Rows in subset:", nrow(dsub), "\n")
  
  # Compute mean/SEM correctly accounting for NAs
  stat_summary <- dsub %>%
    group_by(Group) %>%
    summarize(
      mean = mean(Value, na.rm = TRUE),
      sem  = sd(Value, na.rm = TRUE) / sqrt(sum(!is.na(Value))),
      .groups = "drop"
    )
  cat("Summary stats:\n")
  print(stat_summary)
  
  # Kruskal-Wallis and Dunn's tests
  kw_test <- kruskal.test(Value ~ Group, data = dsub)
  dunn    <- dunn.test(dsub$Value, dsub$Group, method = "bonferroni")
  cat("Kruskal-Wallis p-value:", kw_test$p.value, "\n")
  cat("Dunn test comparisons and P-values:\n")
  print(data.frame(Comparison = dunn$comparisons, P_value = dunn$P))
  
  # Calculate ylimits safely
  ylimits <- quantile(dsub$Value, c(0.05, 0.95), na.rm = TRUE)
  if (any(!is.finite(ylimits)) || (ylimits[1] == ylimits[2])) {
    ylimits <- range(dsub$Value, na.rm = TRUE)
    if (any(!is.finite(ylimits)) || (ylimits[1] == ylimits[2])) {
      ylimits <- c(0,1)
    }
  }
  if (ylimits[1] == ylimits[2]) {
    ylimits <- c(ylimits[1] - 0.5, ylimits[2] + 0.5)
  }
  ybreaks <- pretty(seq(ylimits[1], ylimits[2], length.out = 5))
  cat("Y-axis breaks (pre-adjustment):", ybreaks, "\n")
  
  # Set factor levels in subsets BEFORE plotting
  levels_order <- c("Non-PSP Control", "PSP Only", "PSP and Other Diagnosis")
  dsub$Group <- factor(dsub$Group, levels = levels_order)
  stat_summary$Group <- factor(stat_summary$Group, levels = levels_order)
  
  # Create annotations vector for ggsignif
  comp_order <- c(
    "Non-PSP Control - PSP Only", 
    "Non-PSP Control - PSP and Other Diagnosis",
    "PSP Only - PSP and Other Diagnosis"  # will not plot bracket for this
  )
  pvals <- sapply(comp_order, function(comp) {
    ind <- which(dunn$comparisons == comp)
    if(length(ind) == 1) signif(dunn$P[ind], 3) else NA
  })
  
  # Keep only two comparisons for plotting brackets:
  comp_to_plot <- list(
    c("Non-PSP Control", "PSP Only"),
    c("Non-PSP Control", "PSP and Other Diagnosis")
  )
  pvals_for_pairs <- pvals[1:2]
  p_labels <- paste0("p=", pvals_for_pairs)
  p_labels <- ifelse(is.na(p_labels), "ns", p_labels)
  
  # Calculate max y with buffer (for brackets) and lower y limit with buffer (for points)
  max_y_data <- max(c(dsub$Value, stat_summary$mean + stat_summary$sem), na.rm = TRUE)
  min_y_data <- min(c(dsub$Value), na.rm = TRUE)
  
  bracket_buffer <- max_y_data * 0.1  # smaller padding for less space
  lower_buffer <- (max_y_data - min_y_data) * 0.05
  
  y_upper_limit <- max_y_data + 3 * bracket_buffer
  y_lower_limit <- min_y_data - lower_buffer
  
  # Recalculate ybreaks over full adjusted range with buffer, increase breaks count
  ybreaks <- pretty(c(y_lower_limit, y_upper_limit), n = 7)
  cat("Y-axis breaks (final):", ybreaks, "\n")
  
  # Define y positions for two brackets with tighter spacing
  y_positions <- c(
    max_y_data + 1 * bracket_buffer,
    max_y_data + 1.8 * bracket_buffer
  )
  
  # Build plot
  p <- ggplot(dsub, aes(x=Group, y=Value)) +
    geom_point(aes(fill = Group, color = Group), shape = 21, size = 4, stroke = 1.2, alpha = 0.7) +
    scale_fill_manual(values = colors_fill) +
    scale_color_manual(values = colors_border) +
    geom_errorbar(
      data = stat_summary,
      mapping = aes(x = Group, ymin = mean - sem, ymax = mean + sem),
      width = 0.2, color = "black", linewidth = 1, inherit.aes = FALSE
    ) +
    geom_point(
      data = stat_summary,
      mapping = aes(x = Group, y = mean),
      shape = 18, size = 5, fill = "black", color = "black", inherit.aes = FALSE
    ) +
    scale_y_continuous(breaks = ybreaks) +
    coord_cartesian(ylim = c(y_lower_limit, y_upper_limit)) +
    ggtitle(paste(gname, "\nKruskal-Wallis p=", signif(kw_test$p.value, 3))) +
    theme_bw(base_size = 16) +
    theme(legend.position = "none") +
    geom_signif(
      comparisons = comp_to_plot,
      annotations = p_labels,
      y_position = y_positions,
      tip_length = 0.01,
      textsize = 4
    )
  
  print(p)
  
  # Save plot
  ggsave(filename = paste0(gname, ".png"), plot = p, width = 7, height = 5, dpi = 300)
  cat("Saved plot to", paste0(gname, ".png"), "\n")
}