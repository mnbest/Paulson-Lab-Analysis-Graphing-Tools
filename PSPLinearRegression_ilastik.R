# Install and load libraries (uncomment install if needed)
install.packages(c("readxl", "ggplot2", "broom"))
library(readxl)
library(ggplot2)
library(broom)

# Excel file path
excel_file <- "~/Desktop/Full AIS_Mature Tangle_Diffuse AT8_Linear Regression Data.xlsx"

# Group definitions
nonpsp <- c("NPC 1","NPC 2","NPC 4","NPC 5","NPC 6","NPC 9","NPC 10","NPC 11","NPC 12")
psponly <- c("PO 1","PO 9","PO 2","PO 3","PO 5","PO 10","PO 8")
pspother <- c("POD 16","POD 17","POD 18","POD 5","POD 19","POD 20","POD 21")

group_map <- setNames(
  c(rep("Non-PSP Control", length(nonpsp)),
    rep("PSP Only", length(psponly)),
    rep("PSP and Other Diagnosis", length(pspother))),
  c(nonpsp, psponly, pspother)
)
group_map2 <- setNames(
  c(rep("Non-PSP Control", length(nonpsp)),
    rep("PSP", length(psponly) + length(pspother))),
  c(nonpsp, psponly, pspother)
)
colors3 <- c("Non-PSP Control"="#489FA7", "PSP Only"="#C43E96", "PSP and Other Diagnosis"="#F2B342")
colors2 <- c("Non-PSP Control"="black", "PSP"="gray70")

sheet_names <- c("MEDIAN", "MEAN", "DENSITY")
pairs <- list(
  list(x="Full_AIS",  y="Mature_Tangle"),
  list(x="Full_AIS",  y="Diffuse_AT8")
)

for (sheet in sheet_names) {
  df <- read_excel(excel_file, sheet=sheet)
  
  # Rename columns for easier reference
  colnames(df)[1:4] <- c("Sample", "Full_AIS", "Mature_Tangle", "Diffuse_AT8")
  
  # Map groups by Sample
  df$Group <- group_map[df$Sample]
  df$Group2 <- group_map2[df$Sample]
  
  for (pair in pairs) {
    # Explicitly convert to numeric before filtering
    df[[pair$x]] <- as.numeric(df[[pair$x]])
    df[[pair$y]] <- as.numeric(df[[pair$y]])
    
    # Now filter out rows with NA in the x or y variable
    temp <- df[!is.na(df[[pair$x]]) & !is.na(df[[pair$y]]), ]
    if (nrow(temp) < 2) next  # Skip if insufficient data
    
    # Fit linear model for annotations
    fit <- lm(temp[[pair$y]] ~ temp[[pair$x]])
    broomed <- broom::tidy(fit)
    slope <- broomed$estimate[2]
    pval <- broomed$p.value[2]
    r2 <- summary(fit)$r.squared
    reg_label <- sprintf("Slope = %.3f\nR² = %.3f\np = %.3g", slope, r2, pval)
    
    title <- paste(sheet, pair$x, "vs", pair$y)
    fname <- paste(sheet, pair$x, "vs", pair$y, sep="_")
    
    # Plot for three groups
    p3 <- ggplot(temp, aes(x=.data[[pair$x]], y=.data[[pair$y]], color=Group)) +
      geom_point(size=3) +
      scale_color_manual(values=colors3) +
      geom_smooth(method="lm", se=TRUE, color="black", linewidth=1) +
      labs(title=title, color="Group", x=pair$x, y=pair$y) +
      theme_bw(base_size=16) +
      theme(legend.position="top") +
      annotate("text", x=Inf, y=Inf, hjust=1.01, vjust=1.2, label=reg_label, size=5)
    ggsave(paste0(fname, "_3group.png"), plot=p3, width=7, height=6, dpi=300)
    
    # Plot for combined two groups
    p2 <- ggplot(temp, aes(x=.data[[pair$x]], y=.data[[pair$y]], color=Group2)) +
      geom_point(size=3) +
      scale_color_manual(values=colors2) +
      geom_smooth(method="lm", se=TRUE, color="black", linewidth=1) +
      labs(title=paste(title, "(Combined Groups)"), color="Group", x=pair$x, y=pair$y) +
      theme_bw(base_size=16) +
      theme(legend.position="top") +
      annotate("text", x=Inf, y=Inf, hjust=1.01, vjust=1.2, label=reg_label, size=5)
    ggsave(paste0(fname, "_2group.png"), plot=p2, width=7, height=6, dpi=300)
  }
}

# Compares the different groups via ANOVA

install.packages(c("readxl", "ggplot2", "broom")) # Install if missing
library(readxl)
library(ggplot2)
library(broom)

excel_file <- "~/Desktop/Full AIS_Mature Tangle_Diffuse AT8_Linear Regression Data.xlsx"

nonpsp <- c("NPC 1","NPC 2","NPC 4","NPC 5","NPC 6","NPC 9","NPC 10","NPC 11","NPC 12")
psponly <- c("PO 1","PO 9","PO 2","PO 3","PO 5","PO 10","PO 8")
pspother <- c("POD 16","POD 17","POD 18","POD 5","POD 19","POD 20","POD 21")

group_map <- setNames(
  c(rep("Non-PSP Control", length(nonpsp)),
    rep("PSP Only", length(psponly)),
    rep("PSP and Other Diagnosis", length(pspother))),
  c(nonpsp, psponly, pspother)
)
colors3 <- c("Non-PSP Control"="#489FA7", "PSP Only"="#C43E96", "PSP and Other Diagnosis"="#F2B342")

sheet_names <- c("MEDIAN", "MEAN", "DENSITY")
pairs <- list(
  list(x="Full_AIS",  y="Mature_Tangle"),
  list(x="Full_AIS",  y="Diffuse_AT8")
)

for (sheet in sheet_names) {
  df <- read_excel(excel_file, sheet=sheet)
  colnames(df)[1:4] <- c("Sample", "Full_AIS", "Mature_Tangle", "Diffuse_AT8")
  df$Group <- as.factor(group_map[df$Sample])
  
  for (pair in pairs) {
    df[[pair$x]] <- as.numeric(df[[pair$x]])
    df[[pair$y]] <- as.numeric(df[[pair$y]])
    temp <- df[!is.na(df[[pair$x]]) & !is.na(df[[pair$y]]) & !is.na(df$Group), ]
    if(nrow(temp) < 3) next
    
    # --- GROUP-WISE REGRESSIONS ---
    stat_bygroup <- by(
      temp, temp$Group,
      function(subd) {
        m <- lm(subd[[pair$y]] ~ subd[[pair$x]])
        sm <- summary(m)
        c(Slope = coef(m)[2],
          R2 = sm$r.squared,
          P = coef(summary(m))[2,4])
      }
    )
    stat_table <- do.call(rbind, stat_bygroup)
    stat_text <- paste(
      paste0(rownames(stat_table), ": slope=", round(stat_table[,1],3),
             ", R²=", round(stat_table[,2],3), ", p=", signif(stat_table[,3],3)),
      collapse="\n"
    )
    
    # --- INTERACTION REGRESSION: group, x, and group:x ----
    fit_int <- lm(temp[[pair$y]] ~ temp[[pair$x]]*temp$Group)
    fit_int_summ <- summary(fit_int)
    interaction_p <- coef(summary(fit_int))[grep(":", names(coef(fit_int))),4]
    overall_p <- anova(fit_int)["temp[[pair$x]]:temp$Group", "Pr(>F)"]
    
    int_text <- sprintf("Interaction (x*group) p=%.3g", interaction_p[1])
    cat("\nSheet:",sheet,"\n",pair$x,"vs",pair$y,"\n")
    print(fit_int_summ)
    cat("\n---\n")
    
    # --- ANOVA for group differences in Y only (no X) ---
    aov_fit <- aov(temp[[pair$y]] ~ temp$Group)
    aov_p <- summary(aov_fit)[[1]][["Pr(>F)"]][1]
    aov_text <- sprintf("One-way ANOVA (Y ~ group): p=%.3g", aov_p)
    
    # --- 3-group plot with all stats ---
    p3 <- ggplot(temp, aes(x=.data[[pair$x]], y=.data[[pair$y]], color=Group)) +
      geom_point(size=3) +
      scale_color_manual(values=colors3) +
      geom_smooth(method="lm", se=TRUE, aes(group=Group, color=Group), linewidth=1) +
      labs(title=paste(sheet, pair$x, "vs", pair$y), color="Group", x=pair$x, y=pair$y) +
      theme_bw(base_size=16) +
      theme(legend.position="top") +
      annotate("text", x=Inf, y=-Inf, label=stat_text, hjust=1.1, vjust=-.1, size=4, color="black", fontface="italic") +
      annotate("text", x=-Inf, y=Inf, label=int_text, hjust=-0.07, vjust=1.1, size=4, color="black") +
      annotate("text", x=-Inf, y=-Inf, label=aov_text, hjust=-.16, vjust=-.2, size=4, color="gray30")
    ggsave(
      paste0(sheet, "_", pair$x, "_VS_", pair$y, "_groupwise.png"),
      plot=p3, width=8, height=7, dpi=300
    )
  }
}