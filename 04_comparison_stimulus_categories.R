##TASK DIFFICULTY BY STIMULUS CATEGORY
# Kruskal-Wallis test on mean sessions per stimulus pair + Dunn post-hoc (BH)
R

#Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(rstatix)    # For statistical tests
library(ggpubr)     # For publication-ready plots

#Install FSA if not already installed (for Dunn's test)
if (!require(FSA)) {
  install.packages("FSA")
  library(FSA)
}



#Data
categories <- c("H", "V", "V + H", "SO", "G")
category_labels <- c("H" = "Hor", "V" = "Ver", "V + H" = "Hor + Ver",
                     "SO" = "SpO", "G" = "Geo")

#One sheet per category -> one long data frame
combined_data <- lapply(categories, function(cat_name) {
  read_excel("Mean_Sessions_Pairs.xlsx", sheet = cat_name) %>%
    setNames(c("Stimulus_Pair", "Mean_Sessions")) %>%
    mutate(Stimulus_Pair = as.character(Stimulus_Pair),
           Category      = cat_name)
}) %>%
  bind_rows() %>%
  mutate(Mean_Sessions = as.numeric(Mean_Sessions),
         Category      = factor(Category, levels = categories))

cat("Sample size per category:\n")
print(table(combined_data$Category))

#Descriptive statistics
desc_stats <- combined_data %>%
  group_by(Category) %>%
  summarise(
    n      = n(),
    Mean   = mean(Mean_Sessions, na.rm = TRUE),
    SD     = sd(Mean_Sessions, na.rm = TRUE),
    SEM    = SD / sqrt(n),
    Median = median(Mean_Sessions, na.rm = TRUE),
    IQR    = IQR(Mean_Sessions, na.rm = TRUE),
    Min    = min(Mean_Sessions, na.rm = TRUE),
    Max    = max(Mean_Sessions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Median))

cat("\nDESCRIPTIVE STATISTICS (highest to lowest median):\n")
print(desc_stats, n = Inf)

#Kruskal-wallis test
kw <- kruskal_test(combined_data, Mean_Sessions ~ Category)

cat("\nKRUSKAL-WALLIS TEST:\n")
cat(sprintf("H(%d) = %.3f, p = %.4f, N = %d\n", kw$df, kw$statistic, kw$p, kw$n))

#Dunn's post-hoc test (only if overall test is significant)
if (kw$p < 0.05) {
  dunn_res <- dunnTest(Mean_Sessions ~ Category, data = combined_data,
                       method = "bh")$res %>%       # Benjamini-Hochberg correction
    arrange(P.adj)
  
  cat("\nDUNN'S POST-HOC TEST (BH-adjusted):\n")
  print(dunn_res)
} else {
  cat("\nOverall test not significant: no post-hoc test needed.\n")
}

#Figures
theme_thesis <- theme_minimal(base_size = 12) +
  theme(
    axis.text.x       = element_text(size = 12, color = "black"),
    axis.text.y       = element_text(size = 12, color = "black"),
    axis.title.x      = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y      = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.line         = element_line(color = "black", linewidth = 0.5),
    axis.ticks        = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position   = "none",
    panel.grid.major  = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor  = element_blank()
  )

#A.Boxplot with individual stimulus pairs; red diamond = median
p_box <- ggplot(combined_data, aes(Category, Mean_Sessions, fill = Category)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.15, height = 0, size = 2.5, alpha = 0.7, color = "gray30") +
  stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "red") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = category_labels) +
  scale_y_continuous(breaks = scales::breaks_width(2), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Stimulus category", y = "Mean sessions to learning criterion") +
  theme_thesis

#B.Bar plot: mean ± SEM
p_bar <- ggplot(desc_stats, aes(Category, Mean, fill = Category)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = 0.2, linewidth = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = category_labels) +
  scale_y_continuous(breaks = scales::breaks_width(1),
                     limits = c(0, max(desc_stats$Mean + desc_stats$SEM) * 1.2),
                     expand = c(0, 0)) +
  labs(x = "Stimulus category",
       y = "Mean (± SE) number of sessions to reach learning criterion") +
  theme_thesis

print(p_box)
print(p_bar)

#Save plots and files
output_dir <- "Category_Analysis_Output"
dir.create(output_dir, showWarnings = FALSE)

save_plot <- function(plot, name) {
  ggsave(file.path(output_dir, paste0(name, ".png")), plot,
         width = 10, height = 7, dpi = 300)
  ggsave(file.path(output_dir, paste0(name, ".pdf")), plot,
         width = 10, height = 7)
}
save_plot(p_box, "boxplot_category_comparison")
save_plot(p_bar, "barplot_category_means")

write.csv(combined_data, file.path(output_dir, "combined_category_data.csv"), row.names = FALSE)
write.csv(desc_stats,    file.path(output_dir, "descriptive_statistics.csv"), row.names = FALSE)
if (exists("dunn_res")) {
  write.csv(dunn_res,    file.path(output_dir, "dunn_posthoc.csv"),           row.names = FALSE)
}
