# ==============================================================================
# SPIDER MONKEY 1ST TRIAL BINOMIAL ANALYSIS
# Visual Discrimination Performance vs. Chance Level (50%)
# Author: Mar Figuerola Folch
# ==============================================================================

# 1. LIBRARIES & GLOBAL SETUP --------------------------------------------------
library(readxl)    # Reading Excel files
library(dplyr)     # Data manipulation & vectorized stats
library(tidyr)     # Reshaping data
library(ggplot2)   # Visualization
library(writexl)   # Clean Excel export

# Input and Output paths
file_path  <- "1st_Trial_Long_Term.xlsx"
output_dir <- "First_Trial_Analysis_Output"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Set global ggplot theme defaults
theme_set(theme_minimal(base_size = 12))

# 2. DATA IMPORT & RESHAPING ---------------------------------------------------
# Read raw sheet and clean empty rows/columns
trial_data_raw <- read_excel(file_path, sheet = "1st Trial") %>%
  filter(!is.na(`Pair number`) & `Pair number` != "") %>%
  select(where(~ !all(is.na(.))))

# Extract monkey names (Columns 2 to 9)
monkey_names <- names(trial_data_raw)[2:9]

# Reshape to tidy long format
long_data <- trial_data_raw %>%
  pivot_longer(
    cols = all_of(monkey_names),
    names_to = "Monkey",
    values_to = "Choice"
  ) %>%
  filter(!is.na(Choice) & Choice != "")

total_trials  <- nrow(long_data)
s_plus_count  <- sum(long_data$Choice == "S+")
s_minus_count <- sum(long_data$Choice == "S-")

# 3. OVERALL STATISTICAL ANALYSIS ----------------------------------------------
# Overall Binomial Test (S+ vs. Chance 0.5)
binom_splus  <- binom.test(s_plus_count, total_trials, p = 0.5, alternative = "two.sided")
binom_sminus <- binom.test(s_minus_count, total_trials, p = 0.5, alternative = "two.sided")

overall_summary <- tibble(
  Choice_Type = c("S+", "S-"),
  Count       = c(s_plus_count, s_minus_count),
  Proportion  = c(s_plus_count / total_trials, s_minus_count / total_trials),
  P_Value     = c(binom_splus$p.value, binom_sminus$p.value),
  CI_Lower    = c(binom_splus$conf.int[1], binom_sminus$conf.int[1]),
  CI_Upper    = c(binom_splus$conf.int[2], binom_sminus$conf.int[2]),
  Significant = ifelse(P_Value < 0.05, "Yes", "No")
)

# 4. VECTORIZED ANALYSIS BY MONKEY & PAIR --------------------------------------
# Helper function for binomial test extraction
run_binom <- function(x, n) {
  res <- binom.test(x, n, p = 0.5, alternative = "two.sided")
  tibble(P_Value = res$p.value, CI_Lower = res$conf.int[1], CI_Upper = res$conf.int[2])
}

# Individual Monkey Summary & Binomial Test (Replaces for-loop + rbind)
monkey_summary <- long_data %>%
  group_by(Monkey) %>%
  summarise(
    Total_Trials = n(),
    S_plus       = sum(Choice == "S+"),
    S_minus      = sum(Choice == "S-"),
    Prop_Splus   = S_plus / Total_Trials,
    .groups      = "drop"
  ) %>%
  rowwise() %>%
  mutate(run_binom(S_plus, Total_Trials)) %>%
  ungroup() %>%
  mutate(Significant = P_Value < 0.05)

# Pair-by-Pair Summary & Binomial Test
pair_summary <- long_data %>%
  group_by(Pair_Number = `Pair number`) %>%
  summarise(
    Total_Trials = n(),
    S_plus       = sum(Choice == "S+"),
    S_minus      = sum(Choice == "S-"),
    Prop_Splus   = S_plus / Total_Trials,
    .groups      = "drop"
  ) %>%
  arrange(Pair_Number) %>%
  rowwise() %>%
  mutate(run_binom(S_plus, Total_Trials)) %>%
  ungroup() %>%
  mutate(Significant = P_Value < 0.05)

# 5. PLOT GENERATION -----------------------------------------------------------

# Plot 1: Overall Choice Distribution
plot_overall <- ggplot(overall_summary, aes(x = Choice_Type, y = Count, fill = Choice_Type)) +
  geom_col(width = 0.6, color = "black") +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", Count, Proportion * 100)), vjust = -0.3, size = 4) +
  geom_hline(yintercept = total_trials / 2, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 1.5, y = (total_trials / 2) + 1.5, label = "Chance Level (50%)", color = "red", fontface = "bold") +
  scale_fill_manual(values = c("S+" = "#4daf4a", "S-" = "#e41a1c")) +
  labs(title = "Overall First Trial Choices", subtitle = paste("Total trials:", total_trials), x = "Choice Type", y = "Number of Choices") +
  theme(legend.position = "none")

# Plot 2: Individual Monkey Performance with Asterisks
plot_monkeys <- ggplot(monkey_summary, aes(x = reorder(Monkey, Prop_Splus), y = Prop_Splus * 100)) +
  geom_col(fill = "darkgreen", width = 0.7, color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%d/%d\n%.1f%%", S_plus, Total_Trials, Prop_Splus * 100)), vjust = -0.3, size = 3.8) +
  # Asterisks for significant performance (p < 0.05)
  geom_text(data = filter(monkey_summary, Significant == TRUE), aes(label = "*"), vjust = -1.8, size = 9, color = "red", fontface = "bold") +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 10), expand = c(0, 0.05)) +
  labs(
    title = "Individual Monkey First-Trial Choice Accuracy",
    x = "Individual Monkey", y = "Percentage of S+ Choices",
    caption = "* indicates significant deviation from chance (p < 0.05, two-tailed binomial test)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 0.5)
  ) +
  coord_cartesian(clip = "off")

# Plot 3: Performance by Stimulus Pair
plot_pairs <- ggplot(pair_summary, aes(x = factor(Pair_Number), y = S_plus)) +
  geom_col(fill = "#ff7f0e", width = 0.7, color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%d/%d", S_plus, Total_Trials)), vjust = -0.3, size = 3.5) +
  geom_text(data = filter(pair_summary, Significant == TRUE), aes(label = "*"), vjust = -1.5, size = 8, color = "red", fontface = "bold") +
  scale_y_continuous(limits = c(0, 9.5), breaks = 0:8, expand = c(0, 0)) +
  labs(
    title = "First-Trial Correct Responses by Stimulus Pair",
    x = "Stimulus Pair Number", y = "Number of S+ Responses",
    caption = "* indicates significant deviation from chance (p < 0.05, two-tailed binomial test)"
  ) +
  theme(
    axis.text = element_text(size = 11),
    axis.line = element_line(color = "black", linewidth = 0.5)
  ) +
  coord_cartesian(clip = "off")

# Display plots
print(plot_overall)
print(plot_monkeys)
print(plot_pairs)

# 6. EXPORT PLOTS & SUMMARY TABLES ---------------------------------------------
# Save High-Res PNG & PDF Figures
ggsave(file.path(output_dir, "plot_overall.png"), plot_overall, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_dir, "plot_monkeys.png"), plot_monkeys, width = 10, height = 6.5, dpi = 300)
ggsave(file.path(output_dir, "plot_pairs.png"), plot_pairs, width = 12, height = 6.5, dpi = 300)

ggsave(file.path(output_dir, "plot_overall.pdf"), plot_overall, width = 8, height = 6)
ggsave(file.path(output_dir, "plot_monkeys.pdf"), plot_monkeys, width = 10, height = 6.5)
ggsave(file.path(output_dir, "plot_pairs.pdf"), plot_pairs, width = 12, height = 6.5)

# Save Clean Excel Workbook with All Summaries
write_xlsx(
  list(
    "Overall_Binomial" = overall_summary,
    "Monkey_Performance" = monkey_summary,
    "Pair_Performance"   = pair_summary
  ),
  path = file.path(output_dir, "Binomial_Analysis_Summary.xlsx")
)

# 7. PRINT SUMMARY REPORT ------------------------------------------------------
cat("\n============================================\n")
cat("       FIRST TRIAL BINOMIAL TEST RESULTS    \n")
cat("============================================\n")
cat("Total valid trials :", total_trials, "\n")
cat("Overall S+ count   :", s_plus_count, sprintf("(%.1f%%)", (s_plus_count/total_trials)*100), "\n")
cat("Binomial P-value   :", round(binom_splus$p.value, 4), "\n")
cat("Significant (p<0.05):", ifelse(binom_splus$p.value < 0.05, "YES", "NO"), "\n")
cat("============================================\n")
