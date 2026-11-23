# ==============================================================================
# Visual Discrimination & Learning Criterion in Black-Handed Spider Monkeys
# Author: Mar Figuerola Folch
# ==============================================================================

# 1. LIBRARIES & SETUP ---------------------------------------------------------
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)

# File path definition
file_path <- "1st S mean_LC mean.xlsx"

# Set global ggplot theme defaults
theme_set(theme_minimal(base_size = 12))

# 2. HELPER FUNCTIONS ----------------------------------------------------------
# Function to compute summary statistics (Mean, SD, n, SE) across trial columns
calculate_row_summary <- function(df, trial_cols = 2:10) {
  df %>%
    rowwise() %>%
    mutate(
      Mean = mean(c_across(all_of(trial_cols)), na.rm = TRUE),
      SD   = sd(c_across(all_of(trial_cols)), na.rm = TRUE),
      n    = sum(!is.na(c_across(all_of(trial_cols)))),
      SE   = SD / sqrt(n)
    ) %>%
    ungroup() %>%
    select(Pair_number, Mean, SD, n, SE)
}

# 3. DATA IMPORT & CLEANING ----------------------------------------------------
# Import raw sheets
first_session_raw   <- read_excel(file_path, sheet = "1st session")
sessions_crit_raw   <- read_excel(file_path, sheet = "nºs reach criterion")

# Clean Pair_number column
first_session_clean <- first_session_raw %>%
  mutate(Pair_number = as.numeric(.[[1]])) %>%
  filter(!is.na(Pair_number))

sessions_crit_clean <- sessions_crit_raw %>%
  mutate(Pair_number = as.numeric(.[[1]])) %>%
  filter(!is.na(Pair_number))

# 4. DATA PROCESSING & SUMMARIZATION -------------------------------------------
# Compute summary stats for Sheet 1 & Sheet 2 using the helper function
first_summary    <- calculate_row_summary(first_session_clean)
sessions_summary <- calculate_row_summary(sessions_crit_clean)

# Assign performance colors cleanly using case_when()
first_summary <- first_summary %>%
  mutate(
    Bar_Color = case_when(
      Pair_number %in% c(1, 2, 3, 5) ~ "#FF8C00",   # Orange
      Pair_number %in% c(4, 6)       ~ "#FFD700",   # Gold/Yellow
      Pair_number %in% c(7, 8, 9)    ~ "#90EE90",   # Light Green
      Mean <= 5                      ~ "#90EE90",   # Chance level (≤5)
      Mean > 5                       ~ "#006400"    # Above chance / criterion (>5)
    )
  )

# 5. PLOT GENERATION -----------------------------------------------------------

# PLOT 1: First Session Correct Responses
plot1 <- ggplot(first_summary, aes(x = Pair_number, y = Mean)) +
  geom_col(aes(fill = Bar_Color), color = "black", alpha = 0.85, width = 0.8) +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE),
    width = 0.25, color = "darkblue", linewidth = 0.7
  ) +
  geom_hline(yintercept = 8, color = "red", linewidth = 0.6, linetype = "dashed") +
  geom_hline(yintercept = 5, color = "blue", linewidth = 0.6, linetype = "dotted") +
  scale_fill_identity() +  # Reads color hex/names directly from data frame
  scale_x_continuous(breaks = 1:max(first_summary$Pair_number)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10, expand = c(0, 0)) +
  labs(
    x = "Stimulus Pair",
    y = "Mean Correct Trials",
    title = "First Session Correct Responses Across Stimulus Pairs",
    caption = "Red dashed line: Learning criterion (8/10) | Blue dotted line: Chance level (5/10)"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

# PLOT 2: Sessions to Reach Learning Criterion
plot2 <- ggplot(sessions_summary, aes(x = Pair_number, y = Mean)) +
  geom_col(fill = "darkgreen", color = "black", alpha = 0.85, width = 0.8) +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE),
    width = 0.25, color = "darkblue", linewidth = 0.7
  ) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", linewidth = 0.8) +
  scale_x_continuous(
    breaks = 1:max(sessions_summary$Pair_number),
    expand = expansion(add = c(0.3, 0.3))
  ) +
  scale_y_continuous(
    breaks = seq(0, 16, by = 2),
    limits = c(0, 16),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Stimulus Pair",
    y = "Mean (±SE) Sessions to Criterion",
    title = "Sessions Required to Reach Learning Criterion"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(-0.15, "cm"),  # True inward ticks
    axis.text.x = element_text(margin = margin(t = 7)),
    axis.text.y = element_text(margin = margin(r = 7)),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

# Display plots in RStudio viewer
print(plot1)
print(plot2)

# 6. EXPORTING PLOTS & DATA ----------------------------------------------------

# Save high-resolution publication figures
ggsave("first_session_performance.png", plot1, width = 10, height = 6, dpi = 300)
ggsave("sessions_to_criterion.png", plot2, width = 10, height = 6, dpi = 300)

# Format summary tables for export
first_session_results <- first_summary %>%
  transmute(
    Pair_Number = Pair_number,
    Mean_Correct = round(Mean, 2),
    SE = round(SE, 2),
    SD = round(SD, 2),
    Sample_Size = n
  )

sessions_criterion_results <- sessions_summary %>%
  transmute(
    Pair_Number = Pair_number,
    Mean_Sessions = round(Mean, 2),
    SE = round(SE, 2),
    SD = round(SD, 2),
    Sample_Size = n
  )

# Export to single formatted Excel workbook
write_xlsx(
  list(
    "First_Session_Summary" = first_session_results,
    "Sessions_Criterion_Summary" = sessions_criterion_results
  ),
  path = "spider_monkey_cognitive_results.xlsx"
)

# Export individual CSV files
write.csv(first_session_results, "first_session_results.csv", row.names = FALSE)
write.csv(sessions_criterion_results, "sessions_to_criterion_results.csv", row.names = FALSE)

# Print Summary Statistics to Console
cat("\n==========================================\n")
cat("       SUMMARY DIAGNOSTICS & RESULTS      \n")
cat("==========================================\n")
cat("Mean correct responses (1st Session):", round(mean(first_summary$Mean), 2), "\n")
cat("Pairs reaching criterion (≥8):", sum(first_summary$Mean >= 8), "/", nrow(first_summary), "\n")
cat("Pairs at chance level (≤5):", sum(first_summary$Mean <= 5), "/", nrow(first_summary), "\n")
cat("Mean sessions to criterion:", round(mean(sessions_summary$Mean), 2), "\n")
cat("Fastest pair learning:", min(sessions_summary$Mean), "sessions\n")
cat("Slowest pair learning:", max(sessions_summary$Mean), "sessions\n")
cat("==========================================\n")
