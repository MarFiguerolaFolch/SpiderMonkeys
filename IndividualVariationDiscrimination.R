# ==============================================================================
# SPIDER MONKEY VISUAL DISCRIMINATION: INDIVIDUAL DIFFERENCES IN LEARNING SPEED
# Linear mixed model: Sessions ~ Monkey + (1 | Pair_number)
# ==============================================================================

# 1. SETUP ---------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(rstatix)
library(lme4)
library(lmerTest)
library(emmeans)

# 2. DATA ----------------------------------------------------------------------
data <- read_excel("NumberSessions_StimulusPair.xlsx",
                   sheet = "Session_StimulusPair")
colnames(data)[1] <- "Pair_number"

cat("Stimulus pairs:", nrow(data), "\n")
cat("Monkeys:", paste(colnames(data)[-1], collapse = ", "), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(data)))

long_data <- data %>%
  pivot_longer(-Pair_number, names_to = "Monkey", values_to = "Sessions") %>%
  mutate(Pair_number = factor(Pair_number),
         Sessions    = as.numeric(Sessions))

# 3. DESCRIPTIVE STATISTICS ----------------------------------------------------
desc_stats <- long_data %>%
  group_by(Monkey) %>%
  summarise(
    n_pairs = sum(!is.na(Sessions)),
    Mean    = mean(Sessions, na.rm = TRUE),
    SD      = sd(Sessions, na.rm = TRUE),
    Median  = median(Sessions, na.rm = TRUE),
    Min     = min(Sessions, na.rm = TRUE),
    Max     = max(Sessions, na.rm = TRUE),
    SEM     = SD / sqrt(n_pairs),
    .groups = "drop"
  ) %>%
  arrange(Mean)

cat("\nDESCRIPTIVE STATISTICS (fastest to slowest):\n")
print(desc_stats, n = Inf)

# Order monkeys from fastest to slowest (used by all plots; desc_stats is
# already sorted by Mean, so its row order defines the factor levels)
monkey_levels <- desc_stats$Monkey
long_data  <- long_data  %>% mutate(Monkey = factor(Monkey, levels = monkey_levels))
desc_stats <- desc_stats %>% mutate(Monkey = factor(Monkey, levels = monkey_levels))

# 4. LINEAR MIXED MODEL --------------------------------------------------------
lmm_model <- lmer(Sessions ~ Monkey + (1 | Pair_number),
                  data = long_data, REML = TRUE)

cat("\nMODEL SUMMARY:\n")
print(summary(lmm_model))

lmm_anova <- anova(lmm_model)
cat("\nANOVA TABLE (effect of Monkey):\n")
print(lmm_anova)

cat(sprintf("\nF(%d, %.1f) = %.2f, p = %.4f\n",
            lmm_anova$NumDF[1], lmm_anova$DenDF[1],
            lmm_anova$`F value`[1], lmm_anova$`Pr(>F)`[1]))

# 5. POST-HOC TESTS (Tukey-adjusted) -------------------------------------------
emm <- emmeans(lmm_model, pairwise ~ Monkey, adjust = "tukey")

contrasts_df <- as.data.frame(summary(emm$contrasts, infer = TRUE)) %>%
  dplyr::select(contrast, estimate, SE, df, t.ratio, lower.CL, upper.CL, p.value)

cat("\nALL PAIRWISE CONTRASTS (Tukey-adjusted):\n")
print(contrasts_df, digits = 3)

# Significant contrasts, split into the two monkeys involved
sig_contrasts <- contrasts_df %>%
  filter(p.value < 0.05) %>%
  separate(contrast, into = c("m1", "m2"), sep = " - ", remove = FALSE)

cat("\nSIGNIFICANT CONTRASTS (adjusted p < 0.05):\n")
print(sig_contrasts, digits = 3)

significant_monkeys <- unique(c(sig_contrasts$m1, sig_contrasts$m2))
min_p_nonsig <- contrasts_df %>%
  filter(p.value >= 0.05) %>%
  summarise(min(p.value)) %>%
  pull()

# Ready-to-paste sentences for the Results section
cat("\nTEXT FOR RESULTS:\n")
for (i in seq_len(nrow(sig_contrasts))) {
  with(sig_contrasts[i, ], {
    faster <- ifelse(estimate < 0, m1, m2)
    slower <- ifelse(estimate < 0, m2, m1)
    cat(sprintf(
      "%s reached the learning criterion significantly faster than %s (estimate = %.2f sessions, SE = %.2f, t(%.1f) = %.2f, p = %.3f).\n",
      faster, slower, abs(estimate), SE, df, abs(t.ratio), p.value))
  })
}
cat(sprintf("No other pairwise differences were significant (all adjusted p > %.3f).\n",
            min_p_nonsig))

# 6. FIGURES -------------------------------------------------------------------
theme_thesis <- theme_minimal(base_size = 12) +
  theme(
    axis.text         = element_text(size = 11, color = "black"),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    axis.title        = element_text(size = 13, color = "black", face = "bold"),
    axis.line         = element_line(color = "black", linewidth = 0.5),
    axis.ticks        = element_line(color = "black"),
    legend.position   = "none",
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "gray90", linewidth = 0.3)
  )

# 6a. Boxplot: asterisk above monkeys involved in a significant contrast
asterisk_box <- long_data %>%
  group_by(Monkey) %>%
  summarise(y = max(Sessions, na.rm = TRUE) + 1, .groups = "drop") %>%
  filter(Monkey %in% significant_monkeys)

p_box <- ggplot(long_data, aes(Monkey, Sessions, fill = Monkey)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2, na.rm = TRUE) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.5, na.rm = TRUE) +
  geom_text(data = asterisk_box, aes(y = y, label = "*"),
            color = "red", size = 8, fontface = "bold") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(breaks = seq(0, 20, 5)) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(x = "Individual", y = "Sessions to reach learning criterion") +
  theme_thesis

# 6b. Bar plot (mean ± SEM) with a bracket for every significant contrast.
#     Brackets are stacked above the tallest bar, so no positions are hard-coded.
bracket_step <- 0.6
bracket_tick <- 0.15
y_base <- max(desc_stats$Mean + desc_stats$SEM) + 0.4

brackets <- sig_contrasts %>%
  mutate(
    x1 = match(m1, levels(long_data$Monkey)),
    x2 = match(m2, levels(long_data$Monkey)),
    y  = y_base + (row_number() - 1) * bracket_step
  )

p_bar <- ggplot(desc_stats, aes(Monkey, Mean, fill = Monkey)) +
  geom_col(alpha = 0.7, width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = 0.2, linewidth = 0.6) +
  geom_segment(data = brackets, inherit.aes = FALSE,
               aes(x = x1, xend = x2, y = y, yend = y), linewidth = 0.6) +
  geom_segment(data = brackets, inherit.aes = FALSE,
               aes(x = x1, xend = x1, y = y, yend = y - bracket_tick), linewidth = 0.6) +
  geom_segment(data = brackets, inherit.aes = FALSE,
               aes(x = x2, xend = x2, y = y, yend = y - bracket_tick), linewidth = 0.6) +
  geom_text(data = brackets, inherit.aes = FALSE,
            aes(x = (x1 + x2) / 2, y = y + 0.05, label = "*"),
            color = "red", size = 8, fontface = "bold") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(breaks = scales::breaks_width(1),
                     limits = c(0, max(brackets$y, y_base) + 0.8),
                     expand = c(0, 0)) +
  labs(x = "Individual",
       y = "Mean (± SE) number of sessions to reach learning criterion") +
  theme_thesis +
  theme(axis.title.y = element_text(size = 11, hjust = 0))

# Save (PDF + PNG) and display
save_plot <- function(plot, name) {
  ggsave(paste0(name, ".pdf"), plot, width = 8, height = 6)
  ggsave(paste0(name, ".png"), plot, width = 8, height = 6, dpi = 300)
}
save_plot(p_box, "Boxplot_LearningSpeed")
save_plot(p_bar, "MeanSEM_LearningSpeed")
print(p_box)
print(p_bar)

# 7. MODEL DIAGNOSTICS ---------------------------------------------------------
# Saved to a PDF so it does not add blank pages to the RStudio Plots pane
pdf("Model_Diagnostics.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
plot(fitted(lmm_model), resid(lmm_model),
     xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs fitted")
abline(h = 0, lty = 2)
qqnorm(resid(lmm_model)); qqline(resid(lmm_model))
dev.off()

# 8. FRIEDMAN TEST (non-parametric check; needs complete data) -----------------
# Drops any monkey with missing values (Nena) instead of hard-coding the name
friedman_result <- data %>%
  dplyr::select(where(~ !anyNA(.x))) %>%
  pivot_longer(-Pair_number, names_to = "Monkey", values_to = "Sessions") %>%
  mutate(Monkey = factor(Monkey), Pair_number = factor(Pair_number)) %>%
  friedman_test(Sessions ~ Monkey | Pair_number)

cat("\nFRIEDMAN TEST (complete-data monkeys only):\n")
print(friedman_result)

# 9. SAVE TABLES ---------------------------------------------------------------
write.csv(desc_stats,    "Monkey_Results.csv",             row.names = FALSE)
write.csv(contrasts_df,  "Pairwise_Contrasts_Tukey.csv",   row.names = FALSE)