# Packages
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr"))  install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggpubr"))  install.packages("ggpubr")

library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Read the data
data <- read_excel("/Users/maizellab06/Pictures/Dissertation Figures/publication/Fig2/CarboTag/data/2025-10-25_CarboTag-BDP_2domains.xlsx")
data$Region <- as.factor(data$Region)

# Define your palette (same as bar plot)
bar_colors <- c("#d62728", "#1f77b4")

# y-axis limits
y_lower <- 2.5
y_upper <- 3.5

# n per Region for axis labels
region_counts <- data %>%
  count(Region)

axis_lab_with_n <- function(x) {
  paste0(x, "\n(n=", region_counts$n[match(x, region_counts$Region)], ")")
}

# All pairwise comparisons for Wilcoxon tests
comparisons <- combn(levels(data$Region), 2, simplify = FALSE)

# Plot
p <- ggplot(data, aes(x = Region, y = MeanIntR, fill = Region)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 1) +
  geom_jitter(width = 0.1, size = 4, alpha = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 5, fill = "white") +
  scale_fill_manual(values = bar_colors) +          # << your colors
  scale_y_continuous(limits = c(y_lower, y_upper),
                     breaks = seq(y_lower, y_upper, by = 0.2)) +
  scale_x_discrete(labels = axis_lab_with_n, expand = c(0.4, 0)) +
  labs(x = "", y = "") +
  theme_classic(base_size = 20) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey80", size = 0.5)
  )


# Add Wilcoxon stars with Bonferroni correction
p + stat_compare_means(method = "wilcox.test",
                       comparisons = comparisons,
                       label = "p.signif",
                       p.adjust.method = "bonferroni",
                       hide.ns = TRUE)

