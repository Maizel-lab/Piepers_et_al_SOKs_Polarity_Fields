## fig4_wall_growth_vs_signal.R
##
## Reproduces Fig 4 -- wall growth vs. SOK signal intensity, 2-panel
## scatter (t_x, t_y), one PDF per selection (SOK2, SOK5).
##
## Input: consolidated_wall_signal.csv (one row per wall/cell-pair, with
## wall_status, is_self_wall, wall_growth_ratio, window,
## signalBorderOverInteriorAverage_{a,b}_{x,y}).
##
## Filter (adopted convention, validated to reproduce the shared memo's
## exact published rho/p for SOK2 and SOK5): wall_status == "both" &
## is_self_wall == FALSE.
##
## duration_hours is parsed from the window name (e.g. "t1-t4" -> 0.5 *
## (4-1) = 1.5h); growth_rate_per_hour = log(wall_growth_ratio) /
## duration_hours; the signal "proxy" for each timepoint is the mean of
## the two cells' (a, b) signalBorderOverInteriorAverage.
##
## Requires: ggplot2
## Usage: Rscript fig4_wall_growth_vs_signal.R consolidated_wall_signal.csv SOK2
##        Rscript fig4_wall_growth_vs_signal.R consolidated_wall_signal.csv SOK5

suppressMessages(library(ggplot2))

args <- commandArgs(trailingOnly = TRUE)
in_path <- if (length(args) >= 1) args[1] else "consolidated_wall_signal.csv"
sel_name <- if (length(args) >= 2) args[2] else "SOK2"

cw <- read.csv(in_path, stringsAsFactors = FALSE)
## is_self_wall / wall_status come from a Python-written CSV as the strings
## "True"/"False" (not an R logical) -- compare as strings, not booleans.
d <- cw[cw$selection == sel_name & cw$wall_status == "both" & cw$is_self_wall == "False", ]

duration_hours <- function(w) {
  parts <- as.numeric(gsub("t", "", strsplit(w, "-")[[1]]))
  0.5 * (parts[2] - parts[1])
}
d$duration_hours <- vapply(d$window, duration_hours, numeric(1))
d$log_growth_ratio <- log(d$wall_growth_ratio)
d$growth_rate_per_hour <- d$log_growth_ratio / d$duration_hours
d$wall_signal_ratio_proxy_x <- rowMeans(d[, c("signalBorderOverInteriorAverage_a_x",
                                               "signalBorderOverInteriorAverage_b_x")], na.rm = FALSE)
d$wall_signal_ratio_proxy_y <- rowMeans(d[, c("signalBorderOverInteriorAverage_a_y",
                                               "signalBorderOverInteriorAverage_b_y")], na.rm = FALSE)

s_x <- d[complete.cases(d$wall_signal_ratio_proxy_x, d$growth_rate_per_hour),
         c("wall_signal_ratio_proxy_x", "growth_rate_per_hour")]
sp_x <- cor.test(s_x$wall_signal_ratio_proxy_x, s_x$growth_rate_per_hour, method = "spearman", exact = FALSE)

s_y <- d[complete.cases(d$wall_signal_ratio_proxy_y, d$growth_rate_per_hour),
         c("wall_signal_ratio_proxy_y", "growth_rate_per_hour")]
sp_y <- cor.test(s_y$wall_signal_ratio_proxy_y, s_y$growth_rate_per_hour, method = "spearman", exact = FALSE)

cat(sprintf("=== %s wall growth vs signal ===\n", sel_name))
cat(sprintf("  t_x: n=%d rho=%+.3f p=%.4f\n", nrow(s_x), unname(sp_x$estimate), sp_x$p.value))
cat(sprintf("  t_y: n=%d rho=%+.3f p=%.4f\n", nrow(s_y), unname(sp_y$estimate), sp_y$p.value))

mkplot <- function(x, y, xlab, title) {
  ggplot(data.frame(x = x, y = y), aes(x, y)) +
    geom_point(size = 1.6, alpha = 0.6, color = "#2a78d6") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray40") +
    labs(x = xlab, y = "growth_rate_per_hour = log(wall_growth_ratio) / duration_h", title = title) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(size = 10, hjust = 0.5))
}

p_x <- mkplot(s_x$wall_signal_ratio_proxy_x, s_x$growth_rate_per_hour,
              "wall_signal_ratio_proxy_x\n(mean signalBorderOverInteriorAverage, t_x)",
              sprintf("t_x : n=%d, rho=%+.3f, p=%.3f", nrow(s_x), unname(sp_x$estimate), sp_x$p.value))
p_y <- mkplot(s_y$wall_signal_ratio_proxy_y, s_y$growth_rate_per_hour,
              "wall_signal_ratio_proxy_y\n(mean signalBorderOverInteriorAverage, t_y)",
              sprintf("t_y : n=%d, rho=%+.3f, p=%.3f", nrow(s_y), unname(sp_y$estimate), sp_y$p.value)) + ylab(NULL)

suptitle <- sprintf("%s: wall growth vs. SOK signal intensity (walls present at both t_x and t_y, excluding self-walls)",
                     sel_name)
out_path <- sprintf("fig4_wall_growth_vs_signal_%s_R.pdf", tolower(sel_name))
if (requireNamespace("gridExtra", quietly = TRUE) && requireNamespace("grid", quietly = TRUE)) {
  g <- gridExtra::arrangeGrob(p_x, p_y, ncol = 2,
                               top = grid::textGrob(suptitle, gp = grid::gpar(fontsize = 11)))
  ggsave(out_path, g, width = 11, height = 4.8)
} else {
  cat("NOTE: install.packages('gridExtra') to combine panels into one PDF; saving panels separately.\n")
  ggsave(sub("_R.pdf", "_R_tx.pdf", out_path), p_x, width = 5.5, height = 4.8)
  ggsave(sub("_R.pdf", "_R_ty.pdf", out_path), p_y, width = 5.5, height = 4.8)
}
cat(sprintf("Saved %s\n", out_path))
