## fig1_fig2_polar_separation.R
##
## Reproduces:
##   Fig 1 -- SOK2/SOK5 polarity vs. Curvature-PE (interface), polar,
##            2x2 grid (rows = SOK2/SOK5, cols = t_x/t_y)
##   Fig 2 -- SOK2 vs PDG at t_y, single polar panel
##
## Input: consolidated_angles.csv (one row per lineage/window, with
## sok_x_angle_deg, sok_y_angle_deg, pdg_angle_deg, curvpe_x_angle_deg,
## curvpe_y_angle_deg, and the *_max/*_min columns used for the quality
## filter), columns `selection` (SOK2/SOK5) and `window`.
##
## Requires: ggplot2
##
## Usage:
##   Rscript fig1_fig2_polar_separation.R /path/to/consolidated_angles.csv

suppressMessages({
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
in_path <- if (length(args) >= 1) args[1] else "consolidated_angles.csv"
here <- dirname(sub("--file=", "", grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE))[1])
if (is.na(here) || here == "") here <- "."
source(file.path(here, "common_axial_stats.R"))

N_PERM <- 10000
SEED <- 0
N_BINS <- 12
FILTER_PCT <- 25

df <- read.csv(in_path, stringsAsFactors = FALSE)
df$sok_x_ratio <- df$sok_x_max / df$sok_x_min
df$sok_y_ratio <- df$sok_y_max / df$sok_y_min

## Build one panel's data: bar heights (observed), 95% null band, observed
## mean-separation ray, and header text. Returns a list consumed by the
## plotting code below.
build_panel <- function(sok, ref, label, n0) {
  n <- length(sok)
  sep <- axial_separation_deg(sok, ref)
  obs_mean <- mean(sep)
  edges <- seq(0, 90, length.out = N_BINS + 1)
  centers <- (edges[-length(edges)] + edges[-1]) / 2
  obs_counts <- as.numeric(table(cut(sep, breaks = edges, include.lowest = TRUE)))

  nd <- permutation_null_histogram(sok, ref, edges, n_perm = N_PERM, seed = SEED)
  lo <- apply(nd$null_counts, 2, quantile, probs = 0.025)
  hi <- apply(nd$null_counts, 2, quantile, probs = 0.975)
  null_center <- mean(nd$null_means)
  p <- (sum(abs(nd$null_means - null_center) >= abs(obs_mean - null_center)) + 1) / (N_PERM + 1)
  sig <- p < 0.05

  list(
    label = label, n = n, n0 = n0, centers = centers, edges = edges,
    obs_counts = obs_counts, lo = lo, hi = hi, obs_mean = obs_mean,
    null_center = null_center, p = p, sig = sig,
    naive_r = n / N_BINS
  )
}

## Render one panel (ggplot object) from build_panel()'s output.
render_panel <- function(pd) {
  bars <- data.frame(center = pd$centers, count = pd$obs_counts)
  band <- data.frame(center = pd$centers, lo = pd$lo, hi = pd$hi)
  rmax <- max(pd$obs_counts, pd$hi) * 1.15
  color <- if (pd$sig) "#c0392b" else "#4a4a4a"
  verdict <- if (pd$sig) "significant" else "not significant"
  title <- sprintf("%s  (n=%d of %d)\nmean sep=%.1f° (null≈%.1f°), p=%.4f — %s",
                    pd$label, pd$n, pd$n0, pd$obs_mean, pd$null_center, pd$p, verdict)

  bin_w <- pd$edges[2] - pd$edges[1]
  ggplot() +
    geom_ribbon(data = band, aes(x = center, ymin = lo, ymax = hi),
                fill = "#cccccc", alpha = 0.6) +
    geom_hline(yintercept = pd$naive_r, linetype = "dotted", color = "black", alpha = 0.45) +
    geom_col(data = bars, aes(x = center, y = count), fill = "#2a78d6",
             color = "white", width = bin_w * 0.92) +
    geom_vline(xintercept = pd$obs_mean, color = color, linewidth = 1.1) +
    geom_vline(xintercept = 45, linetype = "dotted", color = "gray40") +
    coord_polar(theta = "x", start = -pi / 2, direction = 1) +
    scale_x_continuous(limits = c(0, 90), breaks = c(0, 22.5, 45, 67.5, 90),
                        labels = c("0°\n(parallel)", "22.5°", "45°",
                                   "67.5°", "90°\n(perpendicular)")) +
    scale_y_continuous(limits = c(0, rmax)) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(color = color, size = 9, hjust = 0.5))
}

## ---------------------------------------------------------------- Fig 1

selections <- c("SOK2", "SOK5")
anchors <- c("x", "y")
panels <- list()
cat("=== Fig 1: SOK vs Curvature-PE ===\n")
for (sel in selections) {
  for (anchor in anchors) {
    sok_col <- paste0("sok_", anchor, "_angle_deg")
    curv_col <- paste0("curvpe_", anchor, "_angle_deg")
    ratio_col <- paste0("sok_", anchor, "_ratio")
    sel_unfilt <- df[df$selection == sel, ]
    n0 <- sum(complete.cases(sel_unfilt[[sok_col]], sel_unfilt[[curv_col]]))
    sel_df <- filter_within_window(df[df$selection == sel, ], ratio_col, FILTER_PCT)
    ok <- complete.cases(sel_df[[sok_col]], sel_df[[curv_col]])
    sel_df <- sel_df[ok, ]
    pd <- build_panel(sel_df[[sok_col]], sel_df[[curv_col]], sprintf("%s, t_%s", sel, anchor), n0)
    cat(sprintf("  %s t_%s: n=%d (of %d), mean sep=%.1f (null~%.1f), p=%.4f\n",
                sel, anchor, pd$n, pd$n0, pd$obs_mean, pd$null_center, pd$p))
    panels[[paste(sel, anchor)]] <- render_panel(pd)
  }
}

if (requireNamespace("gridExtra", quietly = TRUE)) {
  g <- gridExtra::arrangeGrob(panels[["SOK2 x"]], panels[["SOK2 y"]],
                               panels[["SOK5 x"]], panels[["SOK5 y"]], ncol = 2)
  ggsave("fig1_sok_vs_curvaturepe_polar_R.pdf", g, width = 11, height = 11)
  cat("Saved fig1_sok_vs_curvaturepe_polar_R.pdf\n")
} else {
  cat("NOTE: install.packages('gridExtra') to combine panels into one PDF; ",
      "saving panels separately instead.\n")
  for (nm in names(panels)) {
    ggsave(sprintf("fig1_%s.pdf", gsub(" ", "_", nm)), panels[[nm]], width = 5.5, height = 5.5)
  }
}

## ---------------------------------------------------------------- Fig 2

cat("\n=== Fig 2: SOK2 vs PDG, t_y ===\n")
df$sok_y_ratio <- df$sok_y_max / df$sok_y_min
sel_unfilt <- df[df$selection == "SOK2", ]
n0 <- sum(complete.cases(sel_unfilt$sok_y_angle_deg, sel_unfilt$pdg_angle_deg))
sel_df <- filter_within_window(df[df$selection == "SOK2", ], "sok_y_ratio", FILTER_PCT)
ok <- complete.cases(sel_df$sok_y_angle_deg, sel_df$pdg_angle_deg)
sel_df <- sel_df[ok, ]
pd2 <- build_panel(sel_df$sok_y_angle_deg, sel_df$pdg_angle_deg, "SOK2 polarity vs. PDG, t_y", n0)
cat(sprintf("  n=%d (of %d), mean sep=%.1f (null~%.1f), p=%.4f\n",
            pd2$n, pd2$n0, pd2$obs_mean, pd2$null_center, pd2$p))
ggsave("fig2_sok2_vs_pdg_ty_R.pdf", render_panel(pd2), width = 6, height = 6.3)
cat("Saved fig2_sok2_vs_pdg_ty_R.pdf\n")
