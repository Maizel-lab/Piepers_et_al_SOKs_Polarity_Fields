## fig3_anisotropy_vs_polarity.R
##
## Reproduces Fig 3 -- growth anisotropy vs. SOK polarity (orientation +
## abundance), 4-panel scatter, one PDF per selection (SOK2, SOK5).
##
## Input: a per-lineage table with pdg_max, pdg_min, sok_x_angle_deg,
## sok_y_angle_deg, log_signalTotal_x, log_signalTotal_y, and tissue_x
## (e.g. pdg_vs_signal_sok2.csv / pdg_vs_signal_sok5.csv).
##
## NOTE (see the Python make_all_pdfs.py docstring for the full story):
## the historical SOK2 figure (pre-dating this reconstruction; its
## generating script no longer exists) reported r^2=0.01, p=0.484 for the
## t_x orientation panel specifically. That exact number could not be
## reverse-engineered despite trying several plausible variants; every
## other panel (t_y orientation, both abundance panels, and the aniso
## formula itself) matched exactly. This script therefore uses the
## project's own standard circular-linear statistic (Mardia 1976, R^2 on the
## axially-doubled angle), with significance from a permutation test
## (10,000 shuffles of log anisotropy across cells), as stated in the Methods.
## The parametric F-test p is printed alongside for reference.
##
## Requires: ggplot2
## Usage: Rscript fig3_anisotropy_vs_polarity.R pdg_vs_signal_sok2.csv SOK2 FALSE
##        Rscript fig3_anisotropy_vs_polarity.R pdg_vs_signal_sok5.csv SOK5 TRUE

suppressMessages(library(ggplot2))

args <- commandArgs(trailingOnly = TRUE)
in_path <- if (length(args) >= 1) args[1] else "pdg_vs_signal_sok2.csv"
sel_name <- if (length(args) >= 2) args[2] else "SOK2"
no_caveat <- if (length(args) >= 3) as.logical(args[3]) else FALSE

here <- dirname(sub("--file=", "", grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE))[1])
if (is.na(here) || here == "") here <- "."
source(file.path(here, "common_axial_stats.R"))

suppressMessages(library(scales))

df <- read.csv(in_path, stringsAsFactors = FALSE)
df$aniso <- df$pdg_max / df$pdg_min
df$log_aniso <- log(df$aniso)
## %in% keeps cells with a missing tissue label (a plain != would turn them into all-NA rows)
sub <- df[!(df$tissue_x %in% "Exclude from analyses") & !is.na(df$log_aniso), ]
n <- nrow(sub)

## panel 1 + 2: circular-linear (orientation)
N_PERM <- 10000
m1 <- mardia_circ_lin_perm(sub$sok_x_angle_deg, sub$log_aniso, n_perm = N_PERM, seed = 0)
m2 <- mardia_circ_lin_perm(sub$sok_y_angle_deg, sub$log_aniso, n_perm = N_PERM, seed = 1)

## panel 3 + 4: Pearson (abundance, both variables log-transformed)
s3 <- sub[complete.cases(sub$log_signalTotal_x, sub$log_aniso), c("log_signalTotal_x", "log_aniso")]
sp3 <- cor.test(s3$log_signalTotal_x, s3$log_aniso, method = "pearson")
s4 <- sub[complete.cases(sub$log_signalTotal_y, sub$log_aniso), c("log_signalTotal_y", "log_aniso")]
sp4 <- cor.test(s4$log_signalTotal_y, s4$log_aniso, method = "pearson")

cat(sprintf("=== %s anisotropy vs polarity ===\n", sel_name))
cat(sprintf("  t_x orientation: n=%d r2=%.3f p_perm=%.4f (F-test p=%.4f)\n", m1$n, m1$r2, m1$p, m1$p_F))
cat(sprintf("  t_y orientation: n=%d r2=%.3f p_perm=%.4f (F-test p=%.4f)\n", m2$n, m2$r2, m2$p, m2$p_F))
cat(sprintf("  t_x abundance:   n=%d r=%+.3f p=%.4f\n", nrow(s3), unname(sp3$estimate), sp3$p.value))
cat(sprintf("  t_y abundance:   n=%d r=%+.3f p=%.4f\n", nrow(s4), unname(sp4$estimate), sp4$p.value))

title1 <- sprintf("n=%d, r²=%.2f, p=%.4f%s", m1$n, m1$r2, m1$p, ifelse(m1$p < 0.05, "*", ""))
title2 <- sprintf("n=%d, r²=%.2f, p=%.4f%s", m2$n, m2$r2, m2$p, ifelse(m2$p < 0.05, "*", ""))
title3 <- sprintf("n=%d, r=%+.2f, p=%.4f%s", nrow(s3), unname(sp3$estimate), sp3$p.value,
                   ifelse(sp3$p.value < 0.05, "*", ""))
title4 <- sprintf("n=%d, r=%+.2f, p=%.4f%s", nrow(s4), unname(sp4$estimate), sp4$p.value,
                   ifelse(sp4$p.value < 0.05, "*", ""))

mkplot <- function(x, y, xlab, title) {
  ggplot(data.frame(x = x, y = y), aes(x, y)) +
    geom_point(size = 1.4, alpha = 0.6, color = "#6fa8d8") +
    labs(x = xlab, y = "log(anisotropy) = log(pdg_max / pdg_min)", title = title) +
    theme_minimal(base_size = 9) +
    theme(plot.title = element_text(size = 8.5, hjust = 0.5))
}

p1 <- mkplot(sub$sok_x_angle_deg, sub$log_aniso, "SOK polarity angle, t_x (deg)", title1)
p2 <- mkplot(sub$sok_y_angle_deg, sub$log_aniso, "SOK polarity angle, t_y (deg)", title2) + ylab(NULL)
p3 <- mkplot(s3$log_signalTotal_x, s3$log_aniso, "log(SOK signalTotal, t_x)", title3) + ylab(NULL)
p4 <- mkplot(s4$log_signalTotal_y, s4$log_aniso, "log(SOK signalTotal, t_y)", title4) + ylab(NULL)

suptitle <- sprintf("%s: growth anisotropy vs SOK polarity angle / abundance ('Exclude from analyses' tissue excluded)",
                     sel_name)
note <- "Orientation panels (1-2): Mardia circular-linear R2 on the axially-doubled SOK angle, permutation test (10,000 shuffles). Abundance panels (3-4): Pearson correlation of log signal vs log anisotropy. Single tracked primordium."

out_path <- sprintf("fig3_anisotropy_vs_polarity_%s_R.pdf", tolower(sel_name))
if (requireNamespace("gridExtra", quietly = TRUE) && requireNamespace("grid", quietly = TRUE)) {
  g <- gridExtra::arrangeGrob(p1, p2, p3, p4, ncol = 4,
                               top = grid::textGrob(suptitle, gp = grid::gpar(fontsize = 11)),
                               bottom = grid::textGrob(note, gp = grid::gpar(fontsize = 7, col = "#444444"),
                                                        x = 0.5, hjust = 0.5))
  ggsave(out_path, g, width = 15, height = 4.6)
} else {
  cat("NOTE: install.packages('gridExtra') to combine panels into one PDF; saving panels separately.\n")
  ggsave(sub("_R.pdf", "_R_panel1.pdf", out_path), p1, width = 4, height = 4.2)
  ggsave(sub("_R.pdf", "_R_panel2.pdf", out_path), p2, width = 4, height = 4.2)
  ggsave(sub("_R.pdf", "_R_panel3.pdf", out_path), p3, width = 4, height = 4.2)
  ggsave(sub("_R.pdf", "_R_panel4.pdf", out_path), p4, width = 4, height = 4.2)
}
cat(sprintf("Saved %s\n", out_path))
