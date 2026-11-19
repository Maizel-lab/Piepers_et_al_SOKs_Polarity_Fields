"""Detailed comparison of one intact pRPS5A::SOK5-YFP primordium and one ablation primordium, in raw units on
imaging time (no alignment, one shared time axis): length, growth rate, SOK and PM intensities, and the SOK/PM ratio. Each row shares its
y-axis between the two roots so absolute levels can be compared. Faint dots are frames, lines are Gaussian-smoothed
(SIGMA_H); the ratio stops where the background threshold cuts it (as in the other figures).

Usage: python3 plot_SOK5_vs_ablation.py
"""
import os
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from sok_data import HERE, WINDOW_H, BG_THRESH, SOK5C, smooth, load

ROOTS = [("Analysis_MAX_2023-02-24_18.19.24_sC194-1-1-3_stage2-emergence-track-1_crop.ods", "pRPS5A::SOK5-YFP, intact"),
         ("Analysis_MAX_2023-11-05_stack0_cropped_forDataAnalysis_betterAdjusted.ods", "pRPS5A::SOK5-YFP, ablation")]
STAGES = [(15, "stage II", "--"), (50, "emergence", ":")]  # µm
NO_EMERGENCE = {"pRPS5A::SOK5-YFP, ablation"}  # panels without the emergence marker
PMC, LC = "#7a7a7a", "#222222"
XTICK = 10  # h

data = {s["file"]: s for s in load(verbose=False)}
roots = [(data[f], title) for f, title in ROOTS]


def time_at(t, L, Lref):
    """First time the root reaches Lref (interpolated); None if it is outside the recording."""
    above = np.flatnonzero(L >= Lref)
    if len(above) == 0 or (above[0] == 0 and L[0] > Lref):
        return None
    i = above[0]
    return t[0] if i == 0 else t[i - 1] + (Lref - L[i - 1]) / (L[i] - L[i - 1]) * (t[i] - t[i - 1])


plt.rcParams["svg.fonttype"] = "none"
plt.rcParams["font.family"] = "Helvetica"
plt.rcParams["pdf.fonttype"] = 42
fig, axs = plt.subplots(4, 2, figsize=(7.2, 6.4), sharex=True, sharey="row",
                        gridspec_kw=dict(hspace=0.22, wspace=0.1))
tmax = max(s["t"].max() for s, _ in roots)
for col, (s, title) in enumerate(roots):
    t = s["t"]
    keep = ~np.isnan(s["ratio"])
    rows = [(s["L"], "Length (µm)", LC, None),
            (s["rate"], f"Growth rate\n(µm/h, {WINDOW_H:g} h window)", LC, None),
            (None, "Intensity\n(grey values)", None, None),
            (s["ratio"], "SOK/PM ratio\n(raw)", SOK5C, None)]
    stages = [st for st in STAGES if not (title in NO_EMERGENCE and st[1] == "emergence")]
    for row, (y, ylab, color, _) in enumerate(rows):
        ax = axs[row, col]
        for Lref, name, ls in stages:
            ts = time_at(t, s["L"], Lref)
            if ts is not None:
                ax.axvline(ts, color="#999", lw=0.7, ls=ls, zorder=0)
                if row == 0:
                    ax.text(ts, 1.0, f" {name}", transform=ax.get_xaxis_transform(), ha="left", va="top",
                            fontsize=6.5, color="#666")
        if row == 0:
            ax.plot(t, y, color=color, lw=1.4)
            for Lref, _, ls in stages:
                ax.axhline(Lref, color="#ccc", lw=0.6, ls=ls, zorder=0)
        elif row == 1:
            ax.axhline(0, color="#ccc", lw=0.6, zorder=0)
            ax.plot(t, y, color=color, lw=1.4)
        elif row == 2:
            for v, c, lab in ((s["sok"], SOK5C, "SOK5-YFP"), (s["pm"], PMC, "PM marker")):
                ax.scatter(t, v, s=4, color=c, alpha=0.35, lw=0)
                ax.plot(t, smooth(t, v), color=c, lw=1.4, label=lab)
            ax.axhline(BG_THRESH, color="#999", lw=0.6, ls="-.", zorder=0, label=f"background ({BG_THRESH})")
            if col == 0:
                ax.legend(fontsize=6.5, frameon=False, loc="upper right", handlelength=1.5)
        else:
            ax.axhline(1, color="#ccc", lw=0.6, ls="--", zorder=0)
            ax.scatter(t[keep], y[keep], s=4, color=color, alpha=0.35, lw=0)
            ax.plot(t, smooth(t, y), color=color, lw=1.4)
            sm = smooth(t, y)
            ax.text(0.99, 0.04, f"peak {np.nanmax(sm):.2f}   min {np.nanmin(sm):.2f}", transform=ax.transAxes,
                    ha="right", va="bottom", fontsize=6.5, color="#444")
        if col == 0:
            ax.set_ylabel(ylab, fontsize=7.5)
        ax.tick_params(labelsize=7, length=2.5, pad=2)
        for sp in ("top", "right"):
            ax.spines[sp].set_visible(False)
    axs[0, col].set_title(title + f"\n{s['L'][0]:.0f}–{s['L'].max():.0f} µm imaged", fontsize=8, loc="left")
    axs[-1, col].set_xlabel("Time (h)", fontsize=8)
    axs[-1, col].set_xticks(np.arange(0, tmax + XTICK, XTICK))
    axs[-1, col].set_xlim(0, tmax + 1)
axs[2, 0].set_ylim(bottom=90)
axs[3, 0].set_ylim(bottom=0)
out = os.path.join(HERE, "Figure_SOK5_vs_ablation_detail")
fig.savefig(out + ".png", dpi=300, bbox_inches="tight")
fig.savefig(out + ".pdf", bbox_inches="tight")
fig.savefig(out + ".svg", bbox_inches="tight")
print("saved", out)
