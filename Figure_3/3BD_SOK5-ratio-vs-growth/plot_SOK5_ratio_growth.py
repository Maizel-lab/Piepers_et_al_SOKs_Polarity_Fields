"""Raw SOK5/PM ratio (left axis) and growth rate (right axis) on imaging time, for the intact pRPS5A::SOK5-YFP
primordium and the ablation primordium of plot_SOK5_vs_ablation.py. Both panels share the ratio and growth-rate scales; styled like the existing S10 panels.

Usage: python3 plot_SOK5_ratio_growth.py
"""
import os
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from sok_data import HERE, WINDOW_H, SOK5C, smooth, load

# same roots and markers as plot_SOK5_vs_ablation.py
ROOTS = [("Analysis_MAX_2023-02-24_18.19.24_sC194-1-1-3_stage2-emergence-track-1_crop.ods", "pRPS5A::SOK5-YFP, intact"),
         ("Analysis_MAX_2023-11-05_stack0_cropped_forDataAnalysis_betterAdjusted.ods", "pRPS5A::SOK5-YFP, ablation")]
STAGES = [(15, "stage II", "--"), (50, "emergence", ":")]  # µm
NO_EMERGENCE = {"pRPS5A::SOK5-YFP, ablation"}  # panels without the emergence marker
XTICK = 10  # h


def time_at(t, L, Lref):
    """First time the root reaches Lref (interpolated); None if it is outside the recording."""
    above = np.flatnonzero(L >= Lref)
    if len(above) == 0 or (above[0] == 0 and L[0] > Lref):
        return None
    i = above[0]
    return t[0] if i == 0 else t[i - 1] + (Lref - L[i - 1]) / (L[i] - L[i - 1]) * (t[i] - t[i - 1])


RATIOC, RATEC = "#4d7ebd", "#c1c1c1"  # blue ratio, light grey growth: as in the existing S10 panels
LINE_SIGMA = 1.0  # h, Gaussian smoothing of the plotted lines (slightly more than SIGMA_H elsewhere); dots = frames
X_MARKS = {}  # file -> times (h) of the frames shown as images, drawn as x on the time axis (as in the S10 panels)
data = {s["file"]: s for s in load(verbose=False)}
roots = [(data[f], title) for f, title in ROOTS]
tmax = max(s["t"].max() for s, _ in roots)
rmax = max(np.nanmax(s["ratio"]) for s, _ in roots) * 1.12
gmax = max(np.nanmax(s["rate"]) for s, _ in roots) * 1.12

plt.rcParams["svg.fonttype"] = "none"
plt.rcParams["font.family"] = "Helvetica"
plt.rcParams["pdf.fonttype"] = 42
fig, axs = plt.subplots(2, 1, figsize=(3.3, 4.2), gridspec_kw=dict(hspace=0.45))
for ax, (s, title) in zip(axs, roots):
    t = s["t"]
    for Lref, name, ls in STAGES:
        if title in NO_EMERGENCE and name == "emergence":
            continue
        ts = time_at(t, s["L"], Lref)
        if ts is not None:
            ax.axvline(ts, color="#bbb", lw=0.6, ls=ls, zorder=0)
            ax.text(ts, 0.0, f" {name}", transform=ax.get_xaxis_transform(), ha="left", va="bottom", fontsize=6,
                    color="#888", rotation=90)
    ax2 = ax.twinx()
    keep = ~np.isnan(s["ratio"])
    ax2.scatter(t, s["rate"], s=7, color=RATEC, alpha=0.8, lw=0)
    ax2.plot(t, smooth(t, s["rate"], LINE_SIGMA), color=RATEC, lw=1.6, label="lateral root growth rate")
    ax.scatter(t[keep], s["ratio"][keep], s=7, color=RATIOC, alpha=0.6, lw=0)
    ax.plot(t, smooth(t, s["ratio"], LINE_SIGMA), color=RATIOC, lw=1.6,
            label="ratio: SOK5:YFP / plasma membrane intensity")
    ax.set_zorder(ax2.get_zorder() + 1)  # ratio drawn above the growth rate
    ax.patch.set_visible(False)
    marks = X_MARKS.get(s["file"], [])
    if marks:
        ax.plot(marks, [0] * len(marks), "x", color="#222", ms=4, mew=1, clip_on=False, zorder=5,
                transform=ax.get_xaxis_transform())
    ax.set_xlim(-1, tmax + 1)  # same time axis in both panels
    ax.set_xticks(np.arange(0, tmax + 1, XTICK))
    ax.set_ylim(0, rmax)
    ax2.set_ylim(0, gmax)
    ax.set_xlabel("Time (h)", fontsize=8, labelpad=1)
    ax.set_ylabel("SOK5:YFP / PM ratio", fontsize=8)
    ax2.set_ylabel("Growth rate (µm/h)", fontsize=8)
    for a in (ax, ax2):
        a.tick_params(labelsize=7, length=2.5, width=0.6, pad=2)
        for sp in a.spines.values():
            sp.set_linewidth(0.6)
    ax.set_title(title, fontsize=7.5, loc="left", pad=3)
# one legend for both panels: inside a panel it would cover data on the shared time axis
from matplotlib.lines import Line2D
fig.legend([Line2D([], [], color=RATIOC, lw=1.6), Line2D([], [], color=RATEC, lw=1.6)],
           ["ratio: SOK5:YFP / plasma membrane intensity", "lateral root growth rate"], loc="lower left",
           bbox_to_anchor=(axs[0].get_position().x0 - 0.01, axs[0].get_position().y1 + 0.045), ncol=1,
           frameon=False, handlelength=2.2, labelcolor="linecolor", prop=dict(weight="bold", size=6.5))
out = os.path.join(HERE, "Figure_SOK5_ratio_vs_growth")
fig.savefig(out + ".png", dpi=300, bbox_inches="tight")
fig.savefig(out + ".pdf", bbox_inches="tight")
fig.savefig(out + ".svg", bbox_inches="tight")
print("saved", out)
