"""gSOK1-YFP, pRPS5A::SOK1/2/5-YFP, pRPS5A::SOK5-YFP ablation and pRPS5A::H2B-mTurq2 roots: aligned SOK/PM ratio + length traces and per-root
growth-rate correlation, all in one figure. Same analysis as gSOK1/plot_growth_stall.py (without PM correction).

Usage: python3 plot_all_SOK.py   (data loading and thresholds: sok_data.py)
"""
import os
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from sok_data import (HERE, WINDOW_H, BG_THRESH, SOK1C, SOK2C, SOK5C, H2BC, LENC, GROUPS, CONTROL, smooth, load,
                      peak_ratios)

NCOL = int(os.environ.get("NCOL", 3))  # panels per row
# constructs aligned by start length on a shared axis; the rest (the long H2B time-lapses) are shown unshifted,
# each on its own time axis
ALIGN = {g[1] for g in GROUPS} - {CONTROL}

data = load()

# time zero = the moment each root reaches ALIGN_LEN µm (all constructs); ALIGN_LEN=0 falls back to the start-length /
# median-rate alignment of gSOK1/plot_aligned.py, with H2B left unaligned
ALIGN_LEN = float(os.environ.get("ALIGN_LEN", 15)) or None
EMERGE_LEN = 50  # µm; second marker line, where the root has emerged (only drawn if the root reaches it)
STAGE = {15: "stage II"}  # developmental stage at the alignment length, for labels
if ALIGN_LEN:
    ALIGN = {g[1] for g in GROUPS}
aligned = [s for s in data if s["group"] in ALIGN]


def edge_rate(t, L, start):
    """Growth rate over the first (start=True) or last WINDOW_H hours of the recording; the window is doubled
    until the root grows (> 0.1 µm/h) in it, or it covers the whole recording."""
    w = WINDOW_H
    while True:
        m = t <= t[0] + w if start else t >= t[-1] - w
        r = np.polyfit(t[m], L[m], 1)[0]
        if r > 0.1 or m.all():
            return r
        w *= 2


def time_at_length(t, L, Lref):
    """First time the root reaches Lref (linear interpolation between frames); outside the measured range,
    extrapolated with the root's growth rate over the nearest WINDOW_H hours (None if it is not growing there).
    Returns (time, extrapolated?)."""
    above = np.flatnonzero(L >= Lref)
    if len(above) == 0:
        r = edge_rate(t, L, start=False)
        return (t[-1] + (Lref - L[-1]) / r if r > 0.1 else None), True
    i = above[0]
    if i == 0:
        if L[0] == Lref:
            return t[0], False
        r = edge_rate(t, L, start=True)
        return (t[0] - (L[0] - Lref) / r if r > 0.1 else None), True
    return t[i - 1] + (Lref - L[i - 1]) / (L[i] - L[i - 1]) * (t[i] - t[i - 1]), False


if ALIGN_LEN:
    # time zero = the moment each root reaches ALIGN_LEN µm
    for s in data:
        s["extrap"] = False
        if s["group"] in ALIGN:
            t0, s["extrap"] = time_at_length(s["t"], s["L"], ALIGN_LEN)
            if t0 is None:
                raise ValueError(f'{s["group"]} {s["date"]}: not growing at the recording edge, cannot extrapolate to {ALIGN_LEN} µm')
            s["x"] = s["t"] - t0
            te, ext = time_at_length(s["t"], s["L"], EMERGE_LEN)
            s["x_emerge"] = None if ext else te - t0
            if s["extrap"]:
                print(f'{s["group"]} {s["date"]}: {s["L"][0]:g}-{s["L"][-1]:g} µm, time at {ALIGN_LEN:g} µm extrapolated '
                      f'({s["t"][0] - t0:+.1f} h before the first frame)')
        else:
            s["x"] = s["t"] - s["t"][0]
    xmin = min(s["x"].min() for s in aligned)
    print(f"alignment: time zero at {ALIGN_LEN:g} µm")
else:
    # x alignment as in gSOK1/plot_aligned.py: shift each root by start length / median growth rate (pooled over aligned roots)
    rate = np.median([s["slope"] for s in aligned])
    Lmin = min(s["L"][0] for s in aligned)
    for s in data:
        s["extrap"] = False
        s["x"] = s["t"] - s["t"][0] + ((s["L"][0] - Lmin) / rate if s["group"] in ALIGN else 0)
    xmin = 0
    print(f"alignment: median growth rate {rate:.2f} µm/h, reference start length {Lmin:g} µm")
xmax = max(s["x"].max() for s in aligned)

plt.rcParams["svg.fonttype"] = "none"
plt.rcParams["font.family"] = "Helvetica"
plt.rcParams["pdf.fonttype"] = 42

# panel grid: each construct starts on a new row and wraps after NCOL panels
# (a thin spacer row between constructs leaves room for the x labels above the next heading)
cells, heights = [], []
labels = [g[1] for g in GROUPS if any(s["group"] == g[1] for s in data)]
for i, lab in enumerate(labels):
    if i:
        heights.append(0.28)
    members = sorted([s for s in data if s["group"] == lab], key=lambda s_: s_["date"])
    row = len(heights)
    for k, s in enumerate(members):
        cells.append((row + k // NCOL, k % NCOL, s, k == 0))
    heights += [1.0] * int(np.ceil(len(members) / NCOL))
nrow = len(heights)
occupied = {(r, c): s["group"] for r, c, s, _ in cells}

def make_figure():
    """Traces (A) and per-root correlation (B)."""
    fig = plt.figure(figsize=(7.2, 1.55 * sum(heights) + 0.28 * len(data) + 1.5))
    outer = fig.add_gridspec(2, 1, height_ratios=[1.55 * sum(heights), 0.28 * len(data) + 0.9], hspace=0.08)
    gA = outer[0].subgridspec(nrow, NCOL, hspace=0.75, wspace=0.12, height_ratios=heights)

    axes, shared = [], None
    for num, (r, c, s, first) in enumerate(cells, 1):
        s["num"] = str(num)
        is_al = s["group"] in ALIGN
        ax = fig.add_subplot(gA[r, c], sharex=shared if is_al else None)
        if is_al and shared is None:
            shared = ax
        axes.append(ax)
        ax.plot(s["x"], smooth(s["t"], s["nlen"]), color=LENC, lw=2)
        ax.plot(s["x"], smooth(s["t"], s["norm"]), color=s["color"], lw=2)
        if is_al:
            ax.set_xlim(xmin - 0.02 * (xmax - xmin), xmax + 0.02 * (xmax - xmin))
            if ALIGN_LEN:
                ax.axvline(0, color="#999", lw=0.8, ls="--", zorder=0)
                if s["x_emerge"] is not None:
                    ax.axvline(s["x_emerge"], color="#555", lw=0.9, ls=":", zorder=0)
        else:
            ax.set_xlim(0, s["x"].max() * 1.02)
        ax.set_ylim(-0.05, 1.08)
        ax.set_yticks([0, 1])
        ax.text(1.0, 1.02, s["num"], transform=ax.transAxes, ha="right", va="bottom", fontsize=8, color="#555")
        notes = ([] if s["has_pm"] else ["no PM: SOK only"]) + (["t0 extrap."] if s["extrap"] else [])
        if s["thr"] != BG_THRESH:
            notes.append(f"threshold {s['thr']}")
        if np.isnan(s["rho"]):
            notes.append(f"< {s['thr']}: no ratio")
        if len(notes) > 1:
            notes = [n.replace("no PM: SOK only", "no PM").replace("threshold", "thr.") for n in notes]
        note = f"  ({'; '.join(notes)})" if notes else ""
        ax.text(0.0, 1.02, s["date"] + note, transform=ax.transAxes,
                ha="left", va="bottom", fontsize=7, color="#555")
        for sp in ("top", "right"):
            ax.spines[sp].set_visible(False)
        ax.tick_params(labelsize=8, labelleft=(c == 0))
        below = occupied.get((r + 1, c))
        if is_al and below == s["group"]:
            ax.tick_params(labelbottom=False)
        elif below != s["group"]:
            ax.set_xlabel((f"Time from {STAGE.get(ALIGN_LEN, f'{ALIGN_LEN:g} µm')} (h)" if ALIGN_LEN else "Aligned time (h)")
                          if is_al else "Time (h)",
                          fontsize=9)
        if first:
            ax.text(0.0, 1.3, s["group"], transform=ax.transAxes, ha="left", va="bottom", fontsize=9, weight="bold")

    a_top = max(a.get_position().y1 for a in axes)
    a_bot = min(a.get_position().y0 for a in axes)
    a_left = min(a.get_position().x0 for a in axes)
    a_right = max(a.get_position().x1 for a in axes)
    handles = [Line2D([], [], color=c_, lw=2) for c_ in (SOK1C, SOK2C, SOK5C, H2BC, LENC)]
    names = ["SOK1/PM", "SOK2/PM", "SOK5/PM", "H2B/PM", "Length"]
    if ALIGN_LEN:
        handles += [Line2D([], [], color="#999", lw=0.8, ls="--"), Line2D([], [], color="#555", lw=0.9, ls=":")]
        names += [f"{ALIGN_LEN:g} µm" + (f", {STAGE[ALIGN_LEN]}" if ALIGN_LEN in STAGE else "") + " (t = 0)",
                  f"{EMERGE_LEN:g} µm (emergence)"]
    fig.legend(handles, names, loc="lower right", fontsize=8, frameon=False, ncol=5 if not ALIGN_LEN else 4,
               bbox_to_anchor=(a_right, a_top + 0.02), handlelength=1.5, columnspacing=1.2)
    fig.text(a_left - 0.06, 0.5 * (a_top + a_bot), "Normalised values", rotation=90, va="center", ha="center", fontsize=9)

    # B: per-root Spearman correlation between growth rate and ratio, grouped by construct
    axb = fig.add_subplot(outer[1])
    ys, ticks, y = [], [], 0
    for lab in labels:
        for s in sorted([s for s in data if s["group"] == lab], key=lambda s_: -np.nan_to_num(s_["rho"], nan=-9)):
            ys.append((y, s)); ticks.append(f'{s["num"]}  {s["short"]}'); y += 1
        y += 0.4
    yv = [y_ for y_, _ in ys]
    axb.barh(yv, [s["rho"] for _, s in ys], height=0.6, color=[s["color"] for _, s in ys])
    axb.set_yticks(yv, ticks, fontsize=8)
    axb.set_ylim(y - 0.8, -0.6)
    axb.axvline(0, color="#555", lw=0.8)
    axb.set_xlim(-1.25, 1.25)
    for y_, s in ys:
        v = s["rho"]
        if np.isnan(v):
            axb.text(0.03, y_, "n/a (no ratio)", va="center", ha="left", fontsize=8, color="#888")
            continue
        axb.text(v + (0.03 if v >= 0 else -0.03), y_, f'{v:+.2f}', va="center", ha="left" if v >= 0 else "right",
                 fontsize=8, color="#333")
    for sp in ("top", "right"):
        axb.spines[sp].set_visible(False)
    axb.tick_params(labelsize=8)
    axb.set_xlabel(f"Spearman ρ, growth rate ({WINDOW_H:g} h window) vs normalised reporter/PM ratio, per root", fontsize=9)

    LETTER_X = a_left - 0.11
    fig.text(LETTER_X, a_top + 0.04, "A", fontsize=13, weight="bold", va="top", ha="left")
    fig.text(LETTER_X, axb.get_position().y1 + 0.015, "B", fontsize=13, weight="bold", va="top", ha="left")
    return fig


def make_supplement():
    """Traces only, laid out for a one-page supplementary figure: 4 columns, short constructs share a row, no dates,
    root numbers or panel B; per-root caveats as symbols explained in a footnote."""
    ncol = 4
    place, r, c = [], 0, 0  # (row, col, root, heading?)
    for lab in labels:
        members = sorted([s for s in data if s["group"] == lab], key=lambda s_: s_["date"])
        if c and len(members) > ncol - c:
            r, c = r + 1, 0
        for k, s in enumerate(members):
            if c == ncol:
                r, c = r + 1, 0
            place.append((r, c, s, k == 0 or c == 0))  # heading above the first panel of a construct in each row
            c += 1
    nr = r + 1
    taken = {(r_, c_) for r_, c_, _, _ in place}
    fig = plt.figure(figsize=(7.2, 1.3 * nr + 0.9))
    xlab = f"Time from {STAGE.get(ALIGN_LEN, f'{ALIGN_LEN:g} µm')} (h)" if ALIGN_LEN else "Aligned time (h)"
    gs = fig.add_gridspec(nr, ncol, hspace=0.62, wspace=0.1)
    marks = {"extrap": "*", "no_pm": "†", "thr": "‡"}
    used, axes, shared = set(), [], None
    for r_, c_, s, head in place:
        is_al = s["group"] in ALIGN
        ax = fig.add_subplot(gs[r_, c_], sharex=shared if is_al else None, sharey=axes[0] if axes else None)
        if is_al and shared is None:
            shared = ax
        axes.append(ax)
        if is_al and ALIGN_LEN:
            ax.axvline(0, color="#999", lw=0.6, ls="--", zorder=0)
            if s["x_emerge"] is not None:
                ax.axvline(s["x_emerge"], color="#555", lw=0.7, ls=":", zorder=0)
        ax.plot(s["x"], smooth(s["t"], s["nlen"]), color=LENC, lw=1.4)
        ax.plot(s["x"], smooth(s["t"], s["norm"]), color=s["color"], lw=1.4)
        if is_al:
            ax.set_xlim(xmin - 0.02 * (xmax - xmin), xmax + 0.02 * (xmax - xmin))
        else:
            ax.set_xlim(0, s["x"].max() * 1.02)
        ax.set_ylim(-0.05, 1.08)
        ax.set_yticks([0, 1])
        sym = (marks["extrap"] if s["extrap"] else "") + ("" if s["has_pm"] else marks["no_pm"]) \
            + (marks["thr"] if s["thr"] != BG_THRESH else "")
        used.update(sym)
        if sym:
            ax.text(1.0, 1.06, sym, transform=ax.transAxes, ha="right", va="bottom", fontsize=9, color="#444")
        if head:
            ax.text(0.0, 1.06, s["group"].replace(", ", ",\n"), transform=ax.transAxes, ha="left", va="bottom",
                    fontsize=7.5, weight="bold", linespacing=1.1)  # long headings wrap so they clear the marker
        for sp in ("top", "right"):
            ax.spines[sp].set_visible(False)
        ax.tick_params(labelsize=7, length=2.5, pad=2, labelleft=(c_ == 0))
        if (r_ + 1, c_) in taken and is_al:
            ax.tick_params(labelbottom=False)
        else:
            ax.set_xlabel(xlab, fontsize=7, labelpad=2)
    a_top = max(a.get_position().y1 for a in axes)
    a_bot = min(a.get_position().y0 for a in axes)
    a_left = min(a.get_position().x0 for a in axes)
    a_right = max(a.get_position().x1 for a in axes)
    handles = [Line2D([], [], color=c_, lw=1.6) for c_ in (SOK1C, SOK2C, SOK5C, H2BC, LENC)]
    names = ["SOK1/PM", "SOK2/PM", "SOK5/PM", "H2B/PM", "Length"]
    if ALIGN_LEN:
        handles += [Line2D([], [], color="#999", lw=0.8, ls="--"), Line2D([], [], color="#555", lw=0.9, ls=":")]
        names += [f"{ALIGN_LEN:g} µm" + (f", {STAGE[ALIGN_LEN]}" if ALIGN_LEN in STAGE else ""),
                  f"{EMERGE_LEN:g} µm, emergence"]
    fig.legend(handles, names, loc="lower center", fontsize=7, frameon=False, ncol=len(names),
               bbox_to_anchor=(0.5 * (a_left + a_right), a_top + 0.022), handlelength=1.6, columnspacing=1.1,
               handletextpad=0.4)
    fig.text(a_left - 0.045, 0.5 * (a_top + a_bot), "Normalised value", rotation=90, va="center", ha="center",
             fontsize=8)
    fig.text(a_left - 0.075, a_top + 0.045, "A", fontsize=12, weight="bold", va="top", ha="left")

    # B: peak raw reporter/PM ratio per root, in the free cells next to the last row
    free = ncol - (max(c_ for r_, c_, _, _ in place if r_ == nr - 1) + 1)
    if free >= 2:
        pk = peak_ratios(data)
        axp = fig.add_subplot(gs[nr - 1, ncol - free:])
        p_ = axp.get_position()
        axp.set_position([p_.x0 + 0.07, p_.y0, p_.width - 0.07, p_.height * 0.72])  # clear the x labels above
        cons = [g[1] for g in GROUPS if (pk.construct == g[1]).any()]
        short = {g[1]: g[2].replace("pRPS5A::", "").replace(", ablation", "\nabl.") for g in GROUPS}
        rng = np.random.default_rng(1)
        axp.axhline(1, color="#999", lw=0.6, ls="--", zorder=0)
        for i, lab in enumerate(cons):
            sub = pk[pk.construct == lab]
            axp.scatter(i + rng.uniform(-0.12, 0.12, len(sub)), sub.peak, s=12, color=sub.color.iloc[0], zorder=3,
                        edgecolor="white", lw=0.3)
            axp.plot([i - 0.28, i + 0.28], [sub.peak.median()] * 2, color="#222", lw=1.2, zorder=4)
            axp.text(i, 1.0, f"n={len(sub)}", transform=axp.get_xaxis_transform(), ha="center", va="bottom",
                     fontsize=6, color="#666")
        if CONTROL in cons:
            axp.axvline(cons.index(CONTROL) - 0.5, color="#ccc", lw=0.6, ls="--")
        axp.set_xticks(range(len(cons)), [short[l] for l in cons], fontsize=6.5)
        pr = [i for i, l in enumerate(cons) if l.startswith("pRPS5A")]
        if pr:
            tr = axp.get_xaxis_transform()
            axp.plot([pr[0] - 0.35, pr[-1] + 0.35], [-0.47, -0.47], color="#555", lw=0.6, transform=tr, clip_on=False)
            axp.text(0.5 * (pr[0] + pr[-1]), -0.5, "pRPS5A::", transform=tr, ha="center", va="top", fontsize=6.5)
        axp.set_xlim(-0.6, len(cons) - 0.4)
        axp.set_ylim(0, pk.peak.max() * 1.15)
        axp.set_yticks(range(int(pk.peak.max() * 1.15) + 1))
        axp.set_ylabel("Peak SOK/PM\nratio (raw)", fontsize=7, labelpad=2)
        axp.tick_params(labelsize=6.5, length=2.5, pad=2)
        for sp in ("top", "right"):
            axp.spines[sp].set_visible(False)
        fig.text(axp.get_position().x0 - 0.075, axp.get_position().y1 + 0.035, "B", fontsize=12, weight="bold",
                 va="top", ha="left")
    foot = {"*": f"* {STAGE.get(ALIGN_LEN, 'reference length')} time extrapolated (root already longer when imaging started)",
            "†": "† no PM channel: SOK intensity only",
            "‡": "‡ background threshold 100 instead of 110"}
    fig.text(a_left, a_bot - 0.07, "     ".join(foot[k] for k in "*†‡" if k in used), ha="left", va="top",
             fontsize=6.5, color="#444")
    return fig


suffix = ((f"_{WINDOW_H:g}h" if WINDOW_H != 5.0 else "") + (f"_L{ALIGN_LEN:g}" if ALIGN_LEN not in (15, None) else "")
          + ("_startlength" if not ALIGN_LEN else ""))
for name, make in (("", make_figure), ("_traces", make_supplement)):
    fig = make()
    out = os.path.join(HERE, "Figure_all_SOK_growth_vs_ratio" + suffix + name)
    fig.savefig(out + ".png", dpi=300, bbox_inches="tight")
    fig.savefig(out + ".pdf", bbox_inches="tight")
    fig.savefig(out + ".svg", bbox_inches="tight")
    plt.close(fig)
    print("saved", out)
