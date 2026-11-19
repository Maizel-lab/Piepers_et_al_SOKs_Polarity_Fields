"""Acute polarity angle (relative to horizontal) in provascular vs ground tissue
precursor cells, control vs mutant/treatment, for five comparisons.

Tissue assignment
  - Files with a ``Cell`` column: TL + BL -> provascular, TR + BR -> ground tissue precursor.
  - wtst4 / scr files only carry a ``Tissue`` column: Vascular -> provascular,
    Ground -> ground tissue precursor.

Angle: atan2(yMax, xMax) folded onto 0-90 deg (shortest distance to the
horizontal 0/180 deg line), as in the earlier "polarity csv corrected" scripts.

Statistics: two-sided Mann-Whitney U per tissue, control vs mutant; Holm
correction within each comparison (2 tests) and, additionally, across all 10.
A sensitivity analysis repeats the test on per-sample (per-CSV) medians.
"""

import hashlib
import math
import random
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.transforms import offset_copy
import pandas as pd
from scipy.stats import mannwhitneyu


DATA_DIR = Path(__file__).resolve().parent
OUTPUT_DIR = DATA_DIR / "boxplots"
TISSUES = ("Provascular", "Ground tissue precursor")
CELL_TO_TISSUE = {
    "TL": "Provascular",
    "BL": "Provascular",
    "TR": "Ground tissue precursor",
    "BR": "Ground tissue precursor",
}
TISSUE_COLUMN_TO_TISSUE = {
    "vascular": "Provascular",
    "ground": "Ground tissue precursor",
}
# Box fill (control, mutant) and point fill (control, mutant); point colours for
# red and orange are taken from the Illustrator templates, blue is derived the same way.
RED = (("#EC1E24", "#FF9797"), ("#A20D18", "#C7353E"))
ORANGE = (("#F6AE2F", "#FFD899"), ("#A26C29", "#C48C2B"))
BLUE = (("#38AEE3", "#9FE6FF"), ("#22769C", "#2F9FC6"))
PLT = r"$\it{plt3/5/7}$"
# (title, control label, control folder, mutant label, mutant folder, colors)
COMPARISONS = (
    ("SOK1", "Col-0", "wtst2 SOK1", PLT, "plt SOK1", RED),
    ("SOK2", "Col-0", "wtst2 SOK2", PLT, "plt SOK2", ORANGE),
    ("SOK5", "Col-0", "wtst2 SOK5", PLT, "plt SOK5", BLUE),
    ("PHS1dP", "mock", "phs1dp mock", "DEX", "phs1dp dex", BLUE),
    ("SCR", "Col-0", "wtst4", r"$\it{scr}$", "scr", RED),
)
# Keep only the comparisons whose control and mutant folders are present in this
# repository (the SOK1 and SOK5 plt3/5/7 data sets are not part of the deposited data).
COMPARISONS = tuple(
    c for c in COMPARISONS if (DATA_DIR / c[2]).is_dir() and (DATA_DIR / c[4]).is_dir()
)
# Comparisons that also get a ground-tissue-precursor-only plot.
GROUND_ONLY = ("SOK2", "SCR")
YLABEL = "Acute polarity angle (°)"


def acute_angle(x: float, y: float) -> float:
    axial = math.degrees(math.atan2(y, x)) % 180
    return min(axial, 180 - axial)


def load_folder(folder: Path) -> pd.DataFrame:
    """Read all CSVs below ``folder`` (recursively), skipping byte-identical duplicates."""
    rows = []
    seen_hashes: dict[str, Path] = {}
    # Deepest paths first, so copies inside aberrant/ or wt-like/ win over top-level duplicates.
    for csv_path in sorted(folder.rglob("*.csv"), key=lambda p: (-len(p.parts), str(p))):
        if csv_path.name.startswith("._"):
            continue
        digest = hashlib.sha256(csv_path.read_bytes()).hexdigest()
        if digest in seen_hashes:
            print(f"  skip duplicate: {csv_path.relative_to(DATA_DIR)}")
            continue
        seen_hashes[digest] = csv_path
        df = pd.read_csv(csv_path, encoding="utf-8-sig")
        df.columns = [c.strip() for c in df.columns]
        subgroup = csv_path.parent.name if csv_path.parent != folder else ""
        for _, row in df.iterrows():
            cell = str(row.get("Cell", "")).strip() if pd.notna(row.get("Cell")) else ""
            if "Cell" in df.columns:
                if not cell:
                    continue
                tissue = CELL_TO_TISSUE[cell]
            else:
                raw = row.get("Tissue")
                if pd.isna(raw) or str(raw).strip().lower() not in TISSUE_COLUMN_TO_TISSUE:
                    continue
                tissue = TISSUE_COLUMN_TO_TISSUE[str(raw).strip().lower()]
                cell = str(raw).strip()
            rows.append(
                {
                    "sample": str(csv_path.relative_to(DATA_DIR)),
                    "subgroup": subgroup,
                    "label": row["Label"],
                    "cell": cell,
                    "tissue": tissue,
                    "xMax": row["xMax"],
                    "yMax": row["yMax"],
                    "acute_angle_deg": acute_angle(float(row["xMax"]), float(row["yMax"])),
                }
            )
    return pd.DataFrame(rows)


def holm(p_values: list[float]) -> list[float]:
    order = sorted(range(len(p_values)), key=lambda i: p_values[i])
    adjusted = [0.0] * len(p_values)
    running = 0.0
    for rank, i in enumerate(order):
        running = max(running, min(1.0, (len(p_values) - rank) * p_values[i]))
        adjusted[i] = running
    return adjusted


def stars(p: float) -> str:
    for threshold, label in ((0.0001, "****"), (0.001, "***"), (0.01, "**"), (0.05, "*")):
        if p <= threshold:
            return label
    return "ns"


def mwu(a, b) -> tuple[float, float]:
    if len(a) == 0 or len(b) == 0:
        return float("nan"), float("nan")
    result = mannwhitneyu(a, b, alternative="two-sided")
    return float(result.statistic), float(result.pvalue)


def main() -> None:
    OUTPUT_DIR.mkdir(exist_ok=True)
    all_cells = []
    stats_rows = []
    for title, ctrl_label, ctrl_dir, mut_label, mut_dir, _ in COMPARISONS:
        print(title)
        for role, label, folder in (("control", ctrl_label, ctrl_dir), ("mutant", mut_label, mut_dir)):
            df = load_folder(DATA_DIR / folder)
            df.insert(0, "comparison", title)
            df.insert(1, "role", role)
            df.insert(2, "condition", folder)
            all_cells.append(df)
    cells = pd.concat(all_cells, ignore_index=True)
    cells.to_csv(OUTPUT_DIR / "acute_angles_per_cell.csv", index=False)

    for title, *_ in COMPARISONS:
        sub = cells[cells.comparison == title]
        for tissue in TISSUES:
            t = sub[sub.tissue == tissue]
            ctrl = t[t.role == "control"]
            mut = t[t.role == "mutant"]
            u, p = mwu(ctrl.acute_angle_deg, mut.acute_angle_deg)
            ctrl_s = ctrl.groupby("sample").acute_angle_deg.median()
            mut_s = mut.groupby("sample").acute_angle_deg.median()
            u_s, p_s = mwu(ctrl_s, mut_s)
            stats_rows.append(
                {
                    "comparison": title,
                    "tissue": tissue,
                    "control": ctrl.condition.iloc[0] if len(ctrl) else "",
                    "mutant": mut.condition.iloc[0] if len(mut) else "",
                    "control_n_cells": len(ctrl),
                    "control_n_samples": ctrl["sample"].nunique(),
                    "control_median_deg": ctrl.acute_angle_deg.median(),
                    "control_IQR_deg": ctrl.acute_angle_deg.quantile(0.75) - ctrl.acute_angle_deg.quantile(0.25),
                    "mutant_n_cells": len(mut),
                    "mutant_n_samples": mut["sample"].nunique(),
                    "mutant_median_deg": mut.acute_angle_deg.median(),
                    "mutant_IQR_deg": mut.acute_angle_deg.quantile(0.75) - mut.acute_angle_deg.quantile(0.25),
                    "test": "Mann-Whitney U, two-sided (cells)",
                    "U": u,
                    "p": p,
                    "per_sample_U": u_s,
                    "per_sample_p": p_s,
                }
            )
    stats = pd.DataFrame(stats_rows)
    stats["p_Holm_within_comparison"] = (
        stats.groupby("comparison", sort=False)["p"].transform(lambda s: holm(list(s)))
    )
    stats["p_Holm_all_10"] = holm(list(stats.p))
    stats["per_sample_p_Holm_within_comparison"] = (
        stats.groupby("comparison", sort=False)["per_sample_p"].transform(lambda s: holm(list(s)))
    )
    stats["significance"] = stats.p_Holm_within_comparison.map(stars)
    stats.to_csv(OUTPUT_DIR / "acute_angle_statistics.csv", index=False)
    with pd.option_context("display.width", 250, "display.max_columns", 30):
        print(stats[["comparison", "tissue", "control_n_cells", "control_median_deg",
                     "mutant_n_cells", "mutant_median_deg", "U", "p",
                     "p_Holm_within_comparison", "p_Holm_all_10", "per_sample_p",
                     "significance"]].round(4).to_string(index=False))

    draw(cells, stats)


# Geometry and stroke/font sizes measured from the Illustrator templates
# (this-style.svg: 4.42 svg units per pt; this-style-ground-only.svg: 6.06 per pt).
# Axes size is fixed in inches so every exported plot has identical proportions.
FULL_STYLE = {
    "figsize": (6.36, 6.36),
    "axes_in": (1.46, 1.46, 4.31, 3.83),  # left, bottom, width, height
    "centers": (0.1624, 0.3413, 0.6164, 0.7971),  # axes fraction
    "box_width": 0.131,
    "jitter": 0.055,
    "tick_size": 18.5, "xlabel_size": 21.7, "n_size": 14, "ylabel_size": 18.5,
    "tissue_size": 18.5, "star_size": 50.7, "ns_size": 18.5,
    "box_lw": 1.8, "whisker_lw": 1.8, "median_lw": 5.4, "bracket_lw": 2.0,
    "bracket": (0.97, 1.0),  # leg bottom, bar (axes fraction)
    "n_y": -0.245, "n_y_long": -0.36,
}
GROUND_STYLE = {
    "figsize": (3.56, 3.08),
    "axes_in": (0.606, 0.638, 2.87, 2.21),
    "centers": (0.262, 0.757),
    "box_width": 0.242,
    "jitter": 0.085,
    "tick_size": 10.8, "xlabel_size": 12.6, "n_size": 8.1, "ylabel_size": 12.1,
    "tissue_size": None, "star_size": 29.4, "ns_size": 10.8,
    "box_lw": 1.6, "whisker_lw": 1.8, "median_lw": 3.9, "bracket_lw": 2.0,
    "bracket": (1.03, 1.06),
    "n_y": -0.255, "n_y_long": -0.47,
}


def draw_styled(cells, stats, comparison, tissues, style, out_path, *,
                value="acute_angle_deg", ylim=(0, 95), yticks=range(0, 91, 15),
                yticklabels=None, ylabel=YLABEL) -> None:
    title, ctrl_label, _, mut_label, _, (box_colors, point_colors) = comparison
    rng = random.Random(f"{title}:{','.join(tissues)}")
    sub = cells[cells.comparison == title]
    with plt.rc_context({"font.family": "Helvetica", "svg.fonttype": "none",
                         "pdf.fonttype": 42}):
        fig = plt.figure(figsize=style["figsize"])
        w, h = style["figsize"]
        left, bottom, aw, ah = style["axes_in"]
        ax = fig.add_axes((left / w, bottom / h, aw / w, ah / h))
        trans = ax.get_xaxis_transform()  # x in data (0-1), y in axes fraction
        centers = iter(style["centers"])
        xticks, xlabels = [], []
        # Long rotated labels (plt3/5/7) reach further down, so the n labels move below them.
        n_y = style["n_y_long"] if PLT in (ctrl_label, mut_label) else style["n_y"]
        for tissue in tissues:
            pair = (next(centers), next(centers))
            for x, role, box_c, pt_c, label in zip(
                pair, ("control", "mutant"), box_colors, point_colors, (ctrl_label, mut_label)
            ):
                values = sub[(sub.tissue == tissue) & (sub.role == role)][value].tolist()
                box = ax.boxplot(
                    values, positions=[x], widths=style["box_width"], patch_artist=True,
                    showfliers=False, manage_ticks=False, zorder=2,
                    boxprops={"edgecolor": "black", "linewidth": style["box_lw"]},
                    medianprops={"color": "black", "linewidth": style["median_lw"],
                                 "solid_capstyle": "butt"},
                    whiskerprops={"color": "black", "linewidth": style["whisker_lw"]},
                    capprops={"linewidth": 0},
                )
                box["boxes"][0].set_facecolor(box_c)
                half = style["jitter"]
                ax.scatter([x + rng.uniform(-half, half) for _ in values], values,
                           s=65, color=pt_c, alpha=0.37, edgecolors="none", zorder=4)
                ax.text(x, n_y, f"n={len(values)}", transform=trans,
                        ha="center", va="baseline", fontsize=style["n_size"])
                xticks.append(x)
                xlabels.append(label)

            row = stats[(stats.comparison == title) & (stats.tissue == tissue)].iloc[0]
            leg, bar = style["bracket"]
            significant = row.significance != "ns"
            size = style["star_size"] if significant else style["ns_size"]
            # The bar is broken around the label (the templates mask it with a
            # white box); the gap and baseline offset are in em of the label font.
            gap = (0.55 if significant else 1.35) * size / (aw * 72) / 2
            mid = sum(pair) / 2
            for xs in ((pair[0], pair[0], mid - gap), (mid + gap, pair[1], pair[1])):
                ys = (leg, bar, bar) if xs[0] == pair[0] else (bar, bar, leg)
                ax.plot(xs, ys, transform=trans, color="black",
                        linewidth=style["bracket_lw"], solid_capstyle="butt",
                        solid_joinstyle="miter", clip_on=False, zorder=5)
            baseline = offset_copy(trans, fig=fig, y=-(0.57 if significant else 0.27) * size,
                                   units="points")
            ax.text(mid, bar, row.significance, transform=baseline, ha="center",
                    va="baseline", fontsize=size, clip_on=False, zorder=6)
            if style["tissue_size"]:
                ax.text(sum(pair) / 2, bar + 0.07, tissue.replace(" precursor", "\nprecursor"),
                        transform=trans, ha="center", va="bottom",
                        fontsize=style["tissue_size"], linespacing=1.0)

        ax.set_xlim(0, 1)
        ax.set_xticks(xticks)
        ax.set_xticklabels(xlabels, rotation=45, ha="right", va="top",
                           rotation_mode="anchor", fontsize=style["xlabel_size"])
        ax.set_ylim(*ylim)
        ax.set_yticks(list(yticks))
        if yticklabels is not None:
            ax.set_yticklabels(yticklabels)
        ax.tick_params(axis="both", width=1.8, length=3.5, labelsize=style["tick_size"])
        ax.tick_params(axis="x", labelsize=style["xlabel_size"])
        ax.set_ylabel(ylabel, fontsize=style["ylabel_size"])
        ax.grid(axis="y", color="#b0b1b1", alpha=0.35, linewidth=1)
        ax.set_axisbelow(True)
        for side in ("top", "right"):
            ax.spines[side].set_visible(False)
        for side in ("left", "bottom"):
            ax.spines[side].set_linewidth(1.8)

        for suffix in (".png", ".svg", ".pdf"):
            fig.savefig(out_path.with_suffix(suffix), dpi=300, bbox_inches="tight")
        plt.close(fig)
        print(out_path.with_suffix(".svg"))


def draw(cells: pd.DataFrame, stats: pd.DataFrame) -> None:
    for comparison in COMPARISONS:
        name = comparison[0]
        draw_styled(cells, stats, comparison, TISSUES, FULL_STYLE,
                    OUTPUT_DIR / f"acute_angle_{name}")
        if name in GROUND_ONLY:
            draw_styled(cells, stats, comparison, ("Ground tissue precursor",),
                        GROUND_STYLE, OUTPUT_DIR / f"acute_angle_{name}_ground_tissue_precursor")


if __name__ == "__main__":
    main()
