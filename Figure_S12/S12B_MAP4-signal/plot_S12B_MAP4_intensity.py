"""Fig. S12B: mean pCASP1::mVenus:MAP4 signal intensity per primordium, mock vs DEX.

One primordium per root. Two-sided Mann-Whitney U on primordia (= roots).
Run from this folder: python3 plot_S12B_MAP4_intensity.py
"""
import random
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
from scipy.stats import mannwhitneyu

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parents[1] / "stats_common"))
from rootid import root_id  # noqa: E402

df = pd.read_excel(HERE / "measurements_v2_mVenus-MAP4.xlsx")
name = df.iloc[:, 1].astype(str).str.split(":c:").str[0]
df["root"] = df["Treatment"] + "|" + name.map(root_id)
m, d = df[df.Treatment == "Mock"], df[df.Treatment == "DEX"]
p = mannwhitneyu(m.Mean, d.Mean).pvalue
stats = pd.DataFrame([{"n_primordia_mock": len(m), "n_primordia_dex": len(d),
                       "n_roots_mock": m.root.nunique(), "n_roots_dex": d.root.nunique(),
                       "median_mock": m.Mean.median(), "median_dex": d.Mean.median(), "MWU_p": p}])
out = HERE / "outputs"
out.mkdir(exist_ok=True)
stats.to_csv(out / "FigS12B_statistics.csv", index=False)
print(stats.T.to_string(header=False))

rng = random.Random(1)
with plt.rc_context({"font.family": "Helvetica", "svg.fonttype": "none", "pdf.fonttype": 42}):
    fig, ax = plt.subplots(figsize=(2.4, 3.0))
    for x, (sub, label) in enumerate(((m, "Mock"), (d, "DEX"))):
        v = sub.Mean.to_numpy()
        ax.boxplot(v, positions=[x], widths=0.5, showfliers=False, medianprops={"color": "black", "linewidth": 2})
        ax.scatter([x + rng.uniform(-0.12, 0.12) for _ in v], v, s=18, color="#2ca02c", alpha=0.7, zorder=3)
        ax.text(x, -0.14, f"N={len(v)}", transform=ax.get_xaxis_transform(), ha="center", va="top", fontsize=7)
    mark = "****" if p < 1e-4 else "***" if p < 1e-3 else "**" if p < 1e-2 else "*" if p < 0.05 else "ns"
    top = df.Mean.max() * 1.08
    ax.plot([0, 0, 1, 1], [top * 0.97, top, top, top * 0.97], color="black", linewidth=1)
    ax.text(0.5, top, mark, ha="center", va="bottom")
    ax.set_xticks([0, 1])
    ax.set_xticklabels(["Mock", "DEX"])
    ax.set_ylabel("Mean mVenus:MAP4 intensity (a.u.)")
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for suffix in (".png", ".svg", ".pdf"):
        fig.savefig(out / f"FigS12B_MAP4_intensity{suffix}", dpi=300, bbox_inches="tight")
