"""Fig. S14C: cortical microtubule anisotropy in stage II(-like) primordia, Col-0 vs plt3/5/7.

Data: one row per cell ROI (FibrilTool), anisotropy-wt-vs-plt357_st2-central.xlsx.
Statistics: linear mixed model on log(anisotropy) with a random intercept per root
(stats_common/mixed.py); exact permutation test shuffling genotype labels between roots
(root means of log anisotropy) as a check; cell-level Mann-Whitney U as sensitivity.
Run from this folder: python3 plot_S14C_MT_anisotropy.py
"""
import random
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.stats import mannwhitneyu

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "stats_common"))
from mixed import exact_root_permutation, fit_mixed, root_id  # noqa: E402

df = pd.read_excel(HERE / "anisotropy-wt-vs-plt357_st2-central.xlsx")
df = df[df["file name"].notna() & df["anisotropy"].notna()].copy()
df["root"] = df["genotype"] + "|" + df["file name"].map(root_id)
df["mutant"] = (df["genotype"] != "wt").astype(int)
df["log_anisotropy"] = np.log(df["anisotropy"])

model = fit_mixed(df, "log_anisotropy ~ mutant")
root_means = df.groupby(["mutant", "root"]).log_anisotropy.mean().reset_index()
perm_p, n_relabel = exact_root_permutation(root_means[root_means.mutant == 0].log_anisotropy,
                                           root_means[root_means.mutant == 1].log_anisotropy)
cell_p = mannwhitneyu(df[df.mutant == 0].anisotropy, df[df.mutant == 1].anisotropy).pvalue

stats = pd.DataFrame([{
    "comparison": "MT anisotropy, Col-0 vs plt3/5/7",
    "n_cells_col0": int((df.mutant == 0).sum()), "n_cells_plt": int((df.mutant == 1).sum()),
    "n_images_col0": df[df.mutant == 0]["file name"].nunique(), "n_images_plt": df[df.mutant == 1]["file name"].nunique(),
    "n_roots_col0": df[df.mutant == 0].root.nunique(), "n_roots_plt": df[df.mutant == 1].root.nunique(),
    "median_col0": df[df.mutant == 0].anisotropy.median(), "median_plt": df[df.mutant == 1].anisotropy.median(),
    "fold_change_plt_vs_col0_mixed": float(np.exp(model.fe_params["mutant"])),
    "mixed_p": float(model.pvalues["mutant"]),
    "root_permutation_p": perm_p, "root_permutation_n_relabellings": n_relabel,
    "cell_level_MWU_p": float(cell_p),
}])
out = HERE / "outputs"
out.mkdir(exist_ok=True)
stats.to_csv(out / "FigS14C_statistics.csv", index=False)
print(stats.T.to_string(header=False))

rng = random.Random(1)
with plt.rc_context({"font.family": "Helvetica", "svg.fonttype": "none", "pdf.fonttype": 42}):
    fig, ax = plt.subplots(figsize=(2.6, 3.0))
    for x, (m, label, color) in enumerate(((0, "Col-0", "#7c7c7c"), (1, "plt3/5/7", "#c9c9c9"))):
        v = df[df.mutant == m].anisotropy.to_numpy()
        ax.boxplot(v, positions=[x], widths=0.5, patch_artist=True, showfliers=False,
                   boxprops={"facecolor": color, "linewidth": 1.2}, medianprops={"color": "black", "linewidth": 2})
        ax.scatter([x + rng.uniform(-0.15, 0.15) for _ in v], v, s=10, color="black", alpha=0.5, zorder=3)
        ax.text(x, -0.18, f"n={len(v)}\n({df[df.mutant == m].root.nunique()} roots)", transform=ax.get_xaxis_transform(),
                ha="center", va="top", fontsize=7)
    p = stats.mixed_p.iloc[0]
    mark = "****" if p < 1e-4 else "***" if p < 1e-3 else "**" if p < 1e-2 else "*" if p < 0.05 else "ns"
    top = df.anisotropy.max() * 1.08
    ax.plot([0, 0, 1, 1], [top * 0.97, top, top, top * 0.97], color="black", linewidth=1)
    ax.text(0.5, top, mark, ha="center", va="bottom")
    ax.set_xticks([0, 1])
    ax.set_xticklabels(["Col-0", "plt3/5/7"])
    ax.get_xticklabels()[1].set_style("italic")
    ax.set_ylabel("Microtubule anisotropy")
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for suffix in (".png", ".svg", ".pdf"):
        fig.savefig(out / f"FigS14C_MT_anisotropy{suffix}", dpi=300, bbox_inches="tight")
