"""Fig. 3H: mean SOK5:YFP signal intensity per primordium, mock vs DEX (pCASP1>>PHS1dP).

One value per primordium. Two DEX primordia were imaged in the same root (sample S1), so
the test is also given on root means. Two-sided Mann-Whitney U.
Run from this folder: python3 plot_3H_SOK5_intensity.py
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

df = pd.read_excel(HERE / "SOK5 PHS1∆P measurements.xlsx")
df["treatment"] = df["Sample Label"].str.contains("control").map({True: "Mock", False: "DEX"})
df["root"] = df["treatment"] + "|" + (df["LIF File Name"] + " - " + df["Sample Label"]).map(root_id)
m, d = df[df.treatment == "Mock"], df[df.treatment == "DEX"]
rm, rd = m.groupby("root").SignalMean.mean(), d.groupby("root").SignalMean.mean()
stats = pd.DataFrame([{
    "n_primordia_mock": len(m), "n_primordia_dex": len(d), "n_roots_mock": len(rm), "n_roots_dex": len(rd),
    "median_mock": m.SignalMean.median(), "median_dex": d.SignalMean.median(),
    "MWU_p_primordia": mannwhitneyu(m.SignalMean, d.SignalMean).pvalue,
    "MWU_p_roots": mannwhitneyu(rm, rd).pvalue,
}])
out = HERE / "outputs"
out.mkdir(exist_ok=True)
stats.to_csv(out / "Fig3H_statistics.csv", index=False)
print(stats.T.to_string(header=False))

rng = random.Random(1)
with plt.rc_context({"font.family": "Helvetica", "svg.fonttype": "none", "pdf.fonttype": 42}):
    fig, ax = plt.subplots(figsize=(2.4, 3.0))
    for x, (sub, label) in enumerate(((m, "Mock"), (d, "DEX"))):
        v = sub.SignalMean.to_numpy()
        ax.boxplot(v, positions=[x], widths=0.5, showfliers=False, medianprops={"color": "black", "linewidth": 2})
        ax.scatter([x + rng.uniform(-0.12, 0.12) for _ in v], v, s=18, color="#0098ff", alpha=0.7, zorder=3)
        ax.text(x, -0.14, f"N={len(v)}", transform=ax.get_xaxis_transform(), ha="center", va="top", fontsize=7)
    ax.set_xticks([0, 1])
    ax.set_xticklabels(["Mock", "DEX"])
    ax.set_ylabel("Mean SOK5:YFP intensity (a.u.)")
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for suffix in (".png", ".svg", ".pdf"):
        fig.savefig(out / f"Fig3H_SOK5_intensity{suffix}", dpi=300, bbox_inches="tight")
