"""Shared data loading for plot_all_SOK.py and stats_SOK_growth.py: one dict per root with time, length,
growth rate and the background-thresholded, normalised reporter/PM ratio."""
import glob, os, re
import numpy as np
import pandas as pd
from scipy import stats

HERE = os.path.dirname(os.path.abspath(__file__))
WINDOW_H = float(os.environ.get("WINDOW_H", 5.0))  # growth rate = slope of a linear fit to length over this centred window
SIGMA_H = 0.75      # Gaussian smoothing width (h) for the plotted lines
BG_THRESH = 110     # grey values; from the first frame where the SOK or PM ROI mean is below this, the ratio is dropped
SOK1C, SOK2C, SOK5C, H2BC, LENC = "#d62728", "#fdba12", "#318cc9", "#6cccde", "#bdbdbd"
# (folder, panel heading, short label for panel B, ratio colour)
GROUPS = [("gSOK1", "gSOK1-YFP", "gSOK1", SOK1C),
          ("pRPS5A-SOK1-YFP", "pRPS5A::SOK1-YFP", "pRPS5A::SOK1", SOK1C),
          ("pRPS5A-SOK2-YFP", "pRPS5A::SOK2-YFP", "pRPS5A::SOK2", SOK2C),
          ("pRPS5A-SOK5-YFP", "pRPS5A::SOK5-YFP", "pRPS5A::SOK5", SOK5C),
          ("pRPS5A-SOK5-YFP_ablation", "pRPS5A::SOK5-YFP, ablation", "pRPS5A::SOK5, ablation", SOK5C),
          ("pRPS5A-H2B-mTurq2", "pRPS5A::H2B-mTurq2", "pRPS5A::H2B", H2BC)]
CONTROL = "pRPS5A::H2B-mTurq2"
# single-channel stack (no PM); the sheet's PM column is the PM_*.csv of the SOK2 2021-10-20 root, so it is ignored
NO_PM = {"Analysis_MAX_SOK_dc2025-03-03_data-2021-05-26.ods"}
# per-file background threshold overrides (H2B signal of this root sits at 108-113 throughout)
THRESH = {"Analysis_MAX_2023-07-13_173148_stMP2_stage2-stage4.ods": 100}


def smooth(t, y, sigma=SIGMA_H):
    """Gaussian-weighted average over time, ignoring missing values (gaps stay gaps)."""
    t, y = np.asarray(t, float), np.asarray(y, float)
    ok = ~np.isnan(y)
    out = np.full(len(y), np.nan)
    for i, ti in enumerate(t):
        if ok[i]:
            w = np.exp(-0.5 * ((t[ok] - ti) / sigma) ** 2)
            out[i] = np.sum(w * y[ok]) / np.sum(w)
    return out


def rolling_rate(t, L, window):
    rate = np.full(len(t), np.nan)
    for i, ti in enumerate(t):
        m = np.abs(t - ti) <= window / 2 + 1e-9
        if m.sum() >= 4:
            rate[i] = np.polyfit(t[m], L[m], 1)[0]
    return rate


def load(verbose=True):
    data = []
    for folder, label, short, color in GROUPS:
        files = sorted(glob.glob(os.path.join(HERE, folder, "An*sis_*.ods")) + glob.glob(os.path.join(HERE, folder, "An*sis_*.xlsx")))
        for f in files:
            base = os.path.basename(f)
            if base.startswith(("~", "._", ".~")):
                continue
            d = pd.read_excel(f, engine="odf" if f.endswith(".ods") else None)
            d = d.dropna(subset=["time (h)", "length (µm)"]).reset_index(drop=True)
            date = re.search(r"\d{4}-\d{2}-\d{2}", base.replace("dc2025-03-03_data-", "")).group(0)
            stack = re.search(r"stack\d", base)
            if stack:  # two ablation roots on the same day
                date += " " + stack.group(0)
            t = d["time (h)"].to_numpy(float)
            L = d["length (µm)"].to_numpy(float)
            sok = d["Intensity SOK"].to_numpy(float)
            pm = d["intensity PM"].to_numpy(float)
            has_pm = not np.all(pm == 1) and base not in NO_PM  # PM column of 1s = no PM channel
            thr = THRESH.get(base, BG_THRESH)
            low = np.flatnonzero((sok < thr) | (has_pm & (pm < thr)))
            cut = low[0] if len(low) else len(d)
            ratio = sok / pm if has_pm else sok.copy()
            ratio[cut:] = np.nan
            norm = (ratio - np.nanmin(ratio)) / (np.nanmax(ratio) - np.nanmin(ratio))  # re-normalised over kept frames
            nlen = (L - L.min()) / (L.max() - L.min())  # recomputed (one gSOK1 sheet used the wrong max)
            g = rolling_rate(t, L, WINDOW_H)
            m = ~np.isnan(g) & ~np.isnan(norm)
            rho, p = stats.spearmanr(g[m], norm[m]) if m.sum() >= 4 else (np.nan, np.nan)  # ratio may be cut entirely
            data.append(dict(group=label, short=short, color=color, date=date, file=base, t=t, L=L, nlen=nlen,
                             norm=norm, ratio=ratio, sok=sok, pm=pm, rate=g, has_pm=has_pm, thr=thr, rho=rho, p=p, slope=np.polyfit(t, L, 1)[0]))
            if verbose:
                print(f"{label:28s} {date}  PM {'yes' if has_pm else 'no '}  n={m.sum():2d}  rho={rho:+.2f} (p={p:.3f})"
                      + (f"  ratio cut from t = {t[cut]} h" if cut < len(d) else ""))
    return data


# PM fades towards background (125 at the SOK peak, 110 by 20 h), so the peak ratio is inflated by the denominator
PEAK_EXCLUDE = {"Analysis_MAX_2020-10-18_18.29.46_and_2020-10-20_18.32.52_SOK1-YFP_x_sR356_recovered-upon-chambre-entry-and-growth-continued.xlsx"}


def peak_ratios(data):
    """Peak of the Gaussian-smoothed raw reporter/PM ratio per root (frames kept by the background threshold).
    Roots without a PM channel and PEAK_EXCLUDE are left out."""
    rows = []
    for s in data:
        if not s["has_pm"] or np.all(np.isnan(s["ratio"])) or s["file"] in PEAK_EXCLUDE:
            continue
        sm = smooth(s["t"], s["ratio"])
        rows.append(dict(construct=s["group"], root=s["date"], peak=np.nanmax(sm), min=np.nanmin(sm), color=s["color"]))
    return pd.DataFrame(rows)
