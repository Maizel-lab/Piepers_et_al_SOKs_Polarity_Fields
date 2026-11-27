# Piepers et al. — SOSEKI polarity fields

Code and source data for the manuscript

> **SOSEKI proteins report polarity shaped by organ-level growth and structural cues during plant organogenesis**
> Marcel Piepers, Blanca Jazmin Reyes-Hernández, João R. D. Ramos, Neva Bölke, Laura Schütz, Kathrin Distel, Changzheng Song, Anamarija Primc, Lotte Bald, Roeland M. H. Merks, Dolf Weijers and Alexis Maizel

Preprint: https://www.biorxiv.org/content/10.1101/2025.11.26.690761v2

Contributed by the [Maizel lab](https://www.cos.uni-heidelberg.de/en/research-groups/cell-and-developmental-biology), Centre for Organismal Studies, Heidelberg University.

## How to cite

Please cite the permanent Zenodo record:

Piepers M and Maizel A. (2026). Code documentation for "SOSEKI proteins report polarity shaped by organ-level growth and structural cues during plant organogenesis". Zenodo. https://doi.org/10.5281/zenodo.17598920

---

## Repository layout

The folders follow the figures of the manuscript. Each analysis folder contains its scripts and the processed data they read. Raw microscopy images are not included; they are available from the corresponding author on request.

```
Figure_2/
  2C_SOK-maxima-density/                          SOK signal maxima relative to organ shape (density maps)
  2D_growth-anisotropy_MorphoGraphX/              MorphoGraphX meshes, PDGs and growth anisotropy per time window
  2EF_S8_SOK-polarity-vs-growth-and-PE-interface/ SOK vs wall growth, PE interface, PDG and anisotropy (R)
Figure_3/
  3BD_SOK5-ratio-vs-growth/                       SOK5/PM ratio and growth rate, intact vs ablated primordium
  3EF_S9_strain-simulations/                      Predicted strain and SOK5 signal per wall (Quarto/R)
  3H_SOK5-intensity-PHS1dP/                       SOK5 intensity after endodermal microtubule depolymerisation
Figure_4/
  4D_4I_S12C_acute-polarity-angle/                Acute polarity angles: scr, plt3/5/7 and PHS1ΔP vs controls
  4H_SOK2-plt357-growth-arrest/                   SOK2 edge signal and primordium length in plt3/5/7
Figure_S7/                                        SOK polarity and microtubule orientation, nematic order
Figure_S10/                                       SOK5 and growth, intact vs ablated (raw units)
Figure_S11/                                       SOK reporter levels vs primordium length, all time-lapses
Figure_S12/S12B_MAP4-signal/                      mVenus:MAP4 intensity, mock vs DEX
Figure_S14/                                       Microtubule anisotropy, Col-0 vs plt3/5/7
stats_common/                                     Shared statistics: root assignment and mixed models
```

## Figure panels and the corresponding analyses

| Panel | Folder | Script(s) | Input data | Statistics reported |
|---|---|---|---|---|
| Fig. 2C | `Figure_2/2C_SOK-maxima-density` | `Determine-SOK-maxima.py` (finds the three brightest maxima and LR width in time-lapse stacks), `Plot-SOK-Maxima.py` (density maps per stage); `2026-05-28_plot-clouds.ipynb` is a notebook version of the plotting script | `SOK*_<date>.csv` (time-lapses), `SOK*_manual_measurements.csv` (static images, Fiji *Find Maxima*), `averageWidthPerStage.xlsx` | — (descriptive) |
| Fig. 2D | `Figure_2/2D_growth-anisotropy_MorphoGraphX` | `MorphoGraphX.py` in each window folder (MorphoGraphX 2.0 session log of the processing steps) | MorphoGraphX meshes (`.mgxm`), views (`.mgxv`) and exported PDG tables, one folder per pair of time points (`T0T4`, `T4T8`, …) | — (descriptive) |
| Fig. 2E | `Figure_2/2EF_S8_…` | `fig4_wall_growth_vs_signal.R` | `consolidated_wall_signal.csv` | Spearman correlation (walls, single primordium) |
| Fig. 2F, S8B | `Figure_2/2EF_S8_…` | `fig1_fig2_polar_separation.R` | `consolidated_angles.csv` | Permutation tests, 10,000 shuffles (cells, single primordium) |
| Fig. S8A | `Figure_2/2EF_S8_…` | `fig3_anisotropy_vs_polarity.R` | `pdg_vs_signal_sok2.csv`, `pdg_vs_signal_sok5.csv` | Orientation: permutation test on circular–linear R²; abundance: Pearson (cells, single primordium) |
| Fig. 3B, 3D | `Figure_3/3BD_SOK5-ratio-vs-growth` | `plot_SOK5_ratio_growth.py`, `sok_data.py` | `Analysis_*.ods` per construct (see folder README) | — (descriptive, one primordium each) |
| Fig. 3E, 3F, S9B | `Figure_3/3EF_S9_strain-simulations` | `Analysis_v2.qmd` | `strain_st5.csv` (control, S9B), `merged.csv` (control vs ablated, 3F), `st5_AB_data_set_corrected.csv` | — (descriptive; one template per condition, wall-level p-values off by default) |
| Fig. 3H | `Figure_3/3H_SOK5-intensity-PHS1dP` | `plot_3H_SOK5_intensity.py` | `SOK5 PHS1∆P measurements.xlsx` | Mann–Whitney U on primordia and on root means |
| Fig. 4D, 4I, S12C | `Figure_4/4D_4I_S12C_acute-polarity-angle` | `make_acute_angle_boxplots.py` | MorphoGraphX polarity exports in `wtst4/` + `scr/` (4D), `wtst2 SOK2/` + `plt SOK2/` (4I), `phs1dp mock/` + `phs1dp dex/` (S12C) | Linear mixed model, root as random effect, Holm across tissues; root-level permutation as check |
| Fig. 4H | `Figure_4/4H_SOK2-plt357-growth-arrest` | `SOK2 intensity and LRP length measurements plot.R` | `SOK2 intensity and LRP length measurements.xlsx` | — (descriptive) |
| Fig. S7B, C | `Figure_S7` | `SOK1 2 5 & MT axes plots - nematic order.ipynb` | `SOK*/<stage>/polarisation*.csv`, `MT/` | — (descriptive) |
| Fig. S7D | `Figure_S7` | `SOK1 2 5 polarity axes boxplots.ipynb` (SOK), `MT orientation axes boxplots.ipynb` (microtubules) | `SOK*/<stage>/polarisation*.csv`, `MT/` | Letters: linear mixed model, root as random effect, pairwise Holm |
| Fig. S7F | `Figure_S7` | `Supracellular-microtubule-orientation.ipynb` | `MT/data_redone_after_reordering_and_verification_layers.xlsx` | — (descriptive) |
| Fig. S10 | `Figure_S10` | `plot_SOK5_vs_ablation.py`, `sok_data.py` | `Analysis_*.ods` | — (descriptive) |
| Fig. S11E, F | `Figure_S11` | `plot_all_SOK.py`, `sok_data.py` | `Analysis_*.ods/.xlsx` per construct | — (descriptive) |
| Fig. S12B | `Figure_S12/S12B_MAP4-signal` | `plot_S12B_MAP4_intensity.py` | `measurements_v2_mVenus-MAP4.xlsx` | Mann–Whitney U on primordia (one per root) |
| Fig. S14C | `Figure_S14` | `plot_S14C_MT_anisotropy.py` | `anisotropy-wt-vs-plt357_st2-central.xlsx` | Linear mixed model on log anisotropy, root as random effect; root-level permutation as check |

In `Figure_S7`, the data are organised as `SOK1|SOK2|SOK5 / young LRP | young emerged | mature roots`. Files ending in `_pos` or `_mirrored` are orientation-corrected versions of the MorphoGraphX exports. The notebook in `SOK5/young emerged/` computes the intensity-weighted acute angles for the SOK5 time-lapse.

The strain simulations (Fig. 3E, S9A) were run with the vertex model at https://github.com/Joao-R/LR_strain_prediction. This repository holds only the downstream analysis.

## Running the analyses

Run each script **from its own folder**: all paths are relative to the folder that contains the script.

**Python** (made with Python 3.9.6; numpy 2.0.2, pandas 2.3.3, scipy 1.13.1, matplotlib 3.9.4, odfpy, openpyxl 3.1.5; statsmodels 0.14 for the mixed models; the Fig. S7 notebooks also need Jupyter):

```bash
cd "Figure_4/4D_4I_S12C_acute-polarity-angle" && python3 make_acute_angle_boxplots.py   # writes boxplots/
cd "Figure_3/3BD_SOK5-ratio-vs-growth"      && python3 plot_SOK5_ratio_growth.py
cd "Figure_S10"                             && python3 plot_SOK5_vs_ablation.py
cd "Figure_S11"                             && python3 plot_all_SOK.py
cd "Figure_2/2C_SOK-maxima-density"         && python3 Plot-SOK-Maxima.py
cd "Figure_S14"                             && python3 plot_S14C_MT_anisotropy.py          # writes outputs/
cd "Figure_3/3H_SOK5-intensity-PHS1dP"      && python3 plot_3H_SOK5_intensity.py
cd "Figure_S12/S12B_MAP4-signal"            && python3 plot_S12B_MAP4_intensity.py
```

The Fig. S7 notebooks are run from `Figure_S7/` (`jupyter nbconvert --execute` or cell by cell); they write their figures and statistics tables to `Figure_S7/figures/`.

**R** (≥ 4.1). The circular-statistics scripts need `ggplot2` (Fig. S8A uses `gridExtra` for its four-panel layout if it is installed). `Analysis_v2.qmd` needs Quarto and `tidyverse`, `janitor`, `paletteer`, `ggpubr`, `scales`, `kableExtra`, `emmeans`, `multcomp`, `multcompView`, `ggpmisc` and `conflicted`. The Fig. 4H script needs `readxl`.

```bash
cd "Figure_2/2EF_S8_SOK-polarity-vs-growth-and-PE-interface"
Rscript fig1_fig2_polar_separation.R consolidated_angles.csv
Rscript fig4_wall_growth_vs_signal.R consolidated_wall_signal.csv SOK2
Rscript fig4_wall_growth_vs_signal.R consolidated_wall_signal.csv SOK5
Rscript fig3_anisotropy_vs_polarity.R pdg_vs_signal_sok2.csv SOK2 FALSE
Rscript fig3_anisotropy_vs_polarity.R pdg_vs_signal_sok5.csv SOK5 TRUE

cd "Figure_3/3EF_S9_strain-simulations" && quarto render Analysis_v2.qmd
```

## Statistics in brief

- **Polarity axes** (SOK, PDG, PE interface) are axial (θ ≡ θ + 180°). They are analysed with the doubled-angle transform. Significance comes from permutation tests (10,000 shuffles) that preserve each variable's marginal distribution (`common_axial_stats.R`).
- **Biological replicates are roots.** Cells imaged in the same root are not independent, so cell-level measurements compared between genotypes, treatments, tissues or stages (Fig. 4D, 4I, S7D, S12C, S14C) are analysed with linear mixed models with a random intercept per root (`stats_common/mixed.py`, statsmodels MixedLM, REML). Roots are assigned from the image file names (`stats_common/rootid.py`): the microscope file plus the sample position (`S<n>`, `plant<n>`, `Image<n>`), together with the condition, so that several primordia imaged in one root count once. Two-group comparisons also report an exact permutation test that shuffles condition labels between roots.
- **Acute polarity angles** (0–90° to the primordium midline): angle ~ condition × tissue + (1 | root); the condition effect is tested within each tissue and Holm-corrected for the two tissues. The earlier cell-level Mann–Whitney U tests are kept in the output tables as a sensitivity check only.
- **Single-primordium analyses** (Fig. 2E, 2F, S8): each comes from one tracked primordium per paralog, so the tests assess association within that primordium.
- **SOK/PM ratios and growth rates**: growth rate is the slope of a linear fit to length over a 5 h centred window. Ratios are dropped once the signal falls below background (110 grey values). Settings are in `sok_data.py`.

Full details are in the Supplemental Materials of the manuscript.

## Notes

- `make_acute_angle_boxplots.py` also defines SOK1 and SOK5 plt3/5/7 comparisons. They are skipped automatically because their data are not part of this repository.
