# Fig. S11E, F — SOK reporter levels and primordium length in individual time-lapses

All primordia (gSOK1-YFP, pRPS5A::SOK1/2/5-YFP, pRPS5A::SOK5-YFP ablation, pRPS5A::H2B-mTurq2): normalised reporter/PM ratio and length aligned at stage II (15 µm), emergence (50 µm) marked.
- Figure_all_SOK_growth_vs_ratio: traces (A) + per-primordium Spearman rho of growth rate vs ratio (B).
- Figure_all_SOK_growth_vs_ratio_traces: one-page supplementary layout, traces (A) + peak raw ratio per primordium (B).

## Files
- `plot_all_SOK.py`: makes the figure(s) below; run from this folder.
- `sok_data.py`: shared data loading (background threshold, ratio, growth rate, normalisation).
- Data (`Analysis_*` sheets, in their construct folders), as read by the script:
  - `gSOK1/Analysis_2020-12-22_13.20.17_SOK1-YFP_x_sR356_stage2-to-emerged_GOOD-ONE-track1-track2_SINGLESLICE.xlsx`
  - `gSOK1/Analysis_MAX_2020-10-13_10.20.44_SOK1-YFP_x_sR356_stage2-to-3_nice-crop.xlsx`
  - `gSOK1/Analysis_MAX_2020-10-18_18.29.46_and_2020-10-20_18.32.52_SOK1-YFP_x_sR356_recovered-upon-chambre-entry-and-growth-continued.xlsx`
  - `gSOK1/Analysis_MAX_2020-10-30_18.15.49_SOK1-YFP_x_sR356_development-until-stage-3_fungi-growth-crop-1.xlsx`
  - `gSOK1/Analysis_MAX_2021-12-22_21.24.43_sC149-1-12-1_stage2-youngEmerged_development-without-SOK1-YFP-signal-track-crop.xlsx`
  - `gSOK1/Anaylsis_MAX_2020-10-14_18.03.04_SOK1-YFP_x_sR356_stage2-to-4_nice-track-crop.xlsx`
  - `pRPS5A-SOK1-YFP/Analysis_MAX_2021-05-03_14.21.36_pRPS5A_SOK1_YFP_side-view-track-crop.ods`
  - `pRPS5A-SOK1-YFP/Analysis_MAX_2024-01-17_MAX43-56_raw-track.ods`
  - `pRPS5A-SOK2-YFP/Analysis_MAX_2021-07-07_16.29.16-track-1_dc_crop.ods`
  - `pRPS5A-SOK2-YFP/Analysis_MAX_2021-07-27_15.03.38_pRPS5A-SOK2-YFP_x_pUBQ10-LTi6B-mCherry(x2)_stack_dc-drop_track2_track3_SINGLESLICE_dc-track1_cropped-track-track-crop.ods`
  - `pRPS5A-SOK2-YFP/Analysis_MAX_2021-07-28_dc_cropped_v2.ods`
  - `pRPS5A-SOK2-YFP/Analysis_MAX_2021-10-20_16.36.29_sC193-1_st2-st4-1.ods`
  - `pRPS5A-SOK5-YFP/Analysis_2022-10-24_19.02.08_sC194-1-1_stage4-emergence-track-crop.ods`
  - `pRPS5A-SOK5-YFP/Analysis_2022-11-04_18.23.51_sC194-1-1_stage2-to-emergence-track-crop.ods`
  - `pRPS5A-SOK5-YFP/Analysis_2023-10-02_MAX53-55_dc.ods`
  - `pRPS5A-SOK5-YFP/Analysis_MAX_2021-05-21_11.36.12_pRPS5A_SOK5_YFP_reasonable_v2.ods`
  - `pRPS5A-SOK5-YFP/Analysis_MAX_2023-02-24_18.19.24_sC194-1-1-3_stage2-emergence-track-1_crop.ods`
  - `pRPS5A-SOK5-YFP/Analysis_MAX_SOK_dc2025-03-03_data-2021-05-26.ods`
  - `pRPS5A-SOK5-YFP_ablation/Analysis_2023-11-05_stack1_MAX61-74_raw-track.ods`
  - `pRPS5A-SOK5-YFP_ablation/Analysis_MAX_2023-11-05_stack0_cropped_forDataAnalysis_betterAdjusted.ods`
  - `pRPS5A-H2B-mTurq2/Analysis_2024-08-31_stMP2_forRatioCalculations_MAX_volumes-track.ods`
  - `pRPS5A-H2B-mTurq2/Analysis_MAX_2023-07-13_173148_stMP2_stage2-stage4.ods`
- Outputs: `Figure_all_SOK_growth_vs_ratio.png`, `Figure_all_SOK_growth_vs_ratio.pdf`, `Figure_all_SOK_growth_vs_ratio.svg`, `Figure_all_SOK_growth_vs_ratio_traces.png`, `Figure_all_SOK_growth_vs_ratio_traces.pdf`, `Figure_all_SOK_growth_vs_ratio_traces.svg`

## Run
    python3 plot_all_SOK.py

Requires Python 3 with numpy, pandas, scipy, matplotlib, odfpy (.ods) and openpyxl (.xlsx); made with Python 3.9.6,
numpy 2.0.2, pandas 2.3.3, scipy 1.13.1, matplotlib 3.9.4, odfpy, openpyxl 3.1.5. Figures use Helvetica.

Settings shared by all figures (in `sok_data.py`): imaging interval 0.5 h; growth rate = slope of a linear fit to
length over a 5 h centred window (WINDOW_H); ratio dropped from the first frame where the reporter or PM ROI mean is
below 110 grey values (BG_THRESH; 100 for one H2B movie, THRESH); primordia without a PM channel use reporter
intensity alone (PM column of 1s, or NO_PM).
