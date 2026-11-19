# Fig. S10 — SOK5 accumulation and growth, intact vs ablated primordium (raw units)

Intact vs ablated pRPS5A::SOK5-YFP primordium in raw units on imaging time: length, growth rate, SOK5-YFP and PM intensities, SOK5/PM ratio.

## Files
- `plot_SOK5_vs_ablation.py`: makes the figure(s) below; run from this folder.
- `sok_data.py`: shared data loading (background threshold, ratio, growth rate, normalisation).
- Data (`Analysis_*` sheets, in their construct folders), as read by the script:
  - `pRPS5A-SOK5-YFP/Analysis_MAX_2023-02-24_18.19.24_sC194-1-1-3_stage2-emergence-track-1_crop.ods`
  - `pRPS5A-SOK5-YFP_ablation/Analysis_MAX_2023-11-05_stack0_cropped_forDataAnalysis_betterAdjusted.ods`
- Outputs: `Figure_SOK5_vs_ablation_detail.png`, `Figure_SOK5_vs_ablation_detail.pdf`, `Figure_SOK5_vs_ablation_detail.svg`

## Run
    python3 plot_SOK5_vs_ablation.py

Requires Python 3 with numpy, pandas, scipy, matplotlib, odfpy (.ods) and openpyxl (.xlsx); made with Python 3.9.6,
numpy 2.0.2, pandas 2.3.3, scipy 1.13.1, matplotlib 3.9.4, odfpy, openpyxl 3.1.5. Figures use Helvetica.

Settings shared by all figures (in `sok_data.py`): imaging interval 0.5 h; growth rate = slope of a linear fit to
length over a 5 h centred window (WINDOW_H); ratio dropped from the first frame where the reporter or PM ROI mean is
below 110 grey values (BG_THRESH; 100 for one H2B movie, THRESH); primordia without a PM channel use reporter
intensity alone (PM column of 1s, or NO_PM).
