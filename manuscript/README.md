# MDPI *Mathematics* manuscript

LaTeX version of `articol/draft.docx`, restructured to the MDPI *Mathematics* template
(`Definitions/mdpi.cls` v6.5d, class options `mathematics,article,submit,oneauthor`).

## Layout

| Path | Content |
|---|---|
| `manuscript.tex` | main file (front matter, Sections 1-3, back matter) |
| `sections/results.tex`, `discussion.tex`, `conclusions.tex` | Sections 4-6 |
| `tables/numbers.tex` | **generated**: all numbers quoted in the text, as macros (`\DMAsig`, ...) |
| `tables/table_dm_*.tex` | **generated**: Tables 3-5 (global summary, diagnostics, illustrative units) |
| `figures/fig1_dm_outcomes.png`, `fig2_map_arima_arfima.png`, `fig3_map_rf.png`, `fig4_map_xgb.png` | **generated** by the pipeline run (white background, colour-blind-safe palette) |
| `references.bib` | bibliography; numeric MDPI style via `Definitions/mdpi.bst` |
| `supplementary/Table_S1_*.{xlsx,csv}` | **generated**: complete unit-level Diebold-Mariano output (Panels A-C) |
| `reproduce/run_pipeline_for_manuscript.R` | re-runs scripts 01-04 and the DM benchmark (about 11 min); writes `C:/covid/outputs/diebold_mariano/`, then calls the two scripts below |
| `reproduce/make_figures.R` | Figures 1-4 from `outputs/diebold_mariano/diebold_mariano_latest.rds` (JHU names mapped to map polygons, multi-island units expanded) |
| `reproduce/arima_arfima_fit_details.R` | per-unit ARIMA orders, ARFIMA memory parameter and fallback flags (`outputs/diebold_mariano/arima_arfima_fit_details_latest.csv`) |
| `reproduce/make_tables.py`, `regenerate_tables.ps1` | builds the generated tables / macros / supplementary files from the DM output |
| `reproduce/run_log.txt` | log of the run used for the submitted numbers |
| `build.ps1` | pdflatex -> bibtex -> pdflatex x2 |

Build: `powershell -File build.ps1` (or `latexmk -pdf manuscript.tex`). Rebuild numbers after a new
pipeline run: `powershell -File reproduce\regenerate_tables.ps1`, then build again.

## What changed with respect to the Word draft

1. **Data-import bug fixed and results regenerated.** The raw JHU files in `data_raw/` carry
   four-digit years (`1/22/2020`), but `R/io_import.R` parsed them with `%m/%d/%y`; R silently kept
   only the first two year digits, so every date from 2020-2023 collapsed into calendar year 2020
   and cumulative counts were summed across years (e.g. the "2020-12-31" value for the US equalled the
   sum of the true 31 Dec 2020, 2021 and 2022 cumulative counts). All numbers in the draft
   (181/53/59 significant tests etc.) were computed on these corrupted 366-day series. The parser
   now handles both year formats and aborts if date columns collapse; the pipeline was re-run on the
   full 22 Jan 2020 - 9 Mar 2023 window and all tables, figures and text numbers come from that run.
   The old processed data are kept in `C:/covid/data_processed_backup_misparsed_dates_20261030/`.
2. **Structure** follows MDPI: Introduction; Mathematical Background; Materials and Methods; Results;
   Discussion; Conclusions; MDPI back matter (Supplementary Materials, Author Contributions, Funding,
   IRB, Informed Consent, Data Availability, Conflicts of Interest, Abbreviations). The WIREs-specific
   items (category line, graphical abstract, "Notes") were removed; the graphical abstract can be
   uploaded separately if the journal asks for one.
3. **Abstract** condensed to one paragraph under 200 words; keywords and MSC codes added.
4. **Mathematics corrected**: standard definitions of the Riemann-Liouville integral/derivative and of
   the Atangana-Baleanu derivative (normalisation B(alpha)); the Gruenwald-Letnikov fractional
   difference actually used by the pipeline is now written out (Eq. 6-8); the bias-variance statement
   is presented as a Remark (heuristic), not as a result; the DM sign convention is stated once and
   consistently (positive DM = fractional model has lower loss; the draft's Section 2 said the
   opposite of its Section 4).
5. **Methods now match the code**: study window; 20 standard + 12 fractional features with alpha in
   {0.2,0.4,0.6,0.8}, K = 30 / 14 lags; RF 500 trees; XGBoost 300 rounds, depth 6, eta 0.05, subsample
   0.8; 80/20 chronological split; ARIMA/ARFIMA evaluated as multi-step forecasts from a fixed origin
   (target = daily new cases, DM horizon h = 1) while RF/XGBoost use the direct t+7 target (h = 7);
   acf long-run variance with Bartlett fallback; undefined tests reported, FDS defined; population
   fallback 10^7 and the discontinued JHU "recovered" series disclosed as limitations.
6. **Tables**: draft Tables 1 and 3 (operators) merged into Table 1; Table 2 kept; the numerical
   Tables 3-5 are generated from the DM output. The illustrative units in Table 5 are chosen by a fixed
   rule (two most significant standard wins and two most significant fractional wins per family,
   excluding near-zero series) instead of by hand.
7. **References** converted to numbered MDPI style with DOIs; duplicated Riesz/Liouville entries
   merged; software and data citations added (R, forecast, ranger, JHU CSSE, Granger-Joyeux,
   Hosking, Harvey-Leybourne-Newbold, Podlubny).

## Items the author must confirm before submission

- `\funding{}` statement (currently "no external funding").
- Whether to add an Acknowledgments paragraph and an AI-use declaration (MDPI policy).
- The GitHub repository URL in the Data Availability Statement must contain the fixed `io_import.R`
  and the regenerated outputs.
- Special Issue: the call (`articol/call.txt`, Guest Editor Dr. Lubna Pinky) is selected in the
  submission system, not in the LaTeX source; `\externaleditor{}` is left commented out.
