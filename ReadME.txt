# COVID Fractional SIR Project

This project implements a modular Shiny dashboard for the comparative analysis of classical epidemiological modelling and machine learning methods applied to Johns Hopkins COVID-19 data.

## Current version
- Data overview
- Classical SIR forecasting
- Random Forest prediction
- XGBoost prediction
- Model comparison

## Planned extensions
- Fractional SIR
- Fractional feature engineering
- Fractional-enhanced Random Forest
- Fractional-enhanced XGBoost
- Explainability and downloads

Project root: resolved automatically by global.R (the folder that contains global.R, or the environment variable COVID_PROJECT_DIR). Run the app with shiny::runApp("<project folder>") and the scripts from the scripts/ folder.


Diebold-Mariano outputs are now saved automatically to:
<project root>/outputs/diebold_mariano/

Files generated on each run:
- diebold_mariano_<timestamp>.rds
- diebold_mariano_by_country_<timestamp>.csv
- diebold_mariano_summary_<timestamp>.csv
- diebold_mariano_errors_<timestamp>.csv
- latest snapshots with *_latest names for direct article use


Version v4 adds the Fractional Dominance Score (FDS) to the Diebold-Mariano analysis, displays it in the application, and saves article-ready FDS CSV outputs automatically.


Version v6.1 adds hover-enabled interactive Diebold–Mariano heatmaps, automatic PNG export of DM figures, and separate article-ready family maps.


## v6.2 additions (30 October 2026)
- Fixed the JHU date parser in R/io_import.R: files with four-digit years ("1/22/2020") were read with "%m/%d/%y", which collapsed 2020-2023 into one calendar year and summed cumulative counts across years. The parser now handles both formats and stops if date columns collapse.
- Fixed the JHU-to-map country name recoding used by the Diebold-Mariano maps (US, United Kingdom, Korea, Taiwan*, Burma, both Congos, Bosnia and Herzegovina, Dominican Republic, North Macedonia, West Bank and Gaza, Holy See, multi-island states).
- Removed hard-coded project paths from global.R and scripts/05_run_diebold_mariano.R (see "Project root" above); R/sir_classic.r renamed to R/sir_classic.R to match the file list in global.R.
