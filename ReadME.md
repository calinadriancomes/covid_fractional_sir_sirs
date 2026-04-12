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

Updated project root for this archive: C:/date/covid_fractional_sir_project_v5/


Diebold-Mariano outputs are now saved automatically to:
C:/date/covid_fractional_sir_project_v5/outputs/diebold_mariano/

Files generated on each run:
- diebold_mariano_<timestamp>.rds
- diebold_mariano_by_country_<timestamp>.csv
- diebold_mariano_summary_<timestamp>.csv
- diebold_mariano_errors_<timestamp>.csv
- latest snapshots with *_latest names for direct article use


Version v4 adds the Fractional Dominance Score (FDS) to the Diebold-Mariano analysis, displays it in the application, and saves article-ready FDS CSV outputs automatically.


Version v5 adds three comparative Diebold–Mariano visual components: a family-level stacked bar plot, a cross-country heatmap across model families, and a top table of significant country-family cases where the fractional model is better.


## v6 additions
- Interactive scrollable heatmap in the Diebold-Mariano tab
- Global map with animated GIF indicator and selected-country statistical cloud
- Country-level statistical cloud updates when a country is selected in the sidebar


Version v6.1 adds hover-enabled interactive Diebold–Mariano heatmaps, automatic PNG export of DM figures, and separate article-ready family maps.
