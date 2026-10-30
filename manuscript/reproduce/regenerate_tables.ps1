# Rebuilds manuscript/tables/*.tex, tables/numbers.tex and the Supplementary Table S1 files
# from the latest Diebold-Mariano output of the pipeline. Run from C:\covid.
$ErrorActionPreference = "Stop"
Set-Location (Join-Path $PSScriptRoot "..\..")
python -I manuscript/reproduce/make_tables.py `
  --by-country outputs/diebold_mariano/diebold_mariano_by_country_latest.csv `
  --out-tables manuscript/tables `
  --arfima-details outputs/diebold_mariano/arima_arfima_fit_details_latest.csv `
  --out-supp manuscript/supplementary | Out-Null
Write-Host "Tables regenerated in manuscript/tables and manuscript/supplementary"
