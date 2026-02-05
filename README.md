# Malawi Education Risk Diagnostic (MICS 2019–2020)

This repository contains a fully reproducible R-based workflow for a rapid
education risk diagnostic for Malawi, developed as part of a UNICEF recruitment
exercise.

## Structure
- `scripts/` – modular R scripts for data import, cleaning, analysis, and mapping
- `config/` – path configuration templates (no raw data included)
- `metadata/` – diagnostics, merge summaries, and design validation outputs
- `outputs/` – tables and maps used in the written diagnostic
- `docs/` – written deliverables

## Reproducibility
Raw microdata (MICS) are not included. To reproduce the analysis:
1. Place raw MICS data locally (not tracked)
2. Create `config/paths.yml` based on `paths_example.yml`
3. Run scripts sequentially from `00_setup.R`

All estimates account for survey weights, clustering, and stratification.
