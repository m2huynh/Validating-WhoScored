# WhoScored Rating Validation

**Author:** Minh Huynh

This repository contains a reproducible **R project** for validating **WhoScored player ratings** and evaluating whether a **2 SD threshold** identifies genuinely exceptional football performances.

The project packages the analysis into one place so it can be version-controlled and shared on GitHub. It includes the supplied raw datasets, R scripts, generated tables and figures, and a reviewer-facing report.

## Project structure

| Path | Purpose |
|---|---|
| `WhoScoredRatingValidation.Rproj` | RStudio project file |
| `R/` | Reproducible R scripts for the full workflow |
| `data/raw/` | Supplied Study 1 and Study 3 datasets |
| `data/processed/` | Optional location for derived datasets |
| `outputs/tables/` | Analysis tables and summary files |
| `outputs/figures/` | Analysis plots |
| `docs/` | Markdown report and reviewer-facing materials |
| `run_all.R` | One-command entry point for the analysis |

## Core analytical questions

The project addresses two linked questions. First, does the **WhoScored rating** track transparent match-performance variables in the Study 1 league data? Second, does the **2 SD exceptional-performance rule** isolate a far-right tail of genuinely outstanding performances in the pooled Study 3 tournament data?

## How to use the project

Open `WhoScoredRatingValidation.Rproj` in RStudio, then run `source("run_all.R")`. The scripts will read from `data/raw/` and write outputs to `outputs/tables/`, `outputs/figures/`, and `docs/`.
